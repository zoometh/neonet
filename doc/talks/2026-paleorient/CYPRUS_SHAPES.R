library(stringr)
library(openxlsx)
library(ggplot2)
library(ggrepel)
library(sf)
library(dplyr)
library(reshape2)
library(NbClust)
library(RColorBrewer)
library(gridExtra)
library(tidyr)
library(ggspatial)
library(ggplot2)
library(Momocs)

#########################
# 1 🔹 LOAD DATA AND OUTLINES
#########################

set.seed(123)

sampling <- FALSE
elbow.sickles <- TRUE

fig.full.h <- 15
fig.full.w <- 17
fig.half.h <- 9
fig.half.w <- 12

library(openxlsx)

script_file <- function() {
  command_args <- commandArgs(trailingOnly = FALSE)
  file_argument <- grep("^--file=", command_args, value = TRUE)
  
  if (length(file_argument) == 1L) {
    return(normalizePath(
      sub("^--file=", "", file_argument),
      mustWork = TRUE
    ))
  }
  
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    source_path <- rstudioapi::getSourceEditorContext()$path
    
    if (nzchar(source_path)) {
      return(normalizePath(source_path, mustWork = TRUE))
    }
  }
  
  stop(
    "Cannot determine the script location. Open the saved script in RStudio or run it with Rscript."
  )
}

path.data <- dirname(script_file())
jpgs <- file.path(path.data, "img")
chrono_file <- file.path(path.data, "data.xlsx")
output_folder <- file.path(path.data, "out")

if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

lf <- sort(list.files(
  jpgs,
  pattern = "\\.jpe?g$",
  full.names = TRUE,
  ignore.case = TRUE
))

if (length(lf) == 0L) {
  stop("No JPEG outline files were found in: ", jpgs)
}

# Define output folder exists
output_folder <- file.path(path.data, "out")
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)  # Create folder if it does not exist
}

if (sampling) {
  set.seed(123)
  lf <- sample(lf, 50)
}

library(Momocs)
coo <- import_jpg(lf)


#########################
# 2 🔹 OUTLINE PROCESSING
#########################

sickles <- Out(coo) %>%
  coo_interpolate(n = 80) %>%
  coo_center()


#########################
# 3 🔹 CHECK VALID SHAPES
#########################

# Define a validity check function for a Momocs shape.
# We assume that a valid shape should be a numeric matrix with exactly 2 columns (x and y)
# and with at least 5 rows (points), and without any NA values.
is_valid_shape <- function(x) {
  if (is.null(x)) return(FALSE)
  if (!is.matrix(x)) x <- as.matrix(x)
  dims <- dim(x)
  if (length(dims) != 2) return(FALSE)
  if (dims[2] != 2) return(FALSE)
  if (dims[1] < 5) return(FALSE)
  if (any(is.na(x))) return(FALSE)
  return(TRUE)
}

# Initialize lists to store valid and invalid shape names.
valid_shapes_names <- c()
invalid_shapes_names <- c()

# Loop over all shapes in the sickles object.
for (n in names(sickles$coo)) {
  shape <- sickles$coo[[n]]
  if (is_valid_shape(shape)) {
    valid_shapes_names <- c(valid_shapes_names, n)
  } else {
    invalid_shapes_names <- c(invalid_shapes_names, n)
    message("Invalid shape found: ", n)
  }
}

# Print summary of valid and invalid shapes:
cat("Number of valid shapes:", length(valid_shapes_names), "\n")
cat("Number of invalid shapes:", length(invalid_shapes_names), "\n")


#########################
# 4 🔹 COMPUTE SHAPE AREAS
#########################

# Compute shape areas 
shape_sizes <- coo_area(sickles)

sickles.f <- efourier(sickles, norm = FALSE, nb.h = 20)

if (!is.null(sickles.f$coe)) {
  sickles.f.mat <- as.matrix(sickles.f$coe)
} else {
  stop("Error: Fourier transform did not return coefficients.")
}


###########################
# 5 🔹 ASSIGN COLORS BY SITE
###########################

# Extract site codes (first 3 characters from shape names)
sites <- substr(names(sickles), 1, 3)

# Create a data frame for site-to-shape mapping
df.obj <- data.frame(num = names(sickles), site = sites)

# Get unique site names
sites.uni <- unique(df.obj$site)

# Assign one color per site
n.sites <- length(sites.uni)
site.colors <- rainbow(n.sites, s = 1, v = 1, start = 0, end = max(1, n.sites - 1) / n.sites, alpha = 1)

# Map sites to colors
df.colors <- data.frame(site = sites.uni, cols = site.colors)

# Merge colors with shape data
df.obj.col <- merge(df.obj, df.colors, by = "site", all.x = TRUE)

# Ensure colors are assigned correctly
shape.colors <- df.obj.col$cols


###########################
# 6 🔹 PANEL PLOT
###########################

panel.out <- file.path(output_folder, "01_panel.jpg")

jpeg(
  panel.out,
  height = fig.full.h,
  width = fig.full.w,
  units = "cm",
  res = 600
)

panel(
  sickles,
  names = TRUE,
  cols = shape.colors,
  borders = shape.colors,
  cex.names = 0.2,
  main = "Shapes Panel Colored by Site",
  cex.main = 0.8,
  dim = c(10, 12)
)

dev.off()

###########################
# 7 🔹 STANDARDIZED STACKS
###########################

stack.out <- paste0(path.data, "/out/2_stack.jpg")
jpeg(stack.out, height = fig.half.h, width = fig.half.w, units = "cm", res = 600)

stacked <- sickles %>%
  coo_center() %>%
  coo_alignxax() %>%
  coo_slidedirection("up") 

stack(stacked,
      borders = shape.colors,  # Color by site
      title = "Standardized Stack Colored by Site (Size Ignored)"
)
dev.off()

###########################
# 8 🔹 PCA & Clustering
###########################

# 1) Perform PCA once on the Fourier coefficients
sickles.p       <- PCA(sickles.f)
pc1.2           <- sickles.p$x[, 1:2]
rownames(pc1.2) <- names(sickles)

# 2) Compute WSS for k = 2…10
set.seed(123)
wss_values <- numeric()
for (k in 2:10) {
  km.res           <- kmeans(pc1.2, centers = k, nstart = 25)
  wss_values[k - 1] <- km.res$tot.withinss
}

# 3) Plot elbow (you’ll see the “knee” at k = 4)
elbow_df <- data.frame(k = 2:10, wss = wss_values)
ggplot(elbow_df, aes(x = k, y = wss)) +
  geom_line() +
  geom_point() +
  ggtitle("Elbow Method for Optimal Clusters") +
  xlab("Number of Clusters") + 
  ylab("Total Within-Cluster SS")

# 4) Choose k = 4 (or compute it automatically)
optimal_clusters <- 4

# 5) Run k-means once with k = optimal_clusters
set.seed(123)
kmeans_result <- kmeans(pc1.2, centers = optimal_clusters, nstart = 50, iter.max = 100)

# 6) Build and save membership table
membership_table <- data.frame(
  Shape_Name = rownames(pc1.2),
  Cluster    = kmeans_result$cluster
)
write.xlsx(membership_table, paste0(path.data, "/out/membership_table.xlsx"))

# 7) (Optional) Check that no cluster has fewer than 2 outlines
while (any(table(kmeans_result$cluster) < 2)) {
  optimal_clusters <- optimal_clusters - 1
  if (optimal_clusters < 2) break
  kmeans_result <- kmeans(pc1.2, centers = optimal_clusters, nstart = 50, iter.max = 100)
}

###########################
# 9 🔹 Dendrogram & PCA Plot
###########################

# Prepare cluster‐based colors
library(RColorBrewer)
my.colors       <- brewer.pal(9, "Set1")
my.colors.select<- my.colors[1:optimal_clusters]
my.color.ramp   <- colorRampPalette(my.colors.select)
cluster.cols    <- my.color.ramp(optimal_clusters)[kmeans_result$cluster]

# 9A) Hierarchical clustering (dendrogram colored by k-means clusters)
jpeg(paste0(path.data, "/out/3_clust.jpg"), height = 15, width = 17, units = "cm", res = 400)
CLUST(
  sickles.f,
  hclust_method = "ward.D2",
  k             = optimal_clusters,
  palette       = my.color.ramp,
  cex           = 0.2
)
dev.off()

# 9B) PCA scatter with labels colored by cluster (shapes remain unchanged)
jpeg(paste0(path.data, "/out/4_pca.jpg"), height = 15, width = 17, units = "cm", res = 400)

# Draw the PCA plot with only the shape thumbnails (no text)
plot(
  sickles.p,
  labelspoints = FALSE,
  title        = "PCA of Shape Data"
)

# Overlay each outline’s name in its cluster color
coords <- pc1.2  # PC1, PC2 matrix
text(
  x      = coords[, 1],
  y      = coords[, 2],
  labels = rownames(coords),
  col    = cluster.cols,
  cex    = 0.3,
  pos    = 3
)

dev.off()
 

#######################################
# 10 🔹 Print a summary table
#######################################

# Ensure df_names is correctly defined
df_names <- data.frame(names = names(sickles))

# Assign clusters from K-means result
df_names <- data.frame(
  Shape_Name = rownames(pc1.2),
  Cluster = kmeans_result$cluster
)

# Debug: Print the first few rows to verify
print(head(df_names))

# Save to an Excel file
membership_table_file <- paste0(path.data, "/out/1_membership_table.xlsx")
write.xlsx(df_names, membership_table_file)


################################################
# 11 🔹 Print shapes for each cluster
################################################

# Match shape names to clusters
membership <- df_names[, c("Shape_Name", "Cluster")]
membership$Cluster <- as.factor(membership$Cluster)

library(RColorBrewer)
# Determine how many clusters you have:
n_clusters <- length(unique(membership$Cluster))
# Use at least 3 colors if n_clusters is less than 3:
n_clusters <- max(n_clusters, 3)
# Define your palette. For example:
combined_colors <- brewer.pal(n_clusters, "Set1")

print(head(df_names))

################################################
# 12. All clusters SEPARATED along X
################################################

clusters <- sort(unique(membership$Cluster))
n_clusters <- length(clusters)

# Choose a color palette
palette_colors <- brewer.pal(max(n_clusters, 3), "Set1")

# Decide a SHIFT large enough to separate clusters
# so that shapes won't overlap horizontally.
SHIFT <- 1500  # Increase or decrease as needed

combined_out_file <- file.path(output_folder, "5_overlay_clusters_x_separated.jpg")

# Open a single JPEG device
jpeg(combined_out_file, width = fig.full.w, height = fig.full.h, units = "cm", res = 600)

# 1. Set up an empty plot with a wide enough xlim
#    so all shifted shapes will be visible
plot(
  NA,
  xlim = c(-3000, 3000),  # Adjust to accommodate your SHIFT and shape width
  ylim = c(-600, 600),
  asp  = 1,               # Keep aspect ratio 1:1
  main = "All Clusters Separated on X",
  xlab = "X",
  ylab = "Y"
)

# 2. Loop over each cluster and add shapes at an offset
for (i in seq_along(clusters)) {
  cl <- clusters[i]
  
  # Get shape names for this cluster
  shape_names   <- membership$Shape_Name[membership$Cluster == cl]
  shape_indices <- which(names(sickles) %in% shape_names)
  cluster_shapes <- sickles[shape_indices]
  
  # Convert to Momocs Out object
  cluster_Out <- tryCatch(Out(cluster_shapes), error = function(e) NULL)
  if (!is.null(cluster_Out) && length(cluster_Out) > 0) {
    
    # Compute horizontal offset for this cluster
    # Example formula tries to center them around x=0 if you have multiple clusters
    offset_x <- SHIFT * (i - (n_clusters + 1)/2)
    
    # Plot each shape in this cluster with the offset applied to x
    for (j in seq_along(cluster_Out$coo)) {
      xy <- cluster_Out$coo[[j]]
      # Shift x-coordinates by offset_x
      lines(
        xy[, 1] + offset_x,
        xy[, 2],
        col = palette_colors[i]
      )
    }
  }
}

# Close the device
dev.off()
cat("Saved X-separated overlay image to:", combined_out_file, "\n")


#########################################################################################
# 13 🔹 Include tool's measures to clustering procedure
#########################################################################################

# Path to your Excel file containing the median chronology
chrono_file <- file.path(path.data, "data.xlsx")

# Read the Excel file
df_chrono <- read.xlsx(chrono_file)

# This line aligns IDs to shape metadata:
names(df_chrono)[names(df_chrono) == "ID"] <- "Shape_Name"

# ✅ Join clusters with measurement data
df_joined <- merge(
  df_names,
  df_chrono,  # This already contains MEDIAN, LENGTH, WIDTH, THICKNESS
  by = "Shape_Name",
  all.x = TRUE
)

# 🔹 Convert LENGTH, WIDTH, THICKNESS to numeric
df_joined <- df_joined %>%
  mutate(
    THICKNESS = as.numeric(gsub(",", ".", as.character(THICKNESS)))
  )

# Debug: Print the first few rows to verify
print(head(df_joined))


#########################################################################################
# 14🔹 INTEGRATE DIMENSIONS INTO CLASSIFICATION
#########################################################################################

# Scale measurement data
df_scaled <- df_joined %>%
  mutate(across(c(THICKNESS), scale))

# Combine PCA scores and scaled measurements
combined_data <- cbind(pc1.2[match(df_joined$Shape_Name, rownames(pc1.2)), ],
                       df_scaled[, c("THICKNESS")])

# Combine PCA scores and scaled measurements
# combined_data <- cbind(pc1.2[match(df_joined$Shape_Name, rownames(pc1.2)), ],
#                       df_scaled[, c("LENGTH", "WIDTH", "THICKNESS")])

# Re-run K-means clustering
set.seed(123)
kmeans_combined <- kmeans(combined_data, centers = 4, nstart = 50)

# Update cluster labels
df_joined$Combined_Cluster <- kmeans_combined$cluster

# Create a new membership table with updated cluster assignments
new_membership_table <- df_joined[, c("Shape_Name", "Combined_Cluster")]

# Print the first few rows of the new membership table for inspection
print(head(new_membership_table))

# Save the new membership table to an Excel file
new_membership_table_file <- paste0(path.data, "/out/new_membership_table.xlsx")
write.xlsx(new_membership_table, new_membership_table_file)
cat("New membership table saved to:", new_membership_table_file, "\n")


#########################################################################################
# 15🔹 Update cluster labels in your PCA dataframe
#########################################################################################

# Add combined cluster info to PCA coordinates
shape_pca_coords <- as.data.frame(pc1.2)
shape_pca_coords$Combined_Cluster <- df_joined$Combined_Cluster[match(rownames(shape_pca_coords), df_joined$Shape_Name)]


#########################################################################################
# 16🔹 Plot PCA with combined clusters
#########################################################################################

# Choose color palette
library(RColorBrewer)
n.clusters <- length(unique(shape_pca_coords$Combined_Cluster))
combined_colors <- brewer.pal(max(3, n.clusters), "Set1")[1:n.clusters]

# PCA plot
pca2 <- ggplot(shape_pca_coords, aes(x = PC1, y = PC2, color = factor(Combined_Cluster))) +
  geom_point(size = 2, alpha = 0.8) +
  stat_ellipse(level = 0.95, linetype = "dashed") +
  scale_color_manual(values = combined_colors) +
  labs(title = "PCA of Shapes + Measurements", color = "Combined Cluster") +
  theme_minimal()

# Save the bar chart to a JPEG file.
jpeg(file.path(output_folder, "06_pca_with_thickness.jpg"),
     width = 14, height = 10, units = "in", res = 300)
print(pca2)  # This line ensures the bar_chart is drawn on the device.
dev.off()

# Define a custom mshape function
mshape_custom <- function(shapes) {
  # Assumes that each element of shapes is a matrix (n x 2) with identical dimensions.
  arr <- array(unlist(shapes), dim = c(nrow(shapes[[1]]), ncol(shapes[[1]]), length(shapes)))
  mean_shape <- apply(arr, c(1,2), mean)
  return(mean_shape)
}


#########################################################################################
# 17🔹 Stacked average shapes by combined cluster (using custom mshape)
#########################################################################################

# LENGTH
ggplot(df_joined, aes(x = factor(Combined_Cluster), y = LENGTH)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Length by Combined Cluster", x = "Cluster", y = "Length (mm)") +
  theme_minimal()

# WIDTH
ggplot(df_joined, aes(x = factor(Combined_Cluster), y = WIDTH)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Width by Combined Cluster", x = "Cluster", y = "Width (mm)") +
  theme_minimal()

# THICKNESS
ggplot(df_joined, aes(x = factor(Combined_Cluster), y = THICKNESS)) +
  geom_boxplot(fill = "lightpink") +
  labs(title = "Thickness by Combined Cluster", x = "Cluster", y = "Thickness (mm)") +
  theme_minimal()


#########################################################################################
# 18🔹 SAVE UPDATED SHAPES FOR EACH COMBINED CLUSTER
#########################################################################################

# Match shape names to clusters
combined_membership <- df_joined[, c("Shape_Name", "Combined_Cluster")]
combined_membership$Combined_Cluster <- as.factor(combined_membership$Combined_Cluster)

# Loop through each Combined_Cluster and save shape panels
for (cluster in unique(combined_membership$Combined_Cluster)) {
  
  cluster_indices <- which(combined_membership$Combined_Cluster == cluster)
  shape_names <- combined_membership$Shape_Name[cluster_indices]
  
  # Match the shape names to the sickles object
  shape_indices <- which(names(sickles) %in% shape_names)
  cluster_shapes <- sickles[shape_indices]
  
  cat("\nProcessing Combined Cluster", cluster, "with", length(cluster_shapes), "shapes")
  
  if (!inherits(cluster_shapes, "Out")) {
    cluster_shapes <- tryCatch(Out(cluster_shapes), error = function(e) NULL)
  }
  
  if (!is.null(cluster_shapes) && length(cluster_shapes) > 0) {
    # Output file
    cluster_out_file <- file.path(output_folder, paste0("combined_cluster_", cluster, "_shapes.jpg"))
    
    jpeg(cluster_out_file, height = fig.full.h, width = fig.full.w, units = "cm", res = 600)
    
    panel(cluster_shapes,
          names = TRUE,
          cols = combined_colors[as.integer(cluster)],
          main = paste("Combined Cluster", cluster, "Shapes"))
    
    dev.off()
    
    cat("\nSaved:", cluster_out_file)
  } else {
    cat("\nSkipping Combined Cluster", cluster, "due to invalid shape data.")
  }
}


################################################
# 19🔹 All clusters SEPARATED along X
################################################

clusters <- sort(unique(combined_membership$Combined_Cluster))
n_clusters <- length(clusters)

# Choose a color palette
palette_colors <- brewer.pal(max(n_clusters, 3), "Set1")

# Decide a SHIFT large enough to separate clusters
# so that shapes won't overlap horizontally.
SHIFT <- 1500  # Increase or decrease as needed

combined_out_file <- file.path(output_folder, "7_overlay_combined_x_separated.jpg")

# Open a single JPEG device
jpeg(combined_out_file, width = fig.full.w, height = fig.full.h, units = "cm", res = 600)

# 1. Set up an empty plot with a wide enough xlim
#    so all shifted shapes will be visible
plot(
  NA,
  xlim = c(-3000, 3000),  # Adjust to accommodate your SHIFT and shape width
  ylim = c(-600, 600),
  asp  = 1,               # Keep aspect ratio 1:1
  main = "All Shapes by Cluster",
  xlab = "X",
  ylab = "Y"
)

# 2. Loop over each cluster and add shapes at an offset
for (i in seq_along(clusters)) {
  cl <- clusters[i]
  
  # Get shape names for this cluster
  shape_names   <- combined_membership$Shape_Name[combined_membership$Combined_Cluster == cl]
  shape_indices <- which(names(sickles) %in% shape_names)
  cluster_shapes <- sickles[shape_indices]
  
  # Convert to Momocs Out object
  cluster_Out <- tryCatch(Out(cluster_shapes), error = function(e) NULL)
  if (!is.null(cluster_Out) && length(cluster_Out) > 0) {
    
    # Compute horizontal offset for this cluster
    # Example formula tries to center them around x=0 if you have multiple clusters
    offset_x <- SHIFT * (i - (n_clusters + 1)/2)
    
    # Plot each shape in this cluster with the offset applied to x
    for (j in seq_along(cluster_Out$coo)) {
      xy <- cluster_Out$coo[[j]]
      # Shift x-coordinates by offset_x
      lines(
        xy[, 1] + offset_x,
        xy[, 2],
        col = palette_colors[i]
      )
    }
  }
}

# Close the device
dev.off()
cat("Saved X-separated overlay image to:", combined_out_file, "\n")


#########################################################################################
# 20🔹 Compare clustering procedures
#########################################################################################

# Create a contingency table to compare the two classifications
ct <- table(df_joined$Cluster, df_joined$Combined_Cluster)
print(ct)

# For each cluster in 'Cluster', identify the Combined_Cluster with the maximum overlap
mapping <- apply(ct, 1, function(x) names(x)[which.max(x)])
print(mapping)

# Function to map the Combined_Cluster to the new label based on the mapping
rename_cluster <- function(combined_value) {
  # Identify the corresponding original cluster for this Combined_Cluster value
  for (orig_cluster in names(mapping)) {
    if(as.numeric(combined_value) == as.numeric(mapping[orig_cluster])) {
      return(as.numeric(orig_cluster))
    }
  }
  # If no mapping found, you can return the original combined_value or NA
  return(NA)
}

# Create a new column with the remapped Combined_Cluster values
df_joined$Combined_Cluster_Renamed <- sapply(df_joined$Combined_Cluster, rename_cluster)


# Define the mapping from Combined_Cluster to the new cluster label
# (i.e., new_label = mapping[as.character(Combined_Cluster)])
mapping <- c("1" = 1,  # Combined Cluster 1 → Cluster 3
             "2" = 2,  # Combined Cluster 2 → Cluster 4
             "3" = 4,  # Combined Cluster 3 → Cluster 1
             "4" = 3)  # Combined Cluster 4 → Cluster 2

# Apply the mapping to create a new column
df_joined$Combined_Cluster_Renamed <- mapping[as.character(df_joined$Combined_Cluster)]


#########################################################################################
# 21🔹 Compare clusters metrically
#########################################################################################

library(dplyr)

# Summary for the original clusters
cluster_summary <- df_joined %>%
  group_by(Cluster) %>%
  summarise(
    mean_length    = mean(LENGTH, na.rm = TRUE),
    mean_width     = mean(WIDTH, na.rm = TRUE),
    mean_thickness = mean(THICKNESS, na.rm = TRUE)
  )
print(cluster_summary)

# Summary for the Combined_Cluster
combined_cluster_summary <- df_joined %>%
  group_by(Combined_Cluster) %>%
  summarise(
    mean_length    = mean(LENGTH, na.rm = TRUE),
    mean_width     = mean(WIDTH, na.rm = TRUE),
    mean_thickness = mean(THICKNESS, na.rm = TRUE)
  )
print(combined_cluster_summary)

# Summary for the Combined_Cluster
combined_Cluster_Renamed_summary <- df_joined %>%
  group_by(Combined_Cluster_Renamed) %>%
  summarise(
    mean_length    = mean(LENGTH, na.rm = TRUE),
    mean_width     = mean(WIDTH, na.rm = TRUE),
    mean_thickness = mean(THICKNESS, na.rm = TRUE)
  )
print(combined_Cluster_Renamed_summary)


################################################################################
# 22🔹  “3‐in‐1” Boxplot of Length, Width, and Thickness by Renamed Cluster
################################################################################

library(tidyr)
library(ggplot2)

# 1) Reshape to long format: one row per (Shape_Name × measure × value),
#    using the RENAMED cluster field this time
library(dplyr)
df_measures_long <- df_joined %>%
  dplyr::select(Shape_Name, Combined_Cluster_Renamed, LENGTH, WIDTH, THICKNESS) %>%
  pivot_longer(
    cols      = c(LENGTH, WIDTH, THICKNESS),
    names_to  = "Measure",
    values_to = "Value"
  )

# 2) Make sure “Measure” is a factor in the desired order
df_measures_long$Measure <- factor(df_measures_long$Measure,
                                   levels = c("LENGTH", "WIDTH", "THICKNESS"))

# 3) Choose a color palette for the 4 RENAMED clusters
cluster_levels_renamed <- sort(unique(df_measures_long$Combined_Cluster_Renamed))
n.clusters_renamed   <- length(cluster_levels_renamed)
palette_4clusters <- brewer.pal(max(4, n.clusters_renamed), "Set1")[1:n.clusters_renamed]

# 4) Draw a single ggplot with facets by Measure, now keyed to RENAMED cluster
box3_in1 <- ggplot(df_measures_long,
                   aes(x = factor(Combined_Cluster_Renamed),
                       y = Value,
                       fill = factor(Combined_Cluster_Renamed))) +
  geom_boxplot(
    notch      = FALSE,
    notchwidth = 0.5,     # <- smaller notch
    outlier.size = 1,
    alpha        = 0.8
  ) +
  scale_fill_manual(values = palette_4clusters) +
  facet_wrap(~ Measure, nrow = 1, scales = "free_y") +
  labs(
    x = "Cluster (RENAMED)",
    y = "mm",
    fill = "Cluster"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text      = element_text(face = "bold"),
    axis.title.x    = element_blank(),
    axis.text.x     = element_text(size = 10),
    axis.title.y    = element_text(size = 11),
    legend.position = "none"
  )

# 5) Save to JPEG
jpeg(file.path(output_folder, "10_boxplots_L_W_T_by_cluster_renamed.jpg"),
     width  = 14,   # in inches
     height = 5,    # in inches
     units  = "in",
     res    = 300)
print(box3_in1)
dev.off()


#########################################################################################
# 23🔹 Plot Dominance of Clusters Over Time as a Stacked Bar Chart Using Renamed Clusters
#########################################################################################

library(scales)
# df_joined already merges df_names with df_chrono and has the new Combined_Cluster_Renamed.
# If needed, you can also convert MEDIAN to an ordered factor:
df_joined$MEDIAN <- factor(df_joined$MEDIAN,
                           levels = sort(unique(df_joined$MEDIAN)),
                           ordered = TRUE)

# Create the bar chart object for the combined clustering with identical style.
bar_chart2 <- ggplot(df_joined, aes(x = MEDIAN, fill = factor(Combined_Cluster_Renamed))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Proportion of Combined Clusters by MEDIAN Period",
       x = "MEDIAN Period",
       y = "Proportion of Shapes",
       fill = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display bar_chart2
print(bar_chart2)

# Optionally, save bar_chart2:
jpeg(file.path(output_folder, "11_cluster_proportions_by_phase.jpg"),
     width = 14, height = 8, units = "in", res = 300)
print(bar_chart2)
dev.off()


################################################
# 24🔹 All clusters SEPARATED along X
################################################
combined_membership <- combined_membership %>%
  left_join(df_joined[, c("Shape_Name", "Combined_Cluster_Renamed")], by = "Shape_Name")

clusters <- sort(unique(combined_membership$Combined_Cluster_Renamed))
n_clusters <- length(clusters)

# Choose a color palette
palette_colors <- brewer.pal(max(n_clusters, 3), "Set1")

# Decide a SHIFT large enough to separate clusters
# so that shapes won't overlap horizontally.
SHIFT <- 1500  # Increase or decrease as needed

combined_out_file <- file.path(output_folder, "8_overlay_combined_lusters_x_separated.jpg")

# Open a single JPEG device
jpeg(combined_out_file, width = fig.full.w, height = fig.full.h, units = "cm", res = 600)

# 1. Set up an empty plot with a wide enough xlim
#    so all shifted shapes will be visible
plot(
  NA,
  xlim = c(-3000, 3000),  # Adjust to accommodate your SHIFT and shape width
  ylim = c(-600, 600),
  asp  = 1,               # Keep aspect ratio 1:1
  main = "All Shapes by Cluster",
  xlab = "X",
  ylab = "Y"
)

# 2. Loop over each cluster and add shapes at an offset
for (i in seq_along(clusters)) {
  cl <- clusters[i]
  
  # Get shape names for this cluster
  shape_names   <- combined_membership$Shape_Name[combined_membership$Combined_Cluster_Renamed == cl]
  shape_indices <- which(names(sickles) %in% shape_names)
  cluster_shapes <- sickles[shape_indices]
  
  # Convert to Momocs Out object
  cluster_Out <- tryCatch(Out(cluster_shapes), error = function(e) NULL)
  if (!is.null(cluster_Out) && length(cluster_Out) > 0) {
    
    # Compute horizontal offset for this cluster
    # Example formula tries to center them around x=0 if you have multiple clusters
    offset_x <- SHIFT * (i - (n_clusters + 1)/2)
    
    # Plot each shape in this cluster with the offset applied to x
    for (j in seq_along(cluster_Out$coo)) {
      xy <- cluster_Out$coo[[j]]
      # Shift x-coordinates by offset_x
      lines(
        xy[, 1] + offset_x,
        xy[, 2],
        col = palette_colors[i]
      )
    }
  }
}

# Close the device
dev.off()
cat("Saved X-separated overlay image to:", combined_out_file, "\n")


##########################################################################################
# 25🔹 AVERAGE SHAPES FOR CLUSTERS
##########################################################################################

# Step 1: Compute mean shape for each Combined_Cluster_Renamed
mean_shapes_list <- list()

for (i in seq_along(clusters)) {
  cl <- clusters[i]
  
  shape_names <- combined_membership$Shape_Name[combined_membership$Combined_Cluster_Renamed == cl]
  shape_indices <- which(names(sickles) %in% shape_names)
  cluster_shapes <- Out(sickles$coo[shape_names])
  
  # Extract and clean coordinate matrices
  coos <- cluster_shapes$coo
  coos <- Filter(Negate(is.null), coos)
  
  # Keep only valid, same-sized shapes
  if (length(coos) > 0 && all(sapply(coos, is.matrix)) && all(sapply(coos, nrow) == nrow(coos[[1]]))) {
    arr <- array(unlist(coos), dim = c(nrow(coos[[1]]), 2, length(coos)))
    mean_shape <- apply(arr, c(1, 2), mean)
    mean_shapes_list[[as.character(cl)]] <- mean_shape
  } else {
    warning(paste("Skipping cluster", cl, "due to inconsistent or invalid shapes."))
  }
}

# Output file path
combined_out_file <- file.path(output_folder, "9_average_clusters_mean_shapes.jpg")

# Open JPEG device
jpeg(combined_out_file, width = fig.full.w, height = fig.full.h, units = "cm", res = 600)

# Setup plotting area
plot(
  NA,
  xlim = c(-n_clusters * SHIFT / 2, n_clusters * SHIFT / 2),
  ylim = c(-600, 600),
  asp  = 1,
  main = "Mean Shape by Cluster",
  xlab = "X",
  ylab = "Y"
)

# Plot each mean shape with horizontal offset
for (i in seq_along(clusters)) {
  cl <- clusters[i]
  mean_shape <- mean_shapes_list[[as.character(cl)]]
  
  if (!is.null(mean_shape)) {
    offset_x <- SHIFT * (i - (n_clusters + 1) / 2)
    lines(mean_shape[, 1] + offset_x, mean_shape[, 2], col = palette_colors[i], lwd = 2)
    text(offset_x, 580, labels = paste("Cluster", cl), col = palette_colors[i], cex = 1.2, font = 2)
  }
}

# Close JPEG device
dev.off()
cat("✅ Saved X-separated *mean shapes* overlay image to:", combined_out_file, "\n")


#################################################################################################################
# 26🔹 DESCRIPTIVE DISTRIBUTION OF INSERT LENGTH BY CHRONOLOGICAL PHASE
#################################################################################################################

library(dplyr)
library(ggplot2)

# Establish the chronological order of the eight phases.
phase_order <- df_joined %>%
  dplyr::distinct(PHASE, MEDIAN) %>%
  dplyr::arrange(MEDIAN)

# Prepare all inserts, including all four morphometric clusters.
df_length_plot <- df_joined %>%
  dplyr::mutate(
    PHASE = factor(PHASE, levels = phase_order$PHASE, ordered = TRUE),
    Cluster = factor(Combined_Cluster_Renamed)
  ) %>%
  dplyr::filter(!is.na(LENGTH), !is.na(PHASE), !is.na(Cluster))

# Number of inserts represented in each phase.
phase_n <- df_length_plot %>%
  dplyr::count(PHASE, name = "n") %>%
  dplyr::mutate(label = paste0("n = ", n))

# Descriptive plot: boxplots show the median and interquartile range;
# coloured points represent individual inserts and their cluster assignment.
figure_11 <- ggplot(df_length_plot, aes(x = PHASE, y = LENGTH)) +
  geom_boxplot(
    width = 0.65,
    fill = "grey95",
    colour = "grey35",
    outlier.shape = NA
  ) +
  geom_jitter(
    aes(colour = Cluster),
    width = 0.16,
    height = 0,
    size = 2.1,
    alpha = 0.78
  ) +
  geom_text(
    data = phase_n,
    aes(x = PHASE, y = Inf, label = label),
    inherit.aes = FALSE,
    vjust = 1.35,
    size = 3.3
  ) +
  scale_colour_brewer(
    palette = "Set1",
    name = "Morphometric cluster"
  ) +
  labs(
    x = "Chronological phase",
    y = "Insert length (mm)"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(t = 18, r = 10, b = 10, l = 10),
    legend.position = "right"
  )

print(figure_11)

ggsave(
  file.path(output_folder, "12_insert_length_by_phase.png"),
  figure_11,
  width = 12,
  height = 7,
  units = "in",
  dpi = 300
)

# Provide median length per phase
length_summary_by_phase <- df_length_plot %>%
  dplyr::group_by(PHASE) %>%
  dplyr::summarise(
    n = dplyr::n(),
    median_length_mm = round(stats::median(LENGTH), 1),
    q1_mm = round(stats::quantile(LENGTH, 0.25), 1),
    q3_mm = round(stats::quantile(LENGTH, 0.75), 1),
    min_mm = round(min(LENGTH), 1),
    max_mm = round(max(LENGTH), 1),
    .groups = "drop"
  )

print(length_summary_by_phase)

write.csv(
  length_summary_by_phase,
  file.path(output_folder, "insert_length_summary_by_phase.csv"),
  row.names = FALSE
)
