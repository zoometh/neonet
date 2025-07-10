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
library(readxl2)
library(ggplot2)
library(Momocs)
library(Rtools)



#########################
# 1 🔹 LOAD DATA AND SHAPES
#########################

set.seed(123)
sf::sf_use_s2(FALSE)

sampling <- FALSE
elbow.sickles <- TRUE

fig.full.h <- 15
fig.full.w <- 17
fig.half.h <- 9
fig.half.w <- 12

library(openxlsx)
path.data <- "yourpath/"
df.coords <- read.xlsx(paste0(path.data, "/COORD.xlsx"))
jpgs <- paste0(path.data, "/img")  # Image folder
lf <- list.files(jpgs, full.names = TRUE)  # Store images to list
lf <- sort(list.files(jpgs, full.names = TRUE))

# Define output folder exists
output_folder <- file.path(path.data, "out4")
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
# 2 🔹 SMOOTH SHAPES
#########################

sickles <- Out(coo)
sickles <- coo_interpolate(sickles, n = 100)
sickles <- sickles %>% 
  # coo_scale() %>% 
  # coo_alignxax() %>%
  coo_center()

# Apply smoothing to remove pixel noise to each shape
library(Momocs)   
shapes_con_errore <- c()

# Cicliamo su tutti i nomi (e indici) dei contorni in sickles$coo
for (i in seq_along(sickles$coo)) {
  nome_shape <- names(sickles$coo)[i]
  
  # Estraiamo un oggetto Out che contiene solo il singolo contorno i-esimo
  # In Momocs, subsetting di un Out con sickles[i] restituisce un Out di lunghezza 1
  singolo_out <- sickles[i]
  
  # Proviamo a chiamare coo_smooth su questo singolo contorno
  risultato <- try(Momocs::coo_smooth(singolo_out, n = 1), silent = TRUE)
  
  # Se si verifica un errore, lo registriamo in shapes_con_errore
  if (inherits(risultato, "try-error")) {
    shapes_con_errore <- c(shapes_con_errore, nome_shape)
    message("Errore su shape: ", nome_shape)
  }
}

# Alla fine, 'shapes_con_errore' conterrà i nomi di tutte le sagome che hanno fallito il smoothing.
cat("Shape problematici trovati:", length(shapes_con_errore), "\n")
print(shapes_con_errore)


#########################
# 3 🔹 CHECK VALID SHAPES
#########################

library(Momocs)   
# Ensure a consistent number of points per shape
sickles <- coo_interpolate(sickles, n = 80)

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

panel.out <- paste0(path.data, "/out4/1_panel.jpg")
jpeg(panel.out, height = fig.full.h, width = fig.full.w, units = "cm", res = 600)
panel(sickles,
      names = TRUE,
      cols = shape.colors,  # Color by site
      borders = shape.colors,  # Border color also by site
      cex.names = 0.2,
      main = "Shapes Panel Colored by Site",
      cex.main = 0.8
)
dev.off()


###########################
# 7 🔹 STANDARDIZED STACKS
###########################

stack.out <- paste0(path.data, "/out4/2_stack.jpg")
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
write.xlsx(membership_table, paste0(path.data, "/out4/membership_table.xlsx"))

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
jpeg(paste0(path.data, "/out4/3_clust.jpg"), height = 15, width = 17, units = "cm", res = 400)
CLUST(
  sickles.f,
  hclust_method = "ward.D2",
  k             = optimal_clusters,
  palette       = my.color.ramp,
  cex           = 0.2
)
dev.off()

# 9B) PCA scatter with labels colored by cluster (shapes remain unchanged)
jpeg(paste0(path.data, "/out4/4_pca.jpg"), height = 15, width = 17, units = "cm", res = 400)

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
membership_table_file <- paste0(path.data, "/out4/1_membership_table.xlsx")
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

# Loop through each Combined_Cluster and save shape panels
for (cluster in unique(membership$Cluster)) {
  
  cluster_indices <- which(membership$Cluster == cluster)
  shape_names <- membership$Shape_Name[cluster_indices]
  
  # Match the shape names to the sickles object
  shape_indices <- which(names(sickles) %in% shape_names)
  cluster_shapes <- sickles[shape_indices]
  
  cat("\nProcessing Cluster", cluster, "with", length(cluster_shapes), "shapes")
  
  if (!inherits(cluster_shapes, "Out")) {
    cluster_shapes <- tryCatch(Out(cluster_shapes), error = function(e) NULL)
  }
  
  if (!is.null(cluster_shapes) && length(cluster_shapes) > 0) {
    # Output file
    cluster_out_file <- file.path(output_folder, paste0("original_cluster_", cluster, "_shapes.jpg"))
    
    jpeg(cluster_out_file, height = fig.full.h, width = fig.full.w, units = "cm", res = 600)
    
    panel(cluster_shapes,
          names = TRUE,
          cols = combined_colors[as.integer(cluster)],
          main = paste("Cluster", cluster, "Shapes"))
    
    dev.off()
    
    cat("\nSaved:", cluster_out_file)
  } else {
    cat("\nSkipping Cluster", cluster, "due to invalid shape data.")
  }
}


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
chrono_file <- "yourpath/data.xlsx"

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
new_membership_table_file <- paste0(path.data, "/out4/new_membership_table.xlsx")
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
jpeg("yourpath/out/6_pca2.jpeg",
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
# 20🔹 Match Shapes with Chronological Data
#########################################################################################

# Ensure that the names in your Excel data (df_joined$Shape_Name) match the names in the Out object.
common_shapes <- intersect(df_chrono$Shape_Name, names(sickles))
if(length(common_shapes) == 0) {
  stop("No common shape names found between Excel and shapes data. Check your column names.")
}

# Filter the data frame to only shapes available in your shapes object,
# sort by MEDIAN (chronological order), and assign each a vertical lane.
df_plot <- df_chrono %>%
  filter(Shape_Name %in% common_shapes) %>%
  arrange(MEDIAN) %>%
  mutate(shape_lane = row_number())

library(dplyr)
# Add cluster assignment to df_plot
df_names <- data.frame(Shape_Name = names(sickles),
                       Cluster = kmeans_result$cluster)
df_plot <- df_plot %>%
  left_join(df_joined[, c("Shape_Name", "Combined_Cluster")], by = "Shape_Name") %>%
  rename(Cluster = Combined_Cluster)


#########################################################################################
# 21🔹 Calculate Internal Percentages of Shapes by Cluster Over Time
#########################################################################################

# Create time bins along the MEDIAN axis.
num_bins <- 10
df_plot <- df_plot %>%
  mutate(time_bin = cut(MEDIAN, breaks = num_bins, include.lowest = TRUE))

# Compute counts per time bin per cluster.
df_time_cluster <- df_plot %>%
  group_by(time_bin, Cluster) %>%
  summarise(count = n(), .groups = "drop")

# Also compute the total count per time bin.
df_time_total <- df_plot %>%
  group_by(time_bin) %>%
  summarise(total = n(), .groups = "drop")

# Merge and compute percentage.
df_time_cluster <- df_time_cluster %>%
  left_join(df_time_total, by = "time_bin") %>%
  mutate(percentage = 100 * count / total)


#########################################################################################
# 22🔹 Plot Dominance of Clusters Over Time as a Stacked Bar Chart
#########################################################################################

library(scales)
# df_joined already merges df_names with df_chrono and has the new Combined_Cluster.
# If needed, you can also convert MEDIAN to an ordered factor:
df_joined$MEDIAN <- factor(df_joined$MEDIAN,
                           levels = sort(unique(df_joined$MEDIAN)),
                           ordered = TRUE)

# Create the bar chart object for the combined clustering with identical style.
bar_chart2 <- ggplot(df_joined, aes(x = MEDIAN, fill = factor(Combined_Cluster))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Proportion of Combined Clusters by MEDIAN Period",
       x = "MEDIAN Period",
       y = "Proportion of Shapes",
       fill = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Open the device
jpeg("yourpath/out/9_bar_chart2.jpeg",
     width = 14, height = 8, units = "in", res = 300)

# Print the ggplot object
print(bar_chart2)

# Close the device
dev.off()


#########################################################################################
# 23🔹 Compare clustering procedures
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
# 24🔹 Compare clusters metrically
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
# 25.  “3‐in‐1” Boxplot of Length, Width, and Thickness by Renamed Cluster
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
  geom_boxplot(outlier.size = 1, alpha = 0.8) +
  scale_fill_manual(values = palette_4clusters) +
  facet_wrap(~ Measure, nrow = 1, scales = "free_y") +
  labs(
    title = "",
    x     = "Cluster (RENAMED)",
    y     = "mm",
    fill  = "Cluster"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text       = element_text(face = "bold"),
    axis.title.x     = element_blank(),
    axis.text.x      = element_text(size = 10),
    axis.title.y     = element_text(size = 11),
    legend.position  = "none"
  )

# 5) Save to JPEG
jpeg(file.path(output_folder, "8_boxplots_L_W_T_by_cluster_renamed.jpg"),
     width  = 14,   # in inches
     height = 5,    # in inches
     units  = "in",
     res    = 300)
print(box3_in1)
dev.off()


#########################################################################################
# 26🔹 Plot Dominance of Clusters Over Time as a Stacked Bar Chart Using Renamed Clusters
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
jpeg("yourpath/out/11_bar_chart3.jpeg",
     width = 14, height = 8, units = "in", res = 300)
print(bar_chart2)
dev.off()


################################################
# 27🔹 All clusters SEPARATED along X
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
## 28 AVERAGE SHAPES FOR CLUSTERS
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


##########################################################################################
## 29 CARPO-LITHIC COEVOLUTION: SHARED AXIS, PHASES, BREAKPOINTS
##########################################################################################

# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(segmented)
library(patchwork)
library(openxlsx)

# =====================================================
# 30. Prepare cluster proportions per phase
# =====================================================

library(dplyr)

df_joined$MEDIAN <- as.numeric(as.character(df_joined$MEDIAN))

cluster_summary <- df_joined %>%
  group_by(PHASE, Cluster) %>%
  summarise(count = n(),
            MEDIAN = mean(MEDIAN, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(PHASE) %>%
  mutate(total = sum(count),
         percentage = round(100 * count / total, 2)) %>%
  ungroup() %>%
  dplyr::select(PHASE, MEDIAN, Cluster, percentage) %>%
  pivot_wider(names_from = Cluster,
              values_from = percentage,
              names_prefix = "Cluster_",
              values_fill = 0)

combined_cluster_summary <- df_joined %>%
  group_by(PHASE, Combined_Cluster_Renamed) %>%
  summarise(count = n(),
            MEDIAN = mean(MEDIAN, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(PHASE) %>%
  mutate(total = sum(count),
         percentage = round(100 * count / total, 2)) %>%
  ungroup() %>%
  dplyr::select(PHASE, MEDIAN, Combined_Cluster_Renamed, percentage) %>%
  pivot_wider(names_from = Combined_Cluster_Renamed,
              values_from = percentage,
              names_prefix = "Combined_Cluster_",
              values_fill = 0)


# =====================================================
# 31. Merge with carpological data
# =====================================================

df_carpological <- read.xlsx("yourpath/carpo.xlsx")

combined_data <- cluster_summary %>%
  left_join(df_carpological, by = "PHASE")

# Determine x-axis range from MEDIAN values
x_min <- min(combined_data$MEDIAN.x, na.rm = TRUE)
x_max <- max(combined_data$MEDIAN.x, na.rm = TRUE)

combined_data2 <- combined_cluster_summary %>%
  left_join(df_carpological, by = "PHASE")

# Determine x-axis range from MEDIAN values
x_min <- min(combined_data2$MEDIAN.x, na.rm = TRUE)
x_max <- max(combined_data2$MEDIAN.x, na.rm = TRUE)

# =====================================================
# 32. Cluster Proportions Plot
# =====================================================

# Convert cluster proportions data to long format
long_clusters <- combined_data %>%
  dplyr::select(MEDIAN.x, starts_with("Cluster")) %>%
  pivot_longer(cols = starts_with("Cluster"),
               names_to = "Cluster",
               values_to = "Percentage")

# Phase start dates
phase_starts <- data.frame(
  phase = c("PPNA", "EPPNB-A", "EPPNB-B", "EPPNB-C", "MPPNB", "LPPNB", "AC-1", "AC-2"),
  start = c(9100, 8400, 7900, 7600, 7500, 7200, 6800, 6200)
)


# Convert cluster proportions data to long format
long_clusters2 <- combined_data2 %>%
  dplyr::select(MEDIAN.x, starts_with("Combined_Cluster")) %>%
  pivot_longer(cols = starts_with("Combined_Cluster"),
               names_to = "Combined_Cluster",
               values_to = "Percentage")

# Phase start dates
phase_starts <- data.frame(
  phase = c("PPNA", "EPPNB-A", "EPPNB-B", "EPPNB-C", "MPPNB", "LPPNB", "AC-1", "AC-2"),
  start = c(9100, 8400, 7900, 7600, 7500, 7200, 6800, 6200)
)

# Create cluster proportions plot
p_clusters <- ggplot(long_clusters, aes(x = MEDIAN.x, y = Percentage, color = Cluster)) +
  geom_point(size = 1) +
  geom_smooth(method = 'loess', se = FALSE, linetype = "longdash", size = 0.8) +
  scale_x_continuous(limits = c(x_min, x_max)) +
  labs(title = "Sickle Insert Types through Time_Original_clusters",
       x = "Chronological Median",
       y = "Cluster Proportion (%)") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.margin = margin(t = 15, r = 10, b = 10, l = 10)
  )

# Create cluster proportions plot
p_clusters2 <- ggplot(long_clusters2, aes(x = MEDIAN.x, y = Percentage, color = Combined_Cluster)) +
  geom_point(size = 1) +
  geom_smooth(method = 'loess', se = FALSE, linetype = "longdash", size = 0.8) +
  scale_x_continuous(limits = c(x_min, x_max)) +
  labs(title = "Sickle Insert Types through Time_Combined Clusters",
       x = "Chronological Median",
       y = "Cluster Proportion (%)") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.margin = margin(t = 15, r = 10, b = 10, l = 10)
  )


df_joined$MEDIAN <- as.numeric(as.character(df_joined$MEDIAN))
df_carpological <- read.xlsx("yourpath/carpo.xlsx")

combined_data <- cluster_summary %>%
  left_join(df_carpological, by = "PHASE")


# (Optional) Ensure proper ordering of PHASE
df_carpological <- df_carpological %>%
  mutate(PHASE = factor(PHASE, levels = c("PPNA", "EPPNB-A", "EPPNB-B",
                                          "EPPNB-C", "MPPNB", "LPPNB", "AC-1", "AC-2")))

# 1. Aggregate presence by phase using both column selectors
phase_presence <- df_carpological %>%
  group_by(PHASE) %>%
  summarise(
    across(c(starts_with("Triticum"), starts_with("Hordeum")), 
           ~ sum(. == 1, na.rm = TRUE) / sum(!is.na(.)))
  )

# 2. Convert to long format
phase_presence_long <- phase_presence %>%
  pivot_longer(cols = -PHASE, names_to = "Species", values_to = "Proportion")

# 3. Create a bar chart of species presence by phase
carpo <- ggplot(phase_presence_long, aes(x = PHASE, y = Proportion, fill = Species)) +
  geom_col(position = "dodge") +  # you can change to "stack" if preferred
  theme_minimal() +
  labs(title = "Species Presence by Phase",
       x = "Phase",
       y = "Proportion of Sites with Species Present") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(carpo)
print(phase_presence)
print(phase_presence_long)

ggsave("species_presence_plot.jpeg", plot = carpo, width = 8, height = 6, units = "in", dpi = 300)

# 4. Save the plot in the out4 folder.
# Make sure to adjust the path accordingly.
ggsave(filename = "yourpath/out/species_presence_plot.jpeg", 
       plot = carpo, 
       width = 8, height = 6, units = "in", dpi = 300)



# -------------------------------
# 33. Compute Species Diversity Through Time
# -------------------------------

# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# Define species columns from df_carpological:
species_cols <- c("Hordeum.spontaneum", 
                  "Hordeum.vulgare", 
                  "Triticum.dicoccum",
                  "Triticum.monococcum", 
                  "Triticum.timopheevii.s.l.", 
                  "Triticum.dicoccoides/dicoccum",
                  "Triticum.turgidum/durum", 
                  "Triticum.aestivum")

# Ensure species columns are numeric (in case they were read as characters/factors)
df_carpological <- df_carpological %>%
  mutate(across(all_of(species_cols), ~ as.numeric(as.character(.))))

# Compute the diversity index (percentage of species present per site)
# For each site, the index is (number of species present / total species) * 100.
df_carpological <- df_carpological %>%
  mutate(diversity_index = (rowSums(across(all_of(species_cols)), na.rm = TRUE) / length(species_cols)) * 100)

# Ensure PHASE is an ordered factor so that phases appear in the desired chronological order.
df_carpological <- df_carpological %>%
  mutate(PHASE = factor(PHASE, levels = c("PPNA", "EPPNB-A", "EPPNB-B", 
                                          "EPPNB-C", "MPPNB", "LPPNB", "AC-1", "AC-2")))

# Aggregate the diversity index by phase.
# We also compute a representative chronological median (phase_med) for each phase.
phase_diversity <- df_carpological %>%
  group_by(PHASE) %>%
  summarise(mean_diversity = mean(diversity_index, na.rm = TRUE),
            phase_med = mean(MEDIAN, na.rm = TRUE)) %>%  # e.g. PPNA: -8850, AC-2: -5612
  ungroup()

# Create the species diversity plot.
# Here we use scale_x_reverse() so that -9000 (oldest) appears at the left
# and -4000 (youngest) appears at the right.
p_diversity <- ggplot(phase_diversity, aes(x = phase_med, y = mean_diversity)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dotted", color = "blue", size = 1) +
  scale_y_continuous(name = "Species Diversity Index (%)", limits = c(0, 100)) +
  scale_x_reverse(name = "Chronological Median") +
  theme_minimal() +
  labs(title = "Species Diversity Through Phases")

# Display the species diversity plot.
print(p_diversity)


# -------------------------------
# 34. Combine Sickle & Diversity Plots
# -------------------------------
# Option A: Stacked (Vertical) Composite Using Patchwork
# Assume that 'p_clusters2' is your sickle proportions plot,
# which has been created to use the same x-axis scale (i.e. scale_x_reverse(limits = c(-9000, -4000))).
final_plot_stacked <- p_clusters2 / p_diversity +
  plot_layout(heights = c(2, 1))   # Adjust height ratio as desired

# Display and save the stacked plot.
print(final_plot_stacked)
ggsave("yourpath/out/final_combined_plot_stacked.jpeg",
       final_plot_stacked, width = 14, height = 5, units = "in", dpi = 300)


# Option B: Overlaid in a Single Plot
# Here we overlay the diversity curve on your sickle proportions plot.
p_combined <- p_clusters2 +
  geom_line(data = phase_diversity, aes(x = phase_med, y = mean_diversity),
            color = "blue", size = 1, linetype = "dotted") +
  geom_point(data = phase_diversity, aes(x = phase_med, y = mean_diversity),
             color = "blue", size = 5) +
  scale_x_reverse(name = "Chronological Median", limits = c(-9000, -4000))  # Ensure same x-axis

# Display and save the overlaid plot.
print(p_combined)
ggsave("yourpath/out/final_combined_plot_overlayed.jpeg",
       p_combined, width = 14, height = 3, units = "in", dpi = 300)


# Optional: Print the aggregated data to verify that all phases are present.
print(phase_diversity)


library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# -------------------------------
# 35. Print & Inspect Data
# -------------------------------
cat("=== LONG CLUSTERS 2 (SICKLE) ===\n")
print(head(long_clusters2))
print(summary(long_clusters2))

cat("\n=== PHASE DIVERSITY (CEREALS) ===\n")
print(phase_diversity)
cat("\n")

# Determine x-axis limits from your diversity data (or set manually)
x_min <- min(phase_diversity$phase_med, na.rm = TRUE)  # Should be around -8850
x_max <- max(phase_diversity$phase_med, na.rm = TRUE)  # Should be around -5612

# For a small margin, you can adjust these:
# For example, extend a bit on both sides if desired:
x_min_plot <- x_min - 50   # e.g., -8900
x_max_plot <- x_max + 50   # e.g., -5562

# -------------------------------
# (B) Plot the Sickle Proportions (p_clusters2)
# -------------------------------
# Here we assume long_clusters2 has a column "MEDIAN.x" for the chronological median.
p_clusters2 <- ggplot(long_clusters2, aes(x = MEDIAN.x, y = Percentage, color = Combined_Cluster)) +
  geom_point(size = 2) +
  geom_smooth(method = 'loess', se = FALSE, linetype = "longdash", size = 0.8) +
  # Use a continuous x-axis (older on the left, younger on the right).
  scale_x_continuous(name = "Chronological Median", limits = c(x_min_plot, x_max_plot)) +
  scale_y_continuous(name = "Cluster Proportion (%)", limits = c(0, 100)) +
  labs(title = "Sickle Insert Types through Time_Combined Clusters") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.margin = margin(t = 15, r = 10, b = 10, l = 10)
  )

# -------------------------------
# 36. Plot the Diversity Index (p_diversity)
# -------------------------------
p_diversity <- ggplot(phase_diversity, aes(x = phase_med, y = mean_diversity)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dotted", color = "blue", size = 1) +
  scale_x_continuous(name = "Chronological Median", limits = c(x_min_plot, x_max_plot)) +
  scale_y_continuous(name = "Species Diversity Index (%)", limits = c(0, 100)) +
  labs(title = "Species Diversity Through Phases") +
  theme_minimal()

# View each plot individually
print(p_clusters2)
print(p_diversity)

# -------------------------------
# 37. Combine Plots with Patchwork
# -------------------------------
# Option A: Stacked (Vertical) Composite
final_plot_stacked <- p_clusters2 / p_diversity +
  plot_layout(heights = c(2, 1))  # Adjust the relative heights as desired

print(final_plot_stacked)

ggsave("yourpath/out/final_combined_plot_stacked.jpeg",
       final_plot_stacked, width = 14, height = 5, units = "in", dpi = 300)

# Option B: Overlaid in a Single Plot
p_combined <- p_clusters2 +
  geom_line(data = phase_diversity, aes(x = phase_med, y = mean_diversity),
            color = "blue", size = 1, linetype = "dotted") +
  geom_point(data = phase_diversity, aes(x = phase_med, y = mean_diversity),
             color = "blue", size = 2) +
  scale_x_continuous(name = "Chronological Median", limits = c(x_min_plot, x_max_plot))

print(p_combined)

ggsave("yourpath/out/final_combined_plot_overlayed.jpeg",
       p_combined, width = 14, height = 5, units = "in", dpi = 300)

# Optionally, print the aggregated diversity data to confirm all phases are included.
print(phase_diversity)


# Option B: Overlaid in a Single Plot with a Smoother Line
p_combined <- p_clusters2 +
  geom_smooth(data = phase_diversity, aes(x = phase_med, y = mean_diversity),
              method = "loess", se = FALSE, span = 0.75,
              color = "blue", size = 1, linetype = "dotted") +
  geom_point(data = phase_diversity, aes(x = phase_med, y = mean_diversity),
             color = "blue", size = 2) +
  scale_x_continuous(name = "Chronological Median", limits = c(x_min_plot, x_max_plot))

print(p_combined)

ggsave("yourpath/out/final_combined_plot_overlayed_def.jpeg",
       p_combined, width = 14, height = 4.5, units = "in", dpi = 300)

# Optionally, print the aggregated diversity data to confirm all phases are included.
print(phase_diversity)


#################################################################################################################
##### TEST SIZE EVOLUTION FOR CLUSTERS 1 AND 2
#################################################################################################################

# Load required libraries
library(dplyr)
library(ggplot2)

# Filter your data to include only Cluster 1 and 2
df_filtered <- df_joined %>%
  filter(Combined_Cluster_Renamed %in% c(1, 2)) %>%
  mutate(TOTAL_SIZE = LENGTH + WIDTH + THICKNESS)

# Create a scatterplot with regression line
ggplot(df_filtered, aes(x = MEDIAN, y = TOTAL_SIZE, color = as.factor(Combined_Cluster_Renamed))) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  scale_color_manual(name = "Cluster", values = c("red", "blue"),
                     labels = c("Cluster 1", "Cluster 2")) +
  labs(
    x = "Median Cal BCE",
    y = "Total Insert Size (L + W + T in mm)",
    title = "Regression of Insert Size (L+W+T) Over Time for Clusters 1 and 2"
  ) +
  theme_minimal(base_size = 14)


# Load necessary libraries
library(dplyr)
library(ggplot2)

# Filter data to include only Cluster 1 and 2, and create unified group
df_combined <- df_joined %>%
  filter(Combined_Cluster_Renamed %in% c(1, 2)) %>%
  mutate(TOTAL_SIZE = LENGTH + WIDTH + THICKNESS) %>%
  mutate(Cluster_Group = "Clusters 1 + 2")

# Plot regression line for the combined group
ggplot(df_combined, aes(x = MEDIAN, y = TOTAL_SIZE)) +
  geom_point(color = "darkblue", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkblue", linewidth = 1.2) +
  labs(
    x = "Median Cal BCE",
    y = "Total Insert Size (L + W + T in mm)",
    title = "Regression of Combined Insert Size (Clusters 1 + 2) Over Time"
  ) +
  theme_minimal(base_size = 14)

ggplot(df_combined, aes(x = MEDIAN, y = TOTAL_SIZE)) +
  geom_point(color = "darkblue", size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = "darkblue", span = 0.75, linewidth = 1.2) +
  labs(
    x = "Median Cal BCE",
    y = "Total Insert Size (L + W + T in mm)",
    title = "LOESS Smoothing of Combined Insert Size Over Time"
  ) +
  theme_minimal(base_size = 14)

ggplot(df_combined, aes(x = MEDIAN, y = TOTAL_SIZE)) +
  geom_point(color = "darkblue", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "darkblue", linewidth = 1.2) +
  labs(
    x = "Median Cal BCE",
    y = "Total Insert Size (L + W + T in mm)",
    title = "Quadratic Regression of Combined Insert Size Over Time"
  ) +
  theme_minimal(base_size = 14)

model <- nls(TOTAL_SIZE ~ a * exp(b * MEDIAN), data = df_combined, start = list(a = 1, b = 0.0001))
summary(model)

preds      <- predict(model)
pseudo_R2  <- cor(df_combined$TOTAL_SIZE, preds)^2
pseudo_R2

# Linear model
model_linear <- lm(TOTAL_SIZE ~ MEDIAN, data = df_combined)
summary(model_linear)$r.squared

# Polynomial model (quadratic)
model_quad <- lm(TOTAL_SIZE ~ poly(MEDIAN, 2), data = df_combined)
summary(model_quad)$r.squared

# Polynomial model (cubic)
model_cubic <- lm(TOTAL_SIZE ~ poly(MEDIAN, 3), data = df_combined)
summary(model_cubic)$r.squared

# Polynomial model (cubic)
model_exp <- lm(TOTAL_SIZE ~ poly(MEDIAN, 3), data = df_combined)
summary(model_exp)$r.squared

summary(model_linear)$r.squared
summary(model_quad)$r.squared
summary(model_cubic)$r.squared
summary(model_exp)$r.squared

AIC(model, model_linear, model_quad, model_cubic)

# Create cubic regression plot
p <- ggplot(df_combined, aes(x = MEDIAN, y = TOTAL_SIZE)) +
  geom_point(color = "darkblue", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = TRUE,
              color = "darkred", linewidth = 1.2) +
  labs(
    x = "Median Cal BCE",
    y = "Total Insert Size (mm)",
    title = "Cubic Regression: Insert Size Over Time (Clusters 1 + 2)"
  ) +
  theme_minimal(base_size = 14)


# Display plot
print(p)

# Save the plot to your specified output folder
ggsave("yourpath/out/cubic_regression_insert_size.png",
       plot = p, width = 10, height = 6, dpi = 300)


str(head(df_joined))
print(head(df_joined))







