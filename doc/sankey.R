neo_kcc_sankey <- function(kcc_data = NA,
                           roi = NA,
                           col.req = NA,
                           kcc_colors = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/koppen.tsv",
                           selected.per = NA,
                           verbose = TRUE) {
  `%>%` <- dplyr::`%>%`
  
  # Read color mapping
  koppen_colors <- read.table(kcc_colors, sep = "\t", header = TRUE)
  kcc_color_map <- setNames(koppen_colors$color, koppen_colors$code)
  rev_map <- setNames(rownames(koppen_colors), koppen_colors$code)
  
  if (inherits(kcc_data, "sf")) {
    if (verbose) message("Processing site-based KCC changes")
    
    df <- sf::st_set_geometry(kcc_data, NULL)
    
    if (!all(col.req %in% names(df))) {
      stop("Some 'col.req' fields are missing in the site dataframe.")
    }
    
    df <- df[, col.req, drop = FALSE]
    
    if (!is.na(selected.per)) {
      df <- df[df$Period %in% selected.per, ]
    }
    
    period.names <- paste0(selected.per, collapse = "-")
    title_text <- paste0(period.names, " KCC changes")
    caption_text <- paste0("Number of archaeological sites = ", nrow(df))
    
  } else if (inherits(kcc_data, "character")) {
    if (verbose) message("Processing area-based KCC changes")
    
    source("R/neo_kcc_crop.R")
    
    if (verbose) message("Cropping maps on ROI")
    
    nb_stages <- length(kcc_data)
    df_list <- vector("list", nb_stages)
    stage_names <- character(nb_stages)
    
    for (i in seq_len(nb_stages)) {
      map <- neo_kcc_crop(kcc_data[i], roi = roi)
      df_list[[i]] <- as.data.frame(map, xy = FALSE)
      stage_names[i] <- terra::varnames(map)
    }
    
    # Create a combined dataframe
    df <- as.data.frame(do.call(cbind, df_list))
    names(df) <- stage_names
    
    # Replace numeric KCC values with their labels
    df[stage_names] <- lapply(df[stage_names], function(x) rev_map[as.character(x)])
    
    title_text <- "KCC changes"
    caption_text <- paste0("Number of climate cells = ", nrow(df))
  }
  
  # Build the Sankey dataframe
  df.sank <- df %>% ggsankey::make_long(colnames(df))
  
  if (verbose) message("Building Sankey diagram")
  
  plot <- ggplot2::ggplot(df.sank, ggplot2::aes(
    x = x, next_x = next_x,
    node = node, next_node = next_node,
    fill = factor(node), label = node)) +
    ggsankey::geom_sankey(node.color = "black", show.legend = FALSE) +
    ggsankey::geom_sankey_label(size = 3, color = "black", fill = "white", hjust = 0.5) +
    ggplot2::scale_fill_manual(values = kcc_color_map) +
    ggplot2::labs(title = title_text, caption = caption_text) +
    ggplot2::theme_bw()
  
  if (verbose) message("Sankey plot created")
  
  return(plot)
}













# Count transitions from OldClass to NewClass
transition_counts <- df_changes %>%
  dplyr::group_by(OldClass, NewClass) %>%
  dplyr::summarise(count = dplyr::n(), .groups = "drop")

# Convert to a format suitable for Sankey plotting
links <- transition_counts %>%
  dplyr::rename(source = OldClass, target = NewClass, value = count)

# Extract unique nodes (classes)
nodes <- data.frame(name = unique(c(links$source, links$target)))

# Map class names to node indices
links$source <- match(links$source, nodes$name) - 1
links$target <- match(links$target, nodes$name) - 1

sankey <- networkD3::sankeyNetwork(Links = links, Nodes = nodes,
                                   Source = "source", Target = "target",
                                   Value = "value", NodeID = "name",
                                   fontSize = 12, nodeWidth = 30)

# Display the Sankey diagram
sankey



source("R/neo_kcc_sankey.R")
neo_kcc_sankey(df_cc,
               col.req = col.req,
               selected.per = c("EN"))

med.area <- "https://raw.githubusercontent.com/zoometh/neonet/main/doc/talks/2024-simep/roi.geojson"
med.area <- sf::st_read(med.area,
                        quiet = TRUE)

library(terra)
library(dplyr)
library(tidyr)

source("R/neo_kcc_map_buffer.R")
kcc_clipped_list <- neo_kcc_map_buffer(buffer_km = 300)
num_rasters <- length(kcc_clipped_list)
transitions <- list()
for (i in 1:(num_rasters - 1)) {
  first <- values(kcc_clipped_list[[i]])
  second <- values(kcc_clipped_list[[i + 1]])
  # Create a data frame for current transitions
  trans_df <- data.frame(
    from = first[ , 1],
    to = second[ , 1],
    step = i
  )
  # rm NA
  trans_df <- trans_df[!is.na(trans_df$from) & !is.na(trans_df$to), ]
  transitions[[i]] <- trans_df
}

# Combine all transitions into one data frame
all_transitions <- bind_rows(transitions)
#all_transitions[sample(nrow(all_transitions), 15), ]
# nodes <- data.frame(name = unique(c(all_transitions$from, all_transitions$to)))
transition_summary <- all_transitions %>%
  group_by(from, to, step) %>%
  summarise(count = n(), .groups = 'drop')
library(networkD3)

# Create nodes data frame
nodes <- data.frame(name=unique(c(transition_summary$from, transition_summary$to)))

# Create a lookup to match names to indices
nodes$ID <- 0:(nrow(nodes) - 1)

# Create links by matching from and to with their respective node indices
links <- transition_summary %>%
  left_join(nodes, by = c("from" = "name")) %>%
  rename(source = ID) %>%
  left_join(nodes, by = c("to" = "name")) %>%
  rename(target = ID) %>%
  select(source, target, count)
# Generate the Sankey Diagram
sankey <- sankeyNetwork(Links = links, Nodes = nodes, Source = "source", 
                        Target = "target", Value = "count", NodeID = "name",
                        units = "Transitions")

# Print the Sankey diagram
print(sankey)




# Create a summary of transitions
transition_summary <- all_transitions %>%
  group_by(from, to) %>%
  summarise(count = n(), .groups = 'drop')
library(networkD3)

# Create nodes and links for the Sankey diagram
nodes <- data.frame(name=c(as.character(unique(c(transition_summary$from, transition_summary$to)))))
links <- merge(transition_summary, nodes, by.x="from", by.y="name")
links <- merge(links, nodes, by.x="to", by.y="name")
links <- data.frame(source=links$id.y-1, target=links$id-1, value=links$count)

# Plot Sankey diagram
sankey <- sankeyNetwork(Links = links, Nodes = nodes,
                        Source = "source", Target = "target",
                        Value = "value", NodeID = "name",
                        units = "Count")
print(sankey)







library(dplyr)  # For data manipulation
library(networkD3)  # For creating Sankey diagrams
stacked_rasters <- rast(kcc_clipped_list)
pixel_values <- as.data.frame(values(stacked_rasters))
colnames(pixel_values) <- paste0("Layer", seq_along(kcc_clipped_list))
transition_data <- pixel_values %>%
  group_by(across(everything())) %>%
  summarise(count = n(), .groups = 'drop')
unique_values <- unique(unlist(pixel_values))
nodes <- data.frame(name = as.character(unique_values))
links <- data.frame()
for (i in seq_len(ncol(pixel_values) - 1)) {
  links <- rbind(links, 
                 data.frame(source = match(pixel_values[[i]], unique_values) - 1,
                            target = match(pixel_values[[i + 1]], unique_values) - 1,
                            value = rep(1, length(pixel_values[[i]]))))
}
links <- links %>%
  group_by(source, target) %>%
  summarise(value = n(), .groups = 'drop')
sankeyNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target", 
              Value = "value", NodeID = "name", fontSize = 12, nodeWidth = 30)
# neo_kcc_sankey()?

# unlink(temp_file)


# Load necessary libraries
library(sf)
library(terra)

# Read the polygon (GeoJSON) into an sf object
# med.coast <- "https://raw.githubusercontent.com/zoometh/neonet/main/doc/data/med_sea.geojson"
# med.coast <- sf::st_read(med.coast, quiet = TRUE)

# # Define the raw GitHub URL to the .tif file
# koppen_8k <- "https://raw.githubusercontent.com/zoometh/neonet/main/doc/data/clim/koppen_8k.tif"
# 
# # Download the GeoTIFF file temporarily
# temp_file <- tempfile(fileext = ".tif")
# download.file(koppen_8k, destfile = temp_file, mode = "wb")

# Read the raster data using the terra package
# what <- rast(temp_file)



# Clean up: Remove the temporary file
# unlink(temp_file)



####