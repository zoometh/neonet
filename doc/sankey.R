# Load both GeoTIFFs as raster layers
# koppen_8k <- rast("/mnt/data/koppen_8k.tif")
# koppen_7k <- rast("/mnt/data/koppen_7k.tif")

root.path = "https://raw.githubusercontent.com/zoometh/neonet/main/doc/data/clim/"
before.map <- rast(paste0(root.path, "koppen_8k.tif"))
after.map <- rast(paste0(root.path, "koppen_7k.tif"))

roi <- sf::st_read("https://raw.githubusercontent.com/zoometh/neonet/main/doc/talks/2024-simep/roi.geojson",
                   quiet = TRUE)
before.map.select <- crop(before.map, roi)
after.map.select <- crop(after.map, roi)
after.map.select <- resample(before.map.select, after.map.select)

plot(before.map.select)
plot(after.map.select)

# Convert rasters to data frames
before.df <- as.data.frame(before.map.select, xy = FALSE)
after.df  <- as.data.frame(after.map.select, xy = FALSE)

# Combine into a single data frame
df_changes <- data.frame(OldClass = before.df[,1], NewClass = after.df[,1])

# Remove NA values if necessary
df_changes <- na.omit(df_changes)

# Count transitions from OldClass to NewClass
transition_counts <- df_changes %>%
  group_by(OldClass, NewClass) %>%
  summarise(count = n(), .groups = "drop")

# Convert to a format suitable for Sankey plotting
links <- transition_counts %>%
  rename(source = OldClass, target = NewClass, value = count)


# Extract unique nodes (classes)
nodes <- data.frame(name = unique(c(links$source, links$target)))

# Map class names to node indices
links$source <- match(links$source, nodes$name) - 1
links$target <- match(links$target, nodes$name) - 1

sankey <- sankeyNetwork(Links = links, Nodes = nodes,
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