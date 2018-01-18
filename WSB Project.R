# Walking School Bus start file

# Idea: Build a simulation study that ogranically creates data points,
#       Calls Google API in path mapping, and optimizes number of walking school busses
#       With minimum requirements on student distance and number of busses

# Step 1: Grab a set of schools and their corresponding catchment regions
library(tidyverse)
library(rvest)
library(pdftools)
library(rvest)
library(ggmap)

# SOmething to consider if we pursue this project

# # Get link URLs
# main.page <- read_html(x = "http://www.sd41.bc.ca/elementary-school-addresses/")
# urls <- main.page %>% html_nodes("td a") %>% html_attr("href")
# 
# # Get the map pdfs
# pdfList <- list()
# for(i in 1:length(urls)){
#   pdfList[i] <- download.file(urls[i], paste0("Map", i, ".pdf"), mode = "wb")
# }
# 
# # View the images
# txt <- pdf_text("Map7.pdf")

# Use an arbitrary school as a base graph
ArmstrongSchool <- get_map(location = c(lon = -122.9083027 ,lat = 49.2339746), zoom = 14)
plottedSchool <- ggmap(ArmstrongSchool) + geom_point(aes(x = -122.9083027, y = 49.2339746, size = 3, col = "red", alpha = 0.3)) + theme(legend.position = "none")

# Create a polygon bounding the school catchment
  # Will be done heuristically for now
# Finish Coordinates, just did Elwell and Fareham, started at Newcombe and 10th
lon <- c(-122.917433, -122.926703, -122.927647, -122.930436, -122.927475, -122.931520, -122.914654, -122.913066, -122.906414, -122.892510, -122.892682, -122.897660)
lat <- c(49.225133, 49.232405, 49.231957, 49.234297, 49.235838, 49.239243, 49.249329, 49.247760, 49.247087, 49.240587, 49.235656, 49.235124)
polyCoords <- as.data.frame(cbind(lon, lat))

# Update graph with catchment
plottedSchool <- plottedSchool + geom_polygon(data = polyCoords, aes(x = lon, y = lat) , alpha = 0.3, colour = "red", fill = "red") 
  

