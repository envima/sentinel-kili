#---
#title: "Extraction of vegetation indices from Sentinel 2A scenes for a particular study area considering cloud coverage"
#authors: "Dirk Zeuss, Fortunata Msoffe"
#date: "1 September 2020"
#---


## Praeambel ---------------------------------------------------------------

rm(list = ls(all = TRUE)) # clean workspace

library("raster")
library("RStoolbox")
library("rgdal")

# Set working directory
wd <- "/home/dirk/PowerFolders/sentinel_processing"
setwd(wd)


## Choose sentinel tile name -------------------------------------------------------------

# Set sentinel tiles to process (all scenes matching the string below in the sentinel data folder will be processed)
sentinel_tile <- "T37MBS" 


# Set study area -------------------------------------------------------

studyarea <- readOGR("studyarea/kinapahalfmile.shp") # Vector polygon defining the study area within the sentinel tile set above
# plot(studyarea)


## Set and get paths ---------------------------------------------------------------

# Set path to the directory with sentinel scenes
path_sentinel_data <- "/home/dirk/data/sentinel/kili"

# Get paths to folders of sentinel scenes
paths_sentinel_tiles <- list.files(path_sentinel_data, pattern = sentinel_tile, full.names = TRUE, recursive = FALSE)
# length(paths_sentinel_tiles) # number of scenes matching the tile name set above in the data folder

paths_sentinel_tiles <- grep(".SAFE", paths_sentinel_tiles, value = TRUE) # remove potential non-sentinel paths in the data folder 


## Batch processing -----------------------------------------------------
# Loop over all scenes in the sentinel data folder to calculate vegetation indices and cloud coverage for the study area

result <- list() # create empty list for collecting the results later

for(i in paths_sentinel_tiles)try({ # iterate over all sentinel scene folders in the sentinel data folder
  
  # Show some output on the status of the batch processing
  cat("Processing:", basename(i), "\n")
  
  # Select bands  -------------------------------------------------------
  
  # Set the search pattern for bands to process. In this case only the 10m bands are selected.
  searchPattern_10m <- "_10m.jp2$" 

  # Get paths with images containing "searchPattern_10m"
  paths_bands_10m <- list.files(file.path(i, "GRANULE"), pattern = searchPattern_10m, full.names = TRUE, recursive = TRUE)
  
  # Create stack of bands
  bands_10m <- stack(paths_bands_10m)
  
  # Rename bands to band name and resolution (only for long names)
  regs_matched <- gregexpr("_..._.*$", names(bands_10m)) 
  tempname <- as.character(regmatches(names(bands_10m), regs_matched))
  names(bands_10m) <- substr(tempname, 2, nchar(tempname))
  
  # Reproject, crop and clip  -------------------------------------------------------
  
  # Reproject study area to Sentinel CRS
  studyarea <-  spTransform(studyarea, projection(bands_10m))
  
  # Crop and clip Sentinel tile with study area polygon
  bands_10m <- crop(bands_10m, studyarea)
  bands_10m <- mask(bands_10m, studyarea)


  # Index calculation -------------------------------------------------------
  
  # Choose indices for calculation
  # indices_to_process <- c("RVI", "NDVI") # Manually choose indices here. If you like to process indices which need the 20m resolution bands, these bands must be added to the search pattern above.
  indices_to_process <- NULL # "NULL" in this case means the calculation of all available indices in RStoolbox::spectralIndices()
  
  
  # Create layer of indices
  Index_layer <- RStoolbox::spectralIndices(bands_10m, # see ?RStoolbox::spectralIndices for available indices and the required bands
                                            blue = "B02_10m", # add additional bands here if they are required for other vegetation indices
                                            green = "B03_10m",
                                            red = "B04_10m",
                                            nir = "B08_10m",
                                            indices = indices_to_process,
                                            scaleFactor = 1,
                                            skipRefCheck = TRUE) # see ?RStoolbox::spectralIndices for details. 
  
  # Create raster stack of index layers
  Index_layer <- stack(Index_layer) 
  
  # Calculate summary statistics for each index layer
  indices_res <- sapply(names(Index_layer), function (x) cellStats(Index_layer[[x]], stat = "mean")) # Note that the mean vegetation index over the whole study area is calculated here. Change, if you need other summary statistics.
  
  
  # Cloud coverage in study area   -------------------------------------------------------
  
  # Get path to cloud mask
  path_cloud_mask <- list.files(file.path(i, "GRANULE"), pattern = "MSK_CLOUDS_B00.gml", full.names = TRUE, recursive = TRUE)
  
  # Import cloud mask
  cloud_mask <- readOGR(path_cloud_mask, disambiguateFIDs = TRUE, verbose = FALSE)
  
  # Mask clouds in study area for coverage calculation
  clouds <- mask(bands_10m[[1]], cloud_mask, updatevalue = NA)
  
  # Calculate cloud coverage
  all_pixels_study_area <- length(na.omit(raster::values(bands_10m[[1]]))) # number of all pixels in the study area
  cloud_pixels_study_area <- length(na.omit(clouds@data@values)) # number of cloud pixels in the study area
  
  cloud_coverage <- cloud_pixels_study_area / all_pixels_study_area
  
  names(cloud_coverage) <- "cloud_coverage"
  
  
  # Cloud coverage in full scene   -------------------------------------------------------
  
  # Get path to metadata
  path_overall_cloud_coverage <- list.files(file.path(i, "HTML"), pattern = "UserProduct_index.html", full.names = TRUE, recursive = TRUE)
  
  # Read in metadata
  metadata <- readChar(path_overall_cloud_coverage, file.info(path_overall_cloud_coverage)$size)
  
  # Extract cloud coverage of the full scene
  search_pattern <- ".*(Cloud Coverage Assessment: )"
  temp_string <- sub(search_pattern, "", metadata) 
  cloud_cover_full_scene <- as.numeric(substr(temp_string, 1,5)) / 100
  
  
  # Visual checks  -------------------------------------------------------
  
  # Visually check cloud coverage calculation
  # graphics.off()
  # plotRGB(bands_10m, r = 4, g = 3, b = 2, stretch = "lin")
  # plot(studyarea, add = TRUE)
  # plot(cloud_mask, add=TRUE)
  # plot(clouds, add=TRUE, col="red")
  
  # Visually check index layer
  # plot(Index_layer)
  # title(main=i)
  
  # Write out visual checks to disk
  dir.create(file.path(wd, "output/graphics/indices"), recursive = T, showWarnings = FALSE) 
  dir.create(file.path(wd, "output/graphics/cloud_coverage"), recursive = T, showWarnings = FALSE) 
  
  jpeg(filename = paste0(file.path(wd, "output/graphics/indices", basename(i)), ".jpg"), 
       width = 1600, height = 1400, pointsize = 24)
  plot(Index_layer)
  dev.off()
  
  jpeg(filename = paste0(file.path(wd, "output/graphics/cloud_coverage", basename(i)), ".jpg"), width = 800, height = 600)
  plotRGB(bands_10m, r = 4, g = 3, b = 2, stretch = "lin")
  plot(studyarea, add = TRUE)
  plot(cloud_mask, add=TRUE)
  plot(clouds, add=TRUE, col="red")
  dev.off()
  
  
  # Collect results  -------------------------------------------------------
  
  # Show some output on the status of the batch processing
  cat(names(indices_res), ":", indices_res, ",", 
      "cloud coverage:", cloud_coverage,  ",", 
      "cloud coverage full scene:", cloud_cover_full_scene,  
      "\n")
  
  # Concatenate index results and corresponding Sentinel scene
  result[[basename(i)]] <- c(indices_res, cloud_coverage, cloud_cover_full_scene = cloud_cover_full_scene)
  
  # Clean up
  #rm(cloud_mask, clouds, bands_10m); gc() # if you run into memory issues, try this line.
  
})


# Postprocessing ----------------------------------------------------------

# Create a data.frame ("table") for the results
result_df <- data.frame(Tile = names(result),
                        Date = as.Date(substr(names(result), 12, 19), format = "%Y%m%d"),
                        sapply(names(indices_res), function (x) sapply(result, function (y) y[x])),
                        cloud_coverage = sapply(result, function (x) x["cloud_coverage"]),
                        cloud_cover_full_scene = sapply(result, function (x) x["cloud_cover_full_scene"]),
                        row.names = NULL)

# Order results by date of the processed sentinel scenes
result_df <- result_df[order(result_df$Date),]
result_df


# Save results as csv file in the "output" directory  ------------------------------------------------------------

# Creates the folder "output" in your working directory, if it does not already exist.
dir.create(file.path(wd, "output"), showWarnings = FALSE) 

# Set name of the output result file with number of scenes processed, indices calculated and processing date
name_output_file <- paste("results_", 
                           sentinel_tile, "_",
                           length(paths_sentinel_tiles), 
                           "scenes_", 
                           collapse="_",
                           Sys.Date(),
                           sep = "")
# Write to file
write.csv(result_df, file = paste0(file.path(wd, "output", name_output_file), ".csv"), row.names = FALSE)


## Plot change in indices over time ------------------------------------------------------------

pdf(file = paste0(file.path(wd, "output", name_output_file), ".pdf"), width = 20, height = 12, pointsize=18)
par(mfrow=c(4,5))
for (j in names(indices_res)){
  plot(result_df[,j] ~ result_df$Date, type = "b", xlab = "", ylab = j, main = j)}
dev.off()

## End  --------------------------------------------------------------------------------


