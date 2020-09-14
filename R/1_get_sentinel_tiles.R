#---
#title: "Download sentinel scenes with sen2R for a particular study area and time period"
#authors: "Dirk Zeuss, Fortunata Msoffe"
#date: "1 September 2020"
#---


# Praeambel ---------------------------------------------------------------

rm(list = ls()) # clean workspace

# Load packages
library("sen2r")
library("rgdal")
library("sf")

# sen2r() # Use the gui to enter your scihub credentials, which will then be saved in the package.

# Set working directory
wd <- "/home/dirk/PowerFolders/sentinel_processing"
setwd(wd)

# Set data directory for sentinel scenes
path_sentinel_data <- "/home/dirk/data/sentinel/kili"

# Set study area for querying available Sentinel scenes
studyarea <- readOGR("studyarea/kinapahalfmile.shp") # Vector polygon defining the study area within the sentinel tile set above. Check, if the projection is defined and the same as the sentinel tiles.


## Download  -----------------------------------------------------------

# Get list of available Sentinel scenes
sen_list <- s2_list(spatial_extent = sf::st_as_sf(studyarea), # could be omitted if the tile name is known
                    tile = "37MBS", # choose sentinel tile name. Be careful here to provide only five characters.
                    time_interval = as.Date(c("2017-01-01", "2020-09-01")), # choose time interval
                    level = "L2A",
                    max_cloud = 100) # get all scenes independent of overall cloud coverage

# Download scenes
s2_download(s2_prodlist = sen_list,
            downloader = "builtin",
            apihub = NA,
            service = NA,
            outdir = path_sentinel_data,
            order_lta = TRUE,
            overwrite = FALSE)

## END  --------------------------------------------------------------------------------
