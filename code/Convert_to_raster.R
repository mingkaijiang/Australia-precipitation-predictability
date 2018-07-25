Convert_to_raster <- function(infile) {
    #### Read in 0.1 resolution gridded predictability data
    myDF <- read.csv(infile)
    
    #### Prepare P data
    pDF <- myDF[,c("lon", "lat", "P")]
    cDF <- myDF[,c("lon", "lat", "C")]
    mDF <- myDF[,c("lon", "lat", "M")]
    
    ### library
    library(raster)
    library(sp)
    library(rgdal)
    
    ### coordinates
    coordinates(pDF)=~lon+lat
    coordinates(cDF)=~lon+lat
    coordinates(mDF)=~lon+lat
    
    gridded(pDF) <- T
    gridded(cDF) <- T
    gridded(mDF) <- T
    
    p1 <- raster(pDF)
    p2 <- raster(cDF)
    p3 <- raster(mDF)
    
    ### Save
    writeRaster(p1, "output/predictability_0.1_degree_resolution", format = "GTiff")
    writeRaster(p2, "output/constancy_0.1_degree_resolution", format = "GTiff")
    writeRaster(p3, "output/contingency_0.1_degree_resolution", format = "GTiff")
    
    
}