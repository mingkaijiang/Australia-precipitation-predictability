Convert_to_raster <- function(infile) {
    #### Read in 0.1 resolution gridded predictability data
    myDF <- readRDS(infile)
    
    #### Prepare P data
    pDF <- myDF[,c("lon", "lat", "P")]
    cDF <- myDF[,c("lon", "lat", "C")]
    mDF <- myDF[,c("lon", "lat", "M")]

    
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
    writeRaster(p1, "output/predictability_", format = "GTiff")
    writeRaster(p2, "output/constancy_", format = "GTiff")
    writeRaster(p3, "output/contingency_", format = "GTiff")
    
    
}