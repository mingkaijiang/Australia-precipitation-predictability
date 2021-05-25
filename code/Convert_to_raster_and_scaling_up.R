Convert_to_raster_and_scaling_up <- function(infile) {
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
    
    p1.out <- aggregate(p1, fact=5, fun=mean, na.rm=T)
    p2.out <- aggregate(p2, fact=5, fun=mean, na.rm=T)
    p3.out <- aggregate(p3, fact=5, fun=mean, na.rm=T)
    
    ### Save
    writeRaster(p1.out, "output/predictability_0.5_degree_resolution", format = "GTiff")
    writeRaster(p2.out, "output/constancy_0.5_degree_resolution", format = "GTiff")
    writeRaster(p3.out, "output/contingency_0.5_degree_resolution", format = "GTiff")
    
    
}