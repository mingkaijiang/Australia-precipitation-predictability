select_sites_fixed_bin <- function(sourceDir, destDir, resp.variable) {
    
    siteDF <- read.csv(paste0(getwd(), "/data/sites_14may.csv"))
    
    myDF <- readRDS(paste0(sourceDir, "/Australia_", resp.variable, "_predictability_fixed_bin.rds"))
    

    matDF <- readRDS(paste0(getwd(), "/output/Australia_temperature_annual_average.rds"))
    mapDF <- readRDS(paste0(getwd(), "/output/Australia_rainfall_annual_average.rds"))
    
    
    ### create destDir if not exists
    if(!dir.exists(destDir)) {
        dir.create(destDir, showWarnings = FALSE)
    }
    
    #### Prepare P data
    pDF <- myDF[,c("lon", "lat", "P")]
    cDF <- myDF[,c("lon", "lat", "C")]
    mDF <- myDF[,c("lon", "lat", "M")]
    
    mapDF <- mapDF[,c("lon", "lat", "mean")]
    
    ### coordinates
    coordinates(pDF)=~lon+lat
    coordinates(cDF)=~lon+lat
    coordinates(mDF)=~lon+lat
    
    coordinates(matDF)=~lon+lat
    coordinates(mapDF)=~lon+lat
    
    gridded(pDF) <- T
    gridded(cDF) <- T
    gridded(mDF) <- T
    
    gridded(matDF) <- T
    gridded(mapDF) <- T
    
    p1 <- raster(pDF)
    p2 <- raster(cDF)
    p3 <- raster(mDF)
    
    p4 <- raster(matDF)
    p5 <- raster(mapDF) 
    
    ### site coordinates
    siteDF2 <- siteDF
    coordinates(siteDF2) <- ~longitude+latitude
    
    p1.ext <- extract(p1, siteDF2)
    p2.ext <- extract(p2, siteDF2)
    p3.ext <- extract(p3, siteDF2)
    p4.ext <- extract(p4, siteDF2)
    p5.ext <- extract(p5, siteDF2)
    
    siteDF$Predictability <- p1.ext
    siteDF$Constancy <- p2.ext
    siteDF$Contingency <- p3.ext
    siteDF$MAT <- p4.ext
    siteDF$MAP <- p5.ext
    
    saveRDS(siteDF, paste0(destDir, "/selected_site_", resp.variable, "_fixed_bin_preditability.rds"))
    
}