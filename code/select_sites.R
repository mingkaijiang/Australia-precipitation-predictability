select_sites <- function(sourceDir, destDir, resp.variable) {
    
    siteDF <- read.csv(paste0(getwd(), "/data/sites_14may.csv"))
    
    myDF <- readRDS(paste0(sourceDir, "/Australia_", resp.variable, "_predictability_biome_decile.rds"))
    

    ### create destDir if not exists
    if(!dir.exists(destDir)) {
        dir.create(destDir, showWarnings = FALSE)
    }
    
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
    
    ### site coordinates
    siteDF2 <- siteDF
    coordinates(siteDF2) <- ~longitude+latitude
    
    p1.ext <- extract(p1, siteDF2)
    p2.ext <- extract(p2, siteDF2)
    p3.ext <- extract(p3, siteDF2)
    
    siteDF$Predictability <- p1.ext
    siteDF$Constancy <- p2.ext
    siteDF$Contingency <- p3.ext
    
    saveRDS(siteDF, paste0(destDir, "/selected_site_", resp.variable, "_preditability.rds"))
    
}