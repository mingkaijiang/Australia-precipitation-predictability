select_sites_biome <- function(sourceDir, destDir, resp.variable) {
    
    siteDF <- read.csv(paste0(getwd(), "/data/sites_14may.csv"))
    
    myDF.koppen.sub <- readRDS(paste0(sourceDir, "/Australia_", resp.variable, 
                                      "_predictability_biome_koppen_subgroup_decile.rds"))
    
    myDF.koppen.brd <- readRDS(paste0(sourceDir, "/Australia_", resp.variable, 
                                      "_predictability_biome_koppen_broad_group_decile.rds"))
    
    myDF.wwf.group <- readRDS(paste0(sourceDir, "/Australia_", resp.variable, 
                                      "_predictability_biome_wwf_group_decile.rds"))
    

    matDF <- readRDS(paste0(getwd(), "/output/Australia_temperature_annual_average.rds"))
    mapDF <- readRDS(paste0(getwd(), "/output/Australia_rainfall_annual_average.rds"))
    
    
    ### create destDir if not exists
    if(!dir.exists(destDir)) {
        dir.create(destDir, showWarnings = FALSE)
    }
    
    #### Prepare P data
    pDF1 <- myDF.koppen.sub[,c("lon", "lat", "P")]
    pDF2 <- myDF.koppen.brd[,c("lon", "lat", "P")]
    pDF3 <- myDF.wwf.group[,c("lon", "lat", "P")]
    
    cDF1 <- myDF.koppen.sub[,c("lon", "lat", "C")]
    cDF2 <- myDF.koppen.brd[,c("lon", "lat", "C")]
    cDF3 <- myDF.wwf.group[,c("lon", "lat", "C")]
    
    mDF1 <- myDF.koppen.sub[,c("lon", "lat", "M")]
    mDF2 <- myDF.koppen.brd[,c("lon", "lat", "M")]
    mDF3 <- myDF.wwf.group[,c("lon", "lat", "M")]
    
    
    mapDF <- mapDF[,c("lon", "lat", "mean")]
    
    ### coordinates
    coordinates(pDF1)=~lon+lat
    coordinates(cDF1)=~lon+lat
    coordinates(mDF1)=~lon+lat
    
    coordinates(pDF2)=~lon+lat
    coordinates(cDF2)=~lon+lat
    coordinates(mDF2)=~lon+lat
    
    coordinates(pDF3)=~lon+lat
    coordinates(cDF3)=~lon+lat
    coordinates(mDF3)=~lon+lat
    
    coordinates(matDF)=~lon+lat
    coordinates(mapDF)=~lon+lat
    
    gridded(pDF1) <- T
    gridded(cDF1) <- T
    gridded(mDF1) <- T
    
    gridded(pDF2) <- T
    gridded(cDF2) <- T
    gridded(mDF2) <- T
    
    gridded(pDF3) <- T
    gridded(cDF3) <- T
    gridded(mDF3) <- T
    
    gridded(matDF) <- T
    gridded(mapDF) <- T
    
    p1 <- raster(pDF1)
    p2 <- raster(cDF1)
    p3 <- raster(mDF1)
    
    p4 <- raster(pDF2)
    p5 <- raster(cDF2)
    p6 <- raster(mDF2)
    
    p7 <- raster(pDF3)
    p8 <- raster(cDF3)
    p9 <- raster(mDF3)
    
    p10 <- raster(matDF)
    p11 <- raster(mapDF) 
    
    ### site coordinates
    siteDF2 <- siteDF
    coordinates(siteDF2) <- ~longitude+latitude
    
    p1.ext <- extract(p1, siteDF2)
    p2.ext <- extract(p2, siteDF2)
    p3.ext <- extract(p3, siteDF2)
    p4.ext <- extract(p4, siteDF2)
    p5.ext <- extract(p5, siteDF2)
    p6.ext <- extract(p6, siteDF2)
    p7.ext <- extract(p7, siteDF2)
    p8.ext <- extract(p8, siteDF2)
    p9.ext <- extract(p9, siteDF2)
    p10.ext <- extract(p10, siteDF2)
    p11.ext <- extract(p11, siteDF2)
    
    siteDF$Predictability_Koppen_subgroup <- p1.ext
    siteDF$Constancy_Koppen_subgroup <- p2.ext
    siteDF$Contingency_Koppen_subgroup <- p3.ext
    
    siteDF$Predictability_Koppen_broadgroup <- p4.ext
    siteDF$Constancy_Koppen_broadgroup <- p5.ext
    siteDF$Contingency_Koppen_broadgroup <- p6.ext
    
    
    siteDF$Predictability_WWF_group <- p7.ext
    siteDF$Constancy_WWF_group <- p8.ext
    siteDF$Contingency_WWF_group <- p9.ext
    
    
    siteDF$MAT <- p10.ext
    siteDF$MAP <- p11.ext
    
    saveRDS(siteDF, paste0(destDir, "/selected_site_", resp.variable, "_preditability.rds"))
    
}