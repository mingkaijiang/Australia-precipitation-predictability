Calculate_annual_precipitation <- function(sourceDir, destDir) {
    #### Input data from csv file

    ### create destDir if not exists
    if(!dir.exists(destDir)) {
        dir.create(destDir, showWarnings = FALSE)
    }
    
    ### Read in all files in the input directory
    filenames <- list.files(sourceDir, pattern="*.rds", full.names=TRUE)
    
    for (i in seq_along(filenames)) {
        assign(paste0("DF", i), readRDS(filenames[i]))
    }
    
    ### Prepare output dataframe
    outDF <- DF1[,1:3]
    colnames(outDF) <- c("lon", "lat", "sum")
    
    
    
    ### merge all data into one dataframe
    outDF$sum <- Reduce("+", lapply(mget(paste0('DF', 1:90)), "[[", c("ann")))
    outDF$mean <- outDF$sum/90
    
    ### add biome information
    bDF <- Read_biome_grids_koppen(plot.decision=F)
    
    ### merge
    outDF <- merge(outDF, bDF, by=c("lon", "lat"))
    
    ### make ocean the NAs
    outDF$sum <- ifelse(is.na(outDF$BiomeWWF), NA, outDF$sum)
    outDF$mean <- ifelse(is.na(outDF$BiomeWWF), NA, outDF$mean)
    
    
    saveRDS(outDF, paste0(destDir, "/Australia_rainfall_annual_average.rds"))
    
    
    ### make a plot
    #### Prepare P data
    pdf(paste0(destDir, "/Australia_rainfall_90yr_mean.pdf"))
    
    ### 0.05 resolution
    with(outDF, quilt.plot(lon, lat, mean, nx=400, ny=300,  nlevel=10,
                          xlim=c(110,160), ylim=c(-45, -5),
                          main="MAP (mm)", add.legend=T))
    
  
    world(add=T)
    
    dev.off()
    
}