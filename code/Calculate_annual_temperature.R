Calculate_annual_temperature <- function(sourceDir, destDir) {
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
    colnames(outDF) <- c("lon", "lat", "mat")
    

    ### merge all data into one dataframe
    outDF$mat <- Reduce("+", lapply(mget(paste0('DF', 1:90)), "[[", c("ann")))
    outDF$mat <- outDF$mat/90
    
    saveRDS(outDF, paste0(destDir, "/Australia_temperature_annual_average.rds"))
    
    ### make a plot
    #### Prepare P data
    pdf(paste0(destDir, "/Australia_temperature_90yr_mean.pdf"))
    
    ### 0.05 resolution
    with(outDF, quilt.plot(lon, lat, mat, nx=400, ny=300,  nlevel=10,
                          xlim=c(110,160), ylim=c(-45, -5),
                          main="MAT (degree C)", add.legend=T))
  
    world(add=T)
    
    dev.off()
    
}