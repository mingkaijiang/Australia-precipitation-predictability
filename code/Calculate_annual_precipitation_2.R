Calculate_annual_precipitation_2 <- function(sourceDir, destDir) {
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
    
    
    saveRDS(outDF, paste0(destDir, "/Australia_rainfall_annual_average.rds"))
    
}