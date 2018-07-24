scale_up_first <- function(sourceDir, destDir) {
    #### Input data in 3-d format of grid, grid, month 
    #### Output data in format of: site ID, year, jan, feb, ...., dec, ann, for each grid
    ####
    
    ### Prepare output storage
    out <- matrix(nrow=70*89, ncol=14) 
    out <- as.data.frame(out)
    colnames(out) <- c("lon", "lat", "jan", "feb", "mar", "apr", "may", "jun",
                       "jul", "aug", "sep", "oct", "nov", "dec")
    
    ### prepare the finer resolution data
    f <- read.ascii.grid("/Volumes/Seagate Backup Plus Drive/Australia_predictability_data/1980/rain_19800101.grid")
    
    ### Create grid info
    x.list <- seq(f$header$xllcorner, f$header$xllcorner + (0.5 * (89-1)), by=0.5)
    y.list <- seq(f$header$yllcorner, f$header$yllcorner + (0.5 * (70-1)), by=0.5)
    
    out$lat <- rep(abs(y.list), each=89)
    out$lon <- rep(x.list, by=70)
    
    ### create destDir if not exists
    if(!dir.exists(destDir)) {
        dir.create(destDir, showWarnings = FALSE)
    }
    
    ### Read in all files in the input directory
    filenames <- list.files(sourceDir, pattern="*.rds", full.names=TRUE)
    
    ### create year list
    yr.list <- c(1980:2009)
    
    for (i in seq_along(filenames)) {
        
        ### Read R database
        myDF <- readRDS(filenames[i])
        
        ### outfile name
        outname <- paste0(destDir, "/scaled_rainfall_DF", yr.list[i], ".csv")
        
        ### Loop through month
        for (j in 1:12) {
            ### Convert into raster
            r <- raster(myDF[,,j])
            
            ### Spatial aggregate
            a <- aggregate(r, fact=10, fun=mean, na.rm=T)
            
            #### Raster to points
            o <- rasterToPoints(a)
            
            ### Assign to output
            out[,2+j] <- o[,3]
        }
    
        out$ann <- rowSums(out[,3:14])
        
        ### Save output
        write.csv(out, outname,
                  row.names=F)
    }
}