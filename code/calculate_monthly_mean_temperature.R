calculate_monthly_mean_temperature <- function(sourceDir1, sourceDir2, destDir) {
    #### Input data in 3-d format of grid, grid, month 
    #### Output data in format of: site ID, year, jan, feb, ...., dec, ann, for each grid
    ####
    
    ### Prepare output storage
    out <- matrix(nrow=691*886, ncol=14) 
    out <- as.data.frame(out)
    colnames(out) <- c("lon", "lat", "jan", "feb", "mar", "apr", "may", "jun",
                       "jul", "aug", "sep", "oct", "nov", "dec")
    
    ### prepare the finer resolution data
    f <- read.ascii.grid(paste0(getwd(), "/data/rain_19500101.grid"))
    
    ### Create grid info
    x.list <- seq(f$header$xllcorner, f$header$xllcorner + (0.05 * 885), by=0.05)
    y.list <- seq(f$header$yllcorner, f$header$yllcorner + (0.05 * 690), by=0.05)
    
    out$lat <- rep(rev(y.list), each=886)
    out$lon <- rep(x.list, by=691)
    
    
    ### create destDir if not exists
    if(!dir.exists(destDir)) {
        dir.create(destDir, showWarnings = FALSE)
    }
    
    ### Read in all files in the input directory
    filenames <- list.files(sourceDir1, pattern="*.rds", full.names=TRUE)
    
    ### create year list
    yr.list <- c(1930:2019)
    
    for (i in yr.list) {
        
        ### Read R database
        tmax.files <- paste0(sourceDir1, "/DF", i, ".rds")
        tmin.files <- paste0(sourceDir2, "/DF", i, ".rds")
        
        myDF1 <- readRDS(tmax.files)
        myDF2 <- readRDS(tmin.files)
        
        mylist <- list(myDF1, myDF2)
        
        myDF <- Reduce("+", mylist) / length(mylist)
        
        ### outfile name
        outname <- paste0(destDir, "/DF", i, ".rds")
        
        ### Loop through month
        for (j in 1:12) {
            ### Convert into raster
            r <- raster(myDF[,,j])
            
            #### Raster to points
            o <- rasterToPoints(r)
            
            ### Assign to output
            out[,2+j] <- o[,3]
        }
    
        out$ann <- rowSums(out[,3:14])
        
        ### Save output
        saveRDS(out, outname)
    }
    
    ## end
}