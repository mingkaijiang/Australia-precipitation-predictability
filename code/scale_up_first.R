scale_up_first <- function(sourceDir, destDir) {
    #### Input data in 3-d format of grid, grid, month 
    #### Output data in format of: site ID, year, jan, feb, ...., dec, ann, for each grid
    ####
    
    ### Prepare output storage
    #out <- matrix(nrow=70*89, ncol=14) 
    out <- matrix(nrow=691*886, ncol=14) 
    out <- as.data.frame(out)
    colnames(out) <- c("lon", "lat", "jan", "feb", "mar", "apr", "may", "jun",
                       "jul", "aug", "sep", "oct", "nov", "dec")
    
    ### prepare the finer resolution data
    f <- read.ascii.grid(paste0(getwd(), "/data/AWAP/rain_19500101.grid"))
    
    ### Create grid info
    #x.list <- seq(f$header$xllcorner, f$header$xllcorner + (0.1 * 442), by=0.1)
    #y.list <- seq(f$header$yllcorner, f$header$yllcorner + (0.1 * 345), by=0.1)
    #
    #out$lat <- rep(abs(y.list), each=443)
    #out$lon <- rep(x.list, by=346)
    
    x.list <- seq(f$header$xllcorner, f$header$xllcorner + (0.05 * 885), by=0.05)
    y.list <- seq(f$header$yllcorner, f$header$yllcorner + (0.05 * 690), by=0.05)
    
    out$lat <- rep(abs(y.list), each=886)
    out$lon <- rep(x.list, by=691)
    
    
    ### create destDir if not exists
    if(!dir.exists(destDir)) {
        dir.create(destDir, showWarnings = FALSE)
    }
    
    ### Read in all files in the input directory
    filenames <- list.files(sourceDir, pattern="*.rds", full.names=TRUE)
    
    ### create year list
    yr.list <- c(1930:1950)
    
    for (i in seq_along(filenames)) {
        
        ### Read R database
        myDF <- readRDS(filenames[i])
        
        ### outfile name
        outname <- paste0(destDir, "/rainfall_monthly_DF", yr.list[i], ".rds")
        
        ### Loop through month
        for (j in 1:12) {
            ### Convert into raster
            r <- raster(myDF[,,j])
            
            ### Spatial aggregate
            a <- r # aggregate(r, fact=2, fun=mean, na.rm=T)
            
            #### Raster to points
            o <- rasterToPoints(a)
            
            ### Assign to output
            out[,2+j] <- o[,3]
        }
    
        out$ann <- rowSums(out[,3:14])
        
        ### Save output
        saveRDS(out, outname)
    }
    
    ## end
}