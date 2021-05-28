scale_up_first <- function(sourceDir, destDir) {
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
    
    ext <- extent(min(x.list), max(x.list), min(y.list), max(y.list))
    
    ### create destDir if not exists
    if(!dir.exists(destDir)) {
        dir.create(destDir, showWarnings = FALSE)
    }
    
    ### australia boundary - resolution too coarse
    #ausDF <- readOGR(dsn=paste0(getwd(), 
    #                        "/data/australia_administrative_boundaries_national_polygon/"),
    #                 layer="australia_administrative_boundaries_national_polygon")

    #ausDF2 <- ausDF[ausDF$way_area>=1.065e+13,]

    ### Read in all files in the input directory
    filenames <- list.files(sourceDir, pattern="*.rds", full.names=TRUE)
    
    ### create year list
    yr.list <- c(1930:2019)
    
    for (i in seq_along(filenames)) {
        
        ### Read R database
        myDF <- readRDS(filenames[i])
        
        ### outfile name
        outname <- paste0(destDir, "/rainfall_monthly_DF", yr.list[i], ".rds")
        
        ### Loop through month
        for (j in 1:12) {
            ### Convert into raster
            r <- raster(myDF[,,j])
            
            extent(r) <- ext
            
            ### Spatial aggregate
            #a <- mask(r, ausDF2)
            #plot(a)
            #plot(ausDF2,add=T)
            
            #### Raster to points
            o <- rasterToPoints(r)
            
            ### Assign to output
            out[,2+j] <- o[,3]
        }
    
        out$ann <- rowSums(out[,3:14])
        
        ## check results
        #with(out, quilt.plot(lon, lat, ann, nx=400, ny=300, nlevel=100,
        #                        xlim=c(110,160), ylim=c(-45,-5),
        #                        main="MAP (mm)", add.legend=T))
        #world(add=T)
        
        ### Save output
        saveRDS(out, outname)
    }
    
    ## end
}