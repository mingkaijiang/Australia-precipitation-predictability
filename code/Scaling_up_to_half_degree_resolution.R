Scaling_up_to_half_degree_resolution <- function(inFile, outFile) {
    
    ### Scale up from 0.05 degree to 0.5 degree
    myDF <- read.csv(inFile)
        
    ### mask
    m <- read.ascii.grid("~/Documents/Research/Projects/Australia_precipitation_predictability/Git/data/Australia_masks/AusMaskt_180326.asc")
    
    ### Assign values onto the mask, based on averages
    # plot(m$data)
    
    ### make a [x,y] long format DF to store m
    lDF <- matrix(nrow=m$header$ncols * m$header$nrows, ncol=3)
    lDF <- as.data.frame(lDF)
    colnames(lDF) <- c("x", "y", "v1")
    
    ### Create grid info
    x.list <- seq(m$header$xllcorner, m$header$xllcorner + (0.5 * (m$header$ncols-1)), by=0.5)
    y.list <- seq(m$header$yllcorner, m$header$yllcorner + (0.5 * (m$header$nrows-1)), by=0.5)
    
    ### assign grid info onto lDF
    lDF$x <- rep(x.list, by=m$header$nrows)
    lDF$y <- rep(y.list, each=m$header$ncols)
    
    ### assign original value
    for (i in 1:length(x.list)) {
        for (j in 1:length(y.list)) {
            lDF[lDF$x == x.list[i] & lDF$y == y.list[j], "v1"] <- m$data[j,i]
        }
    }
    
    lDF$v1 <- ifelse(is.na(lDF$v1), NA, 1)
    
    ### to inverse latitude 
    lDF$y <- abs(lDF$y)
    
    ### spatial conversion
    coordinates(lDF)=~x+y
    gridded(lDF) <- T
    base.raster <- raster(lDF)

    ### prepare the finer resolution data
    f <- read.ascii.grid("~/Documents/Research/Projects/Australia_precipitation_predictability/Git/data/1980/rain_19800101.grid")
    
    ### Create grid info
    x.list <- seq(f$header$xllcorner, f$header$xllcorner + (0.05 * (f$header$ncols-1)), by=0.05)
    y.list <- seq(f$header$yllcorner, f$header$yllcorner + (0.05 * (f$header$nrows-1)), by=0.05)
    
    nrows = f$header$nrows
    ncols = f$header$ncols
    
    myDF$y <- rep(y.list, each=ncols)
    myDF$x <- rep(x.list, by=nrows)
    
    ### extract data
    subDF <- myDF[,c("x", "y", "Site_ID")]
    
    ### to inverse latitude 
    subDF$y <- abs(subDF$y)
    
    ### spatial conversion
    coordinates(subDF)=~x+y
    gridded(subDF) <- T
    data.raster <- raster(subDF)
    
    ### convert Australia raster into polygon
    aus.poly <- rasterToPolygons(base.raster, dissolve=T)
    
    ### Cut raster data using Australia mask
    d.raster2 <- mask(data.raster, aus.poly)
    
    ### Spatial aggregate
    a.raster <- aggregate(d.raster2, fact=10, fun=mean, na.rm=T)
    
    ### out
    out <- rasterToPoints(a.raster)
    
    ### write output
    wirte.csv(out, paste0(destDir, "/Australia_rainfall_predictability_0.5_resolution.csv"),
              row.names=F, col.names=T)
    
}