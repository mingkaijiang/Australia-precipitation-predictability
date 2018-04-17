Scaling_up_to_half_degree_resolution <- function(inFile, outFile) {
    
    ### Scale up from 0.05 degree to 0.5 degree
    myDF <- read.csv(inFile)
        
    ### mask
    m <- read.ascii.grid("data/Australia_masks/AusMaskt_180326.asc")
    
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
    
    ### convert Australia raster into polygon
    aus.poly <- rasterToPolygons(base.raster, dissolve=T)

    ### prepare the finer resolution data
    f <- read.ascii.grid("data/1980/rain_19800101.grid")
    
    ### Create grid info
    x.list <- seq(f$header$xllcorner, f$header$xllcorner + (0.05 * (f$header$ncols-1)), by=0.05)
    y.list <- seq(f$header$yllcorner, f$header$yllcorner + (0.05 * (f$header$nrows-1)), by=0.05)
    
    nrows = f$header$nrows
    ncols = f$header$ncols
    
    myDF$y <- rep(y.list, each=ncols)
    myDF$x <- rep(x.list, by=nrows)
    
    ### extract data
    subDF1 <- myDF[,c("x", "y", "P")]
    
    ### to inverse latitude 
    subDF1$y <- abs(subDF1$y)
    
    ### spatial conversion
    coordinates(subDF1)=~x+y
    gridded(subDF1) <- T
    data.raster <- raster(subDF1)
    
    ### Cut raster data using Australia mask
    d.raster2 <- mask(data.raster, aus.poly)
    
    ### Spatial aggregate
    a.raster <- aggregate(d.raster2, fact=10, fun=mean, na.rm=T)
    
    ### out of predictability
    out <- rasterToPoints(a.raster)
    out <- as.data.frame(out)
    
    ### extract data
    subDF2 <- myDF[,c("x", "y", "C")]
    
    ### to inverse latitude 
    subDF2$y <- abs(subDF2$y)
    
    ### spatial conversion
    coordinates(subDF2)=~x+y
    gridded(subDF2) <- T
    data.raster <- raster(subDF2)
    
    ### Cut raster data using Australia mask
    d.raster2 <- mask(data.raster, aus.poly)
    
    ### Spatial aggregate
    a.raster <- aggregate(d.raster2, fact=10, fun=mean, na.rm=T)
    
    ### out of c
    out2 <- rasterToPoints(a.raster)
    out2 <- as.data.frame(out2)
    out$C <- out2$C
    
    ### extract data
    subDF3 <- myDF[,c("x", "y", "M")]
    
    ### to inverse latitude 
    subDF3$y <- abs(subDF3$y)
    
    ### spatial conversion
    coordinates(subDF3)=~x+y
    gridded(subDF3) <- T
    data.raster <- raster(subDF3)
    
    ### Cut raster data using Australia mask
    d.raster3 <- mask(data.raster, aus.poly)
    
    ### Spatial aggregate
    a.raster <- aggregate(d.raster3, fact=10, fun=mean, na.rm=T)
    
    ### out of c
    out3 <- rasterToPoints(a.raster)
    out3 <- as.data.frame(out3)
    out$M <- out3$M
    
    ### write output
    write.csv(out, outFile,
              row.names=F)
    
}