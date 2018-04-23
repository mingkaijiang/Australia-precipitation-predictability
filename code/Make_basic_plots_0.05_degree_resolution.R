Make_basic_plots_0.05_degree_resolution <- function() {
    

    #### Read in 0.05 resolution gridded predictability data
    myDF <- read.csv("output/Australia_rainfall_predictability_0.05_resolution.csv")
    
    #### Read in 0.05 resolution gridded precipitation data
    precDF <- read.csv("output/Australia_rainfall_annual_0.05_resolution.csv")
    
    #### Read in mask
    m <- read.ascii.grid("data/Australia_masks/AusMaskt_180326.asc")
    x.list <- seq(m$header$xllcorner, m$header$xllcorner + (0.5 * (m$header$ncols-1)), by=0.5)
    y.list <- abs(seq(m$header$yllcorner, m$header$yllcorner + (0.5 * (m$header$nrows-1)), by=0.5))
    
    base.raster <- raster(m$data, xmn=min(x.list), ymn=min(y.list),
                          xmx=max(x.list), ymx=max(y.list))
    
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
    
    precDF$y <- rep(y.list, each=ncols)
    precDF$x <- rep(x.list, by=nrows)
    
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
    p.raster <- aggregate(d.raster2, fact=10, fun=mean, na.rm=T)
    
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
    c.raster <- aggregate(d.raster2, fact=10, fun=mean, na.rm=T)
    
    
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
    m.raster <- aggregate(d.raster3, fact=10, fun=mean, na.rm=T)
    

    ### extract data
    subDF4 <- precDF[,c("x", "y", "annual_prec")]
    
    ### to inverse latitude 
    subDF4$y <- abs(subDF4$y)
    
    ### spatial conversion
    coordinates(subDF4)=~x+y
    gridded(subDF4) <- T
    data.raster <- raster(subDF4)
    
    ### Cut raster data using Australia mask
    d.raster4 <- mask(data.raster, aus.poly)
    
    ### Spatial aggregate
    prec.raster <- aggregate(d.raster4, fact=10, fun=mean, na.rm=T)
    
    
    
    #### Prepare P data
    pdf("output/basic_plots_0.05_degree_resolution.pdf")
    
    ### 0.5 degree P
    plot(p.raster)
    
    ### 0.5 degree C
    with(myDF, quilt.plot(x, y, C, nx=82, ny=66,  nlevel=100,
                          xlim=c(110,160), ylim=c(5,45),
                          main="Constancy", add.legend=T))
    
    ### 0.5 degree M
    with(myDF, quilt.plot(x, y, M, nx=82, ny=66,  nlevel=100,
                          xlim=c(110,160), ylim=c(5,45),
                          main="Contingency", add.legend=T))
    
    dev.off()
    
}