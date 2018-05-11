Check_central_Australia <- function() {
    #### Read in 0.05 resolution gridded predictability data
    myDF <- read.csv("output/Australia_rainfall_annual_0.05_resolution.csv")
    
    ### prepare the finer resolution data
    f <- read.ascii.grid("data/1980/rain_19800101.grid")
    
    ### Create grid info
    x.list <- seq(f$header$xllcorner, f$header$xllcorner + (0.05 * (f$header$ncols-1)), by=0.05)
    y.list <- seq(f$header$yllcorner, f$header$yllcorner + (0.05 * (f$header$nrows-1)), by=0.05)
    
    nrows = f$header$nrows
    ncols = f$header$ncols
    
    myDF$y <- rep(y.list, each=ncols)
    myDF$x <- rep(x.list, by=nrows)
    
    ### Extract grids with prec = 0
    test <- subset(myDF, annual_prec <= 50)
    
    ### plot
    require(fields)
    pdf("output/prec_less_than_50mm.pdf")
    quilt.plot(test$x, abs(test$y), test$annual_prec, 
               nx=820, ny=660,  nlevel=10)
    dev.off()
    
}
