Read_biome_grids <- function() {
    ### gridDF - this is the precipitation grid df
    gridDF <- readRDS("scaled_rainfall_data/rainfall_monthly_DF1930.rds")
    gridDF$lat <- -gridDF$lat
    gridDF$id <- 1:length(gridDF$lat)
    subDF <- gridDF[,c("lon", "lat", "id")]
    
    ### Read in biome df
    kpnDF <- read.matrix("data/kpngrp/kpngrp.txt", skip=6)
    kpnDF[kpnDF<0] <- NA
    
    test <- as.vector(kpnDF)

    ### biome df information
    #ncols         1681
    #nrows         1361
    #xllcorner     112
    #yllcorner     -44
    #cellsize      0.025
    xmn <- 112
    xmx <- 112 + 0.025*(1681-1)
    ymn <- -44
    ymx <- -44 + 0.025*(1361-1)
    x.list <- seq(xmn, xmx, by=0.025)
    y.list <- seq(ymn, ymx, by=0.025)
    
    r <- raster(kpnDF)
    ext <- extent(xmn, xmx, ymn, ymx)
    extent(r) <- ext
    #plot(r)
    #world(add=T)
    
    ### extract data according to gridDF lon and lat info
    biom.value <- extract(r, cbind(gridDF$lon, gridDF$lat))
    subDF$Biome <- biom.value
    #plotDF <- subDF[,c("lon", "lat", "Biome")]
    #library(sp)
    #library(rgdal)
    #coordinates(plotDF)=~lon+lat
    #gridded(plotDF) <- T
    #p <- raster(plotDF)
    #plot(p)
    #world(add=T)
    
    return(subDF[,c("lon", "lat", "Biome")])
}
