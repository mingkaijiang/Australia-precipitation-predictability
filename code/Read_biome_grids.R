Read_biome_grids <- function() {
    ### gridDF - this is the precipitation grid df
    gridDF <- readRDS("scaled_rainfall_data/rainfall_monthly_DF1930.rds")
    gridDF$lat <- gridDF$lat
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
    y.list <- rev(seq(ymn, ymx, by=0.025))
    
    r <- raster(kpnDF)
    ext <- extent(xmn, xmx, ymn, ymx)
    extent(r) <- ext
    
    pdf(paste0(getwd(), "/output/biomes_boundary.pdf"))
    plot(r)
    world(add=T)
    dev.off()

    
    ### extract data according to gridDF lon and lat info
    biom.value <- extract(r, cbind(gridDF$lon, gridDF$lat))
    subDF$Biome <- biom.value
    
    subDF$Biome[subDF$Biome%in%c(0, 2, 42)] <- NA
    
    
    ### prepare plot DF
    plotDF <- subDF[,c("lon", "lat", "Biome")]
    plotDF <- plotDF[!is.na(plotDF$Biome),]
    plotDF <- plotDF[plotDF$Biome > 0 & plotDF$Biome < 90, ]
    plotDF <- plotDF[plotDF$Biome != 2, ]
    plotDF <- plotDF[plotDF$Biome < 42, ]
    
    with(plotDF, quilt.plot(lon, lat, Biome, nx=400, ny=300,  nlevel=28,
                          xlim=c(110,160), ylim=c(-45,-5),
                          main="Predictability", add.legend=T))
    world(add=T)
    
    return(subDF[,c("lon", "lat", "Biome")])
}
