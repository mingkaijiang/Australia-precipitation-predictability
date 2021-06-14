Read_biome_grids_koppen <- function() {
    
    ### gridDF - this is the precipitation grid df
    gridDF <- readRDS("scaled_rainfall_data/rainfall_monthly_DF1930.rds")
    gridDF$lat <- gridDF$lat
    gridDF$id <- 1:length(gridDF$lat)
    subDF <- gridDF[,c("lon", "lat", "id")]
    
    r1 <- rasterFromXYZ(subDF)
    
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
    
    r2 <- raster(kpnDF)
    ext <- extent(xmn, xmx, ymn, ymx)
    extent(r2) <- ext
    
    ### extract values according to r1 from r2
    out <- resample(r2, r1, method="ngb")
    # alternative
    #biom.value <- extract(r2, cbind(gridDF$lon, gridDF$lat))
    
    biom.value <- extract(out, cbind(gridDF$lon, gridDF$lat))
    subDF$Biome <- biom.value
    
    ### biome values are:
    
    #Grid-point indices on the classification grid are as follows: 
    #  Equatorial 
    #42 rainforest (monsoonal) 
    #41 savanna 
    #Tropical 
    #37 rainforest (persistently wet) 
    #36 rainforest (monsoonal) 
    #35 savanna 
    #Subtropical 
    #34 no dry season 
    #33 distinctly dry summer 
    #32 distinctly dry winter 
    #31 moderately dry winter 
    #Desert 
    #24 hot (persistently dry) 
    #23 hot (summer drought) 
    #22 hot (winter drought) 
    #21 warm (persistently dry) 
    #Grassland 
    #15 hot (persistently dry) 
    #14 hot (summer drought) 
    #13 hot (winter drought) 
    #12 warm (persistently dry) 
    #11 warm (summer drought) 
    #Temperate 
    #9 no dry season (hot summer) 
    #8 moderately dry winter (hot summer) 
    #7 distinctly dry (and hot) summer 
    #6 no dry season (warm summer) 
    #5 moderately dry winter (warm summer) 
    #4 distinctly dry (and warm) summer 
    #3 no dry season (mild summer) 
    #2 distinctly dry (and mild) summer 
    #1 no dry season (cool summer) 
    
    subDF$Biome2 <- ifelse(subDF$Biome > 40, "Equatorial",
                           ifelse(subDF$Biome < 40 & subDF$Biome > 34, "Tropical",
                                  ifelse(subDF$Biome < 35 & subDF$Biome > 30, "Subtropical",
                                         ifelse(subDF$Biome < 30 & subDF$Biome > 20, "Desert",
                                                ifelse(subDF$Biome < 20 & subDF$Biome > 10, "Grassland",
                                                       ifelse(subDF$Biome < 10, "Temperate", NA))))))
    
    col.pal <- viridis_pal()(6)
    
    p1 <- ggplot(subDF)+
      geom_tile(aes(lon, lat, fill=Biome2))+
      coord_quickmap(xlim=range(subDF$lon), ylim=range(subDF$lat))+
      scale_fill_manual(name = "Koppen biome", 
                        values=col.pal) +
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.title.x = element_text(size=14), 
            axis.text.x = element_text(size=12),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=10),
            legend.title=element_text(size=12),
            panel.grid.major=element_blank(),
            legend.position = "right",
            plot.title = element_text(size = 10, face = "bold"))+
      borders(col=alpha("black", 0.8), lwd=0.1)+
      guides(fill = guide_legend(ncol = 1, byrow = TRUE))
      
    
    pdf(paste0(getwd(), "/output/biomes_boundary_koppen.pdf"))
    plot(p1)
    dev.off()
    
    
    
    return(subDF)
}
