Read_biome_grids_koppen <- function(plot.decision) {
    
    ### gridDF - this is the precipitation grid df
    gridDF <- readRDS("scaled_rainfall_data/rainfall_monthly_DF1930.rds")
    gridDF$lat <- gridDF$lat
    gridDF$id <- 1:length(gridDF$lat)
    subDF <- gridDF[,c("lon", "lat", "id")]
    
    r1 <- rasterFromXYZ(subDF)
    
    ### biome dataset, based on koppen scheme
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
    kpnDF <- read.matrix("data/kpngrp/kpngrp.txt", skip=6)
    kpnDF[kpnDF<0] <- NA
    
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
    
    biom.value <- extract(out, cbind(gridDF$lon, gridDF$lat))
    subDF$BiomeKoppen <- biom.value
    
    ### overall biome bin
    subDF$BiomeKoppenBroad <- ifelse(subDF$BiomeKoppen%in%c(41,42), "Equatorial",
                                     ifelse(subDF$BiomeKoppen%in%c(35,36,37), "Tropical",
                                            ifelse(subDF$BiomeKoppen%in%c(31:34), "Subtropical",
                                                   ifelse(subDF$BiomeKoppen%in%c(21:24), "Desert",
                                                          ifelse(subDF$BiomeKoppen%in%c(11:15), "Grassland",
                                                                 ifelse(subDF$BiomeKoppen%in%c(1:9), "Temperate", 
                                                                        NA))))))
    
    subDF$BiomeKoppen <- ifelse(subDF$BiomeKoppen%in%c(1:9, 11:15, 21:24, 31:37, 41, 42),
                                subDF$BiomeKoppen, NA)
    
    
    ### australia boundary based on WWF scheme
    ausDF <- read.csv("data/biome_temp_prec_full_1991_2012.csv")
    ausDF <- ausDF[,c("lon", "lat", "BIOME")]
    
    ausDF <- subset(ausDF, lon < 160 & lon > 110 & lat < -10 & lat > -45)
    
    r3 <- rasterFromXYZ(ausDF)
    
    out2 <- resample(r3, r1, method="ngb")
    subDF$BiomeWWF <- extract(out2, cbind(gridDF$lon, gridDF$lat))
    
    subDF$BiomeWWF <- ifelse(subDF$BiomeWWF == 0, NA, subDF$BiomeWWF)
    
    test <- subDF
    test$BiomeKoppen <- ifelse(is.na(test$BiomeWWF), NA, test$BiomeKoppen)
    test$BiomeKoppenBroad <- ifelse(is.na(test$BiomeWWF), NA, test$BiomeKoppenBroad)
    
    
    ### plot 
    if (plot.decision == T) {
      col.pal <- viridis_pal()(6)
      
      p1 <- ggplot(test)+
        geom_tile(aes(lon, lat, fill=BiomeKoppenBroad))+
        coord_quickmap(xlim=range(test$lon), ylim=range(test$lat))+
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
      
      
    } else {
      print("no figure plotted")
    }
    
    
    return(test)
}
