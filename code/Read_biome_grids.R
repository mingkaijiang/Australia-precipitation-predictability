Read_biome_grids <- function() {
    
  ### gridDF - this is the precipitation grid df
    gridDF <- readRDS("scaled_rainfall_data/rainfall_monthly_DF1930.rds")
    gridDF$lat <- gridDF$lat
    gridDF$id <- 1:length(gridDF$lat)
    subDF <- gridDF[,c("lon", "lat", "id")]
    
    r1 <- rasterFromXYZ(subDF)
    
    kpnDF <- read.csv("data/biome_temp_prec_full_1991_2012.csv")
    kpnDF <- kpnDF[,c("lon", "lat", "BIOME")]
    
    kpnDF <- subset(kpnDF, lon < 160 & lon > 110 & lat < -10 & lat > -45)
    
    r2 <- rasterFromXYZ(kpnDF)
    
    out <- resample(r2, r1, method="ngb")
   
    pdf(paste0(getwd(), "/output/biomes_boundary.pdf"))
    plot(out)
    world(add=T)
    dev.off()

    
    ### extract data according to gridDF lon and lat info
    biom.value <- extract(out, cbind(gridDF$lon, gridDF$lat))
    subDF$Biome <- biom.value
  
    return(subDF)
}
