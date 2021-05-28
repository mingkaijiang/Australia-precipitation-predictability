Calculate_biome_specific_deciles <- function(sourceDir, destDir, return.decision) {
    
    ### Get biome and grid information
    bDF <- Read_biome_grids()

    
    ### read precipitation data
    filenames <- list.files(sourceDir, pattern="*.rds", full.names=TRUE)

    ### Read in all files in the input directory
    for (i in seq_along(filenames)) {
        assign(paste0("DF", i), readRDS(filenames[i]))
    }
    
    ### biomes are: Tropical and subtropical moist broadleaf forests: 1
    ###             Tropical and subtropical dry broadleaf forests: 2
    ###             Tropical and subtropical coniferous forests: 3
    ###             Temperate Broadleaf and mixed forests: 4
    ###             Temperate conifer forests: 5
    ###             Boreal forests/Taiga: 6
    ###             Tropical and subtropical grasslands, savannas and shrublands: 7
    ###             Temperate grasslands, savannas and shrublands: 8
    ###             Flooded grasslands and savannas: 9
    ###             Montane grasslands and shrublands: 10
    ###             Tundra: 11
    ###             Mediterranean forests, woodlands and scrub: 12
    ###             Deserts and xeric shrublands: 13
    ###             Mangroves: 14
    
    
    for (i in c(1, 2, 4, 7, 8, 10, 12, 13)) {
      assign(paste0("bio", i, ".sites"), bDF$id[bDF$Biome%in%c(i)])
    }
    
    
    ### Assin ID to all rainfall data
    DF1$ID <- DF2$ID <- DF3$ID <- DF4$ID <- DF5$ID <- DF6$ID <- DF7$ID <- DF8$ID <- DF9$ID <- DF10$ID <- 1:length(DF1$lon)
    DF11$ID <- DF12$ID <- DF13$ID <- DF14$ID <- DF15$ID <- DF16$ID <- DF17$ID <- DF18$ID <- DF19$ID <- DF20$ID <- 1:length(DF1$lon)
    DF21$ID <- DF22$ID <- DF23$ID <- DF24$ID <- DF25$ID <- DF26$ID <- DF27$ID <- DF28$ID <- DF29$ID <- DF30$ID <- 1:length(DF1$lon)
    DF31$ID <- DF32$ID <- DF33$ID <- DF34$ID <- DF35$ID <- DF36$ID <- DF37$ID <- DF38$ID <- DF39$ID <- DF40$ID <- 1:length(DF1$lon)
    DF41$ID <- DF42$ID <- DF43$ID <- DF44$ID <- DF45$ID <- DF46$ID <- DF47$ID <- DF48$ID <- DF49$ID <- DF50$ID <- 1:length(DF1$lon)
    DF51$ID <- DF52$ID <- DF53$ID <- DF54$ID <- DF55$ID <- DF56$ID <- DF57$ID <- DF58$ID <- DF59$ID <- DF60$ID <- 1:length(DF1$lon)
    DF61$ID <- DF62$ID <- DF63$ID <- DF64$ID <- DF65$ID <- DF66$ID <- DF67$ID <- DF68$ID <- DF69$ID <- DF70$ID <- 1:length(DF1$lon)
    DF71$ID <- DF72$ID <- DF73$ID <- DF74$ID <- DF75$ID <- DF76$ID <- DF77$ID <- DF78$ID <- DF79$ID <- DF80$ID <- 1:length(DF1$lon)
    DF81$ID <- DF82$ID <- DF83$ID <- DF84$ID <- DF85$ID <- DF86$ID <- DF87$ID <- DF88$ID <- DF89$ID <- DF90$ID <- 1:length(DF1$lon)
    
    ### Combine monthly rainfall data for equatorial sites
    bio1.matrix <- array(NA, c(length(bio1.sites), 12, 90))
    bio2.matrix <- array(NA, c(length(bio2.sites), 12, 90))
    bio4.matrix <- array(NA, c(length(bio4.sites), 12, 90))
    bio7.matrix <- array(NA, c(length(bio7.sites), 12, 90))
    bio8.matrix <- array(NA, c(length(bio8.sites), 12, 90))
    bio10.matrix <- array(NA, c(length(bio10.sites), 12, 90))
    bio12.matrix <- array(NA, c(length(bio12.sites), 12, 90))
    bio13.matrix <- array(NA, c(length(bio13.sites), 12, 90))

    
    for (i in 1:90) {
      tmpDF <- get(paste0("DF", i))
      bio1.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio1.sites, 3:14])
      bio2.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio2.sites, 3:14])
      bio4.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio4.sites, 3:14])
      bio7.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio7.sites, 3:14])
      bio8.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio8.sites, 3:14])
      bio10.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio10.sites, 3:14])
      bio12.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio12.sites, 3:14])
      bio13.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio13.sites, 3:14])

    }
    
    ### exlude zero and re-add zero back in again
    bio1.data <- replace(bio1.matrix, bio1.matrix == 0, NA)
    bio2.data <- replace(bio2.matrix, bio2.matrix == 0, NA)
    bio4.data <- replace(bio4.matrix, bio4.matrix == 0, NA)
    
    bio7.data <- replace(bio7.matrix, bio7.matrix == 0, NA)
    bio8.data <- replace(bio8.matrix, bio8.matrix == 0, NA)
    bio10.data <- replace(bio10.matrix, bio10.matrix == 0, NA)
    bio12.data <- replace(bio12.matrix, bio12.matrix == 0, NA)
    bio13.data <- replace(bio13.matrix, bio13.matrix == 0, NA)

    #b.list <- c("Equatorial", "Tropical", "Subtropical", "Desert", "Grassland", "Temperate", "Other")
    
    ### Compute decile and quantile information for each biome
    if (return.decision=="decile") {

        ### Create out df
        outDF <- data.frame(c(1, 2, 4, 7, 8, 10, 12, 13), NA, NA, NA, NA, NA, 
                            NA, NA, NA, NA, NA, NA)
        colnames(outDF) <- c("Biome", "D0", "D1", "D2", "D3", "D4", "D5",
                             "D6", "D7", "D8", "D9", "D10") 
        
        outDF[,2] <- 0.0
        
        outDF[outDF$Biome==1, 3:12] <- quantile(bio1.data, prob=seq(0,1, length=10), na.rm=T)
        outDF[outDF$Biome==2, 3:12] <- quantile(bio2.data, prob=seq(0,1, length=10), na.rm=T)
        outDF[outDF$Biome==4, 3:12] <- quantile(bio4.data, prob=seq(0,1, length=10), na.rm=T)
        
        outDF[outDF$Biome==7, 3:12] <- quantile(bio7.data, prob=seq(0,1, length=10), na.rm=T)
        outDF[outDF$Biome==8, 3:12] <- quantile(bio8.data, prob=seq(0,1, length=10), na.rm=T)
        outDF[outDF$Biome==10, 3:12] <- quantile(bio10.data, prob=seq(0,1, length=10), na.rm=T)
        outDF[outDF$Biome==12, 3:12] <- quantile(bio12.data, prob=seq(0,1, length=10), na.rm=T)
        outDF[outDF$Biome==13, 3:12] <- quantile(bio13.data, prob=seq(0,1, length=10), na.rm=T)

    } else if (return.decision=="quantile") {
        
        print("no code developed")
    }
    
    
    saveRDS(outDF, paste0(destDir, "/biome_rainfall_deciles.rds"))
    
    return(outDF)
}