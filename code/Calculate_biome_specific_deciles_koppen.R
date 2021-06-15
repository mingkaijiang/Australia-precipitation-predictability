Calculate_biome_specific_deciles_koppen <- function(sourceDir, destDir, biome.decision) {
    
    ### Get biome and grid information
    bDF <- Read_biome_grids_koppen(plot.decision=T)

    
    ### read precipitation data
    filenames <- list.files(sourceDir, pattern="*.rds", full.names=TRUE)

    ### Read in all files in the input directory
    for (i in seq_along(filenames)) {
        assign(paste0("DF", i), readRDS(filenames[i]))
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
    
    koppen.subgroup <- as.numeric(na.omit(unique(bDF$BiomeKoppen)))
    koppen.broadgroup <- as.character(na.omit(unique(bDF$BiomeKoppenBroad)))
    wwf.group <- as.numeric(na.omit(unique(bDF$BiomeWWF)))

    if (biome.decision == "koppen_subgroup") {
      
      biome.list <- koppen.subgroup
      
      for (i in biome.list) {
        assign(paste0("bio", i, ".sites"), bDF$id[bDF$BiomeKoppen%in%c(i)])
      }
      
      
      ### Combine monthly rainfall data for each biome
      bio1.matrix <- array(NA, c(length(bio1.sites), 12, 90))
      bio2.matrix <- array(NA, c(length(bio2.sites), 12, 90))
      bio3.matrix <- array(NA, c(length(bio3.sites), 12, 90))
      bio4.matrix <- array(NA, c(length(bio4.sites), 12, 90))
      bio5.matrix <- array(NA, c(length(bio5.sites), 12, 90))
      bio6.matrix <- array(NA, c(length(bio6.sites), 12, 90))
      bio7.matrix <- array(NA, c(length(bio7.sites), 12, 90))
      bio8.matrix <- array(NA, c(length(bio8.sites), 12, 90))
      bio9.matrix <- array(NA, c(length(bio9.sites), 12, 90))
      
      bio11.matrix <- array(NA, c(length(bio11.sites), 12, 90))
      bio12.matrix <- array(NA, c(length(bio12.sites), 12, 90))
      bio13.matrix <- array(NA, c(length(bio13.sites), 12, 90))
      bio14.matrix <- array(NA, c(length(bio14.sites), 12, 90))
      bio15.matrix <- array(NA, c(length(bio15.sites), 12, 90))
      
      bio21.matrix <- array(NA, c(length(bio21.sites), 12, 90))
      bio22.matrix <- array(NA, c(length(bio22.sites), 12, 90))
      bio23.matrix <- array(NA, c(length(bio23.sites), 12, 90))
      bio24.matrix <- array(NA, c(length(bio24.sites), 12, 90))
      
      bio31.matrix <- array(NA, c(length(bio31.sites), 12, 90))
      bio32.matrix <- array(NA, c(length(bio32.sites), 12, 90))
      bio33.matrix <- array(NA, c(length(bio33.sites), 12, 90))
      bio34.matrix <- array(NA, c(length(bio34.sites), 12, 90))
      bio35.matrix <- array(NA, c(length(bio35.sites), 12, 90))
      bio36.matrix <- array(NA, c(length(bio36.sites), 12, 90))
      bio37.matrix <- array(NA, c(length(bio37.sites), 12, 90))
      
      bio41.matrix <- array(NA, c(length(bio41.sites), 12, 90))
      bio42.matrix <- array(NA, c(length(bio42.sites), 12, 90))
      
      
      for (i in 1:90) {
        tmpDF <- get(paste0("DF", i))
        bio1.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio1.sites, 3:14])
        bio2.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio2.sites, 3:14])
        bio3.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio3.sites, 3:14])
        bio4.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio4.sites, 3:14])
        bio5.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio5.sites, 3:14])
        bio6.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio6.sites, 3:14])
        bio7.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio7.sites, 3:14])
        bio8.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio8.sites, 3:14])
        bio9.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio9.sites, 3:14])
        
        bio11.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio11.sites, 3:14])
        bio12.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio12.sites, 3:14])
        bio13.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio13.sites, 3:14])
        bio14.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio14.sites, 3:14])
        bio15.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio15.sites, 3:14])
        
        bio21.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio21.sites, 3:14])
        bio22.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio22.sites, 3:14])
        bio23.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio23.sites, 3:14])
        bio24.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio24.sites, 3:14])
        
        bio31.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio31.sites, 3:14])
        bio32.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio32.sites, 3:14])
        bio33.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio33.sites, 3:14])
        bio34.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio34.sites, 3:14])        
        bio35.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio35.sites, 3:14])
        bio36.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio36.sites, 3:14])
        bio37.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio37.sites, 3:14])
        
        bio41.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio41.sites, 3:14])
        bio42.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio42.sites, 3:14])
        
      }
      
      ### exlude zero and re-add zero back in again
      bio1.data <- replace(bio1.matrix, bio1.matrix == 0, NA)
      bio2.data <- replace(bio2.matrix, bio2.matrix == 0, NA)
      bio3.data <- replace(bio3.matrix, bio3.matrix == 0, NA)
      bio4.data <- replace(bio4.matrix, bio4.matrix == 0, NA)
      bio5.data <- replace(bio5.matrix, bio5.matrix == 0, NA)
      bio6.data <- replace(bio6.matrix, bio6.matrix == 0, NA)
      bio7.data <- replace(bio7.matrix, bio7.matrix == 0, NA)
      bio8.data <- replace(bio8.matrix, bio8.matrix == 0, NA)
      bio9.data <- replace(bio9.matrix, bio9.matrix == 0, NA)
      
      bio11.data <- replace(bio11.matrix, bio11.matrix == 0, NA)
      bio12.data <- replace(bio12.matrix, bio12.matrix == 0, NA)
      bio13.data <- replace(bio13.matrix, bio13.matrix == 0, NA)
      bio14.data <- replace(bio14.matrix, bio14.matrix == 0, NA)
      bio15.data <- replace(bio15.matrix, bio15.matrix == 0, NA)
      
      bio21.data <- replace(bio21.matrix, bio21.matrix == 0, NA)
      bio22.data <- replace(bio22.matrix, bio22.matrix == 0, NA)
      bio23.data <- replace(bio23.matrix, bio23.matrix == 0, NA)
      bio24.data <- replace(bio24.matrix, bio24.matrix == 0, NA)
      
      bio31.data <- replace(bio31.matrix, bio31.matrix == 0, NA)
      bio32.data <- replace(bio32.matrix, bio32.matrix == 0, NA)
      bio33.data <- replace(bio33.matrix, bio33.matrix == 0, NA)
      bio34.data <- replace(bio34.matrix, bio34.matrix == 0, NA)
      bio35.data <- replace(bio35.matrix, bio35.matrix == 0, NA)
      bio36.data <- replace(bio36.matrix, bio36.matrix == 0, NA)
      bio37.data <- replace(bio37.matrix, bio37.matrix == 0, NA)
      
      bio41.data <- replace(bio41.matrix, bio41.matrix == 0, NA)
      bio42.data <- replace(bio42.matrix, bio42.matrix == 0, NA)
      
      
      ### Create out df
      outDF <- data.frame(biome.list, NA, NA, NA, NA, NA, 
                          NA, NA, NA, NA, NA, NA)
      colnames(outDF) <- c("Biome", "D0", "D1", "D2", "D3", "D4", "D5",
                           "D6", "D7", "D8", "D9", "D10") 
      
      outDF[,2] <- 0.0
      
      outDF[outDF$Biome==1, 3:12] <- quantile(bio1.data, prob=seq(0,1, length=10), na.rm=T)
      outDF[outDF$Biome==2, 3:12] <- quantile(bio2.data, prob=seq(0,1, length=10), na.rm=T)
      outDF[outDF$Biome==3, 3:12] <- quantile(bio3.data, prob=seq(0,1, length=10), na.rm=T)
      outDF[outDF$Biome==4, 3:12] <- quantile(bio4.data, prob=seq(0,1, length=10), na.rm=T)
      outDF[outDF$Biome==5, 3:12] <- quantile(bio5.data, prob=seq(0,1, length=10), na.rm=T)
      outDF[outDF$Biome==6, 3:12] <- quantile(bio6.data, prob=seq(0,1, length=10), na.rm=T)
      outDF[outDF$Biome==7, 3:12] <- quantile(bio7.data, prob=seq(0,1, length=10), na.rm=T)
      outDF[outDF$Biome==8, 3:12] <- quantile(bio8.data, prob=seq(0,1, length=10), na.rm=T)
      outDF[outDF$Biome==9, 3:12] <- quantile(bio9.data, prob=seq(0,1, length=10), na.rm=T)
      
      outDF[outDF$Biome==11, 3:12] <- quantile(bio11.data, prob=seq(0,1, length=10), na.rm=T)
      outDF[outDF$Biome==12, 3:12] <- quantile(bio12.data, prob=seq(0,1, length=10), na.rm=T)
      outDF[outDF$Biome==13, 3:12] <- quantile(bio13.data, prob=seq(0,1, length=10), na.rm=T)
      outDF[outDF$Biome==14, 3:12] <- quantile(bio14.data, prob=seq(0,1, length=10), na.rm=T)
      outDF[outDF$Biome==15, 3:12] <- quantile(bio15.data, prob=seq(0,1, length=10), na.rm=T)
      
      outDF[outDF$Biome==21, 3:12] <- quantile(bio21.data, prob=seq(0,1, length=10), na.rm=T)
      outDF[outDF$Biome==22, 3:12] <- quantile(bio22.data, prob=seq(0,1, length=10), na.rm=T)
      outDF[outDF$Biome==23, 3:12] <- quantile(bio23.data, prob=seq(0,1, length=10), na.rm=T)
      outDF[outDF$Biome==24, 3:12] <- quantile(bio24.data, prob=seq(0,1, length=10), na.rm=T)
      
      outDF[outDF$Biome==31, 3:12] <- quantile(bio31.data, prob=seq(0,1, length=10), na.rm=T)
      outDF[outDF$Biome==32, 3:12] <- quantile(bio32.data, prob=seq(0,1, length=10), na.rm=T)
      outDF[outDF$Biome==33, 3:12] <- quantile(bio33.data, prob=seq(0,1, length=10), na.rm=T)
      outDF[outDF$Biome==34, 3:12] <- quantile(bio34.data, prob=seq(0,1, length=10), na.rm=T)
      outDF[outDF$Biome==35, 3:12] <- quantile(bio35.data, prob=seq(0,1, length=10), na.rm=T)
      outDF[outDF$Biome==36, 3:12] <- quantile(bio36.data, prob=seq(0,1, length=10), na.rm=T)
      outDF[outDF$Biome==37, 3:12] <- quantile(bio37.data, prob=seq(0,1, length=10), na.rm=T)
      
      outDF[outDF$Biome==41, 3:12] <- quantile(bio41.data, prob=seq(0,1, length=10), na.rm=T)
      outDF[outDF$Biome==42, 3:12] <- quantile(bio42.data, prob=seq(0,1, length=10), na.rm=T)
      
      saveRDS(outDF, paste0(destDir, "/biome_koppen_subgroup_rainfall_deciles.rds"))
      
    } else if (biome.decision == "koppen_broadgroup") {
      
      biome.list <- koppen.broadgroup
      
      for (i in biome.list) {
        assign(paste0("bio", i, ".sites"), bDF$id[bDF$BiomeKoppenBroad%in%c(i)])
      }
      
      for (i in biome.list) {
        assign(paste0("bio", i, ".sites"), bDF$id[bDF$BiomeWWF%in%c(i)])
      }
      
      ### Combine monthly rainfall data for each biome
      bioEquatorial.matrix <- array(NA, c(length(bioEquatorial.sites), 12, 90))
      bioTropical.matrix <- array(NA, c(length(bioTropical.sites), 12, 90))
      bioGrassland.matrix <- array(NA, c(length(bioGrassland.sites), 12, 90))
      bioSubtropical.matrix <- array(NA, c(length(bioSubtropical.sites), 12, 90))
      bioDesert.matrix <- array(NA, c(length(bioDesert.sites), 12, 90))
      bioTemperate.matrix <- array(NA, c(length(bioTemperate.sites), 12, 90))

      
      for (i in 1:90) {
        tmpDF <- get(paste0("DF", i))
        bioEquatorial.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bioEquatorial.sites, 3:14])
        bioTropical.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bioTropical.sites, 3:14])
        bioGrassland.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bioGrassland.sites, 3:14])
        bioSubtropical.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bioSubtropical.sites, 3:14])
        bioDesert.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bioDesert.sites, 3:14])
        bioTemperate.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bioTemperate.sites, 3:14])

        
      }
      
      ### exlude zero and re-add zero back in again
      bioEquatorial.data <- replace(bioEquatorial.matrix, bioEquatorial.matrix == 0, NA)
      bioTropical.data <- replace(bioTropical.matrix, bioTropical.matrix == 0, NA)
      bioGrassland.data <- replace(bioGrassland.matrix, bioGrassland.matrix == 0, NA)
      
      bioSubtropical.data <- replace(bioSubtropical.matrix, bioSubtropical.matrix == 0, NA)
      bioDesert.data <- replace(bioDesert.matrix, bioDesert.matrix == 0, NA)
      bioTemperate.data <- replace(bioTemperate.matrix, bioTemperate.matrix == 0, NA)

      
      
      ### Create out df
      outDF <- data.frame(biome.list, NA, NA, NA, NA, NA, 
                          NA, NA, NA, NA, NA, NA)
      colnames(outDF) <- c("Biome", "D0", "D1", "D2", "D3", "D4", "D5",
                           "D6", "D7", "D8", "D9", "D10") 
      
      outDF[,2] <- 0.0
      
      outDF[outDF$Biome=="Equatorial", 3:12] <- quantile(bioEquatorial.data, prob=seq(0,1, length=10), na.rm=T)
      outDF[outDF$Biome=="Tropical", 3:12] <- quantile(bioTropical.data, prob=seq(0,1, length=10), na.rm=T)
      outDF[outDF$Biome=="Grassland", 3:12] <- quantile(bioGrassland.data, prob=seq(0,1, length=10), na.rm=T)
      
      outDF[outDF$Biome=="Subtropical", 3:12] <- quantile(bioSubtropical.data, prob=seq(0,1, length=10), na.rm=T)
      outDF[outDF$Biome=="Desert", 3:12] <- quantile(bioDesert.data, prob=seq(0,1, length=10), na.rm=T)
      outDF[outDF$Biome=="Temperate", 3:12] <- quantile(bioTemperate.data, prob=seq(0,1, length=10), na.rm=T)

      saveRDS(outDF, paste0(destDir, "/biome_koppen_broad_group_rainfall_deciles.rds"))
      
      
    } else if (biome.decision == "wwf_group") {
      
      biome.list <- wwf.group
      
      for (i in biome.list) {
        assign(paste0("bio", i, ".sites"), bDF$id[bDF$BiomeWWF%in%c(i)])
      }
      
      ### Combine monthly rainfall data for each biome
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
      
      saveRDS(outDF, paste0(destDir, "/biome_wwf_rainfall_deciles.rds"))
      
    } else {
      "no biome classification given"
    }
    
}