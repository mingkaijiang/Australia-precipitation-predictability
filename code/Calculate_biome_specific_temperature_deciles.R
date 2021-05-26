Calculate_biome_specific_temperature_deciles <- function(sourceDir, destDir, return.decision) {
    
    ### Get biome and grid information
    bDF <- Read_biome_grids()
    bDF$Biome[is.na(bDF$Biome)] <- 90
    bDF$Site_ID <- 1:length(bDF$lon)
    
    ### read precipitation data
    filenames <- list.files(sourceDir, pattern="*.rds", full.names=TRUE)

    ### Read in all files in the input directory
    for (i in seq_along(filenames)) {
        assign(paste0("DF", i), readRDS(filenames[i]))
    }
    
    ### biomes are: Equatorial: 41, 42
    ###             Tropical: 37, 36, 35
    ###             Subtropical: 34, 33, 32, 31
    ###             Desert: 24, 23, 22, 21
    ###             Grassland: 15, 14, 13, 12, 11
    ###             Temperate: 9, 8, 7, 6, 5, 4, 3, 2, 1
    #eq.sites <- bDF$Site_ID[bDF$Biome%in%c(40, 41, 42)]
    #tr.sites <- bDF$Site_ID[bDF$Biome%in%c(37, 36, 35)]
    #sb.sites <- bDF$Site_ID[bDF$Biome%in%c(34, 33, 32, 31)]
    #ds.sites <- bDF$Site_ID[bDF$Biome%in%c(24, 23, 22, 21)]
    #gs.sites <- bDF$Site_ID[bDF$Biome%in%c(15, 14, 13, 12, 11)]
    #tm.sites <- bDF$Site_ID[bDF$Biome%in%c(9, 8, 7, 6, 5, 4, 3, 2, 1)]
    #na.sites <- bDF$Site_ID[bDF$Biome%in%c(90, 0)]
    
    for (i in c(1:9, 11:15, 21:24, 31:34, 35:37, 40:42, 0, 90)) {
      assign(paste0("bio", i, ".sites"), bDF$Site_ID[bDF$Biome%in%c(i)])
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
    bio40.matrix <- array(NA, c(length(bio40.sites), 12, 90))
    bio41.matrix <- array(NA, c(length(bio41.sites), 12, 90))
    bio42.matrix <- array(NA, c(length(bio42.sites), 12, 90))
    bio0.matrix <- array(NA, c(length(bio0.sites), 12, 90))
    bio90.matrix <- array(NA, c(length(bio90.sites), 12, 90))
    
    
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
      bio40.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio40.sites, 3:14])
      bio41.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio41.sites, 3:14])
      bio42.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio42.sites, 3:14])
      bio0.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio0.sites, 3:14])
      bio90.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% bio90.sites, 3:14])
      
      
    }
    
    
    ### Compute decile and quantile information for each biome
    if (return.decision=="decile") {

        ### Create out df
        outDF <- data.frame(c(1:9, 11:15, 21:24, 31:34, 35:37, 40:42, 0, 90), NA, NA, NA, NA, NA, 
                            NA, NA, NA, NA, NA, NA)
        colnames(outDF) <- c("Biome", "D0", "D1", "D2", "D3", "D4", "D5",
                             "D6", "D7", "D8", "D9", "D10") 
        
        outDF[outDF$Biome==1, 2:12] <- quantile(bio1.matrix, prob=seq(0,1, length=11), na.rm=T)
        outDF[outDF$Biome==2, 2:12] <- quantile(bio2.matrix, prob=seq(0,1, length=11), na.rm=T)
        outDF[outDF$Biome==3, 2:12] <- quantile(bio3.matrix, prob=seq(0,1, length=11), na.rm=T)
        outDF[outDF$Biome==4, 2:12] <- quantile(bio4.matrix, prob=seq(0,1, length=11), na.rm=T)
        outDF[outDF$Biome==5, 2:12] <- quantile(bio5.matrix, prob=seq(0,1, length=11), na.rm=T)
        outDF[outDF$Biome==6, 2:12] <- quantile(bio6.matrix, prob=seq(0,1, length=11), na.rm=T)
        outDF[outDF$Biome==7, 2:12] <- quantile(bio7.matrix, prob=seq(0,1, length=11), na.rm=T)
        outDF[outDF$Biome==8, 2:12] <- quantile(bio8.matrix, prob=seq(0,1, length=11), na.rm=T)
        outDF[outDF$Biome==9, 2:12] <- quantile(bio9.matrix, prob=seq(0,1, length=11), na.rm=T)
        outDF[outDF$Biome==11, 2:12] <- quantile(bio11.matrix, prob=seq(0,1, length=11), na.rm=T)
        outDF[outDF$Biome==12, 2:12] <- quantile(bio12.matrix, prob=seq(0,1, length=11), na.rm=T)
        outDF[outDF$Biome==13, 2:12] <- quantile(bio13.matrix, prob=seq(0,1, length=11), na.rm=T)
        outDF[outDF$Biome==14, 2:12] <- quantile(bio14.matrix, prob=seq(0,1, length=11), na.rm=T)
        outDF[outDF$Biome==15, 2:12] <- quantile(bio15.matrix, prob=seq(0,1, length=11), na.rm=T)
        outDF[outDF$Biome==21, 2:12] <- quantile(bio21.matrix, prob=seq(0,1, length=11), na.rm=T)
        outDF[outDF$Biome==22, 2:12] <- quantile(bio22.matrix, prob=seq(0,1, length=11), na.rm=T)
        outDF[outDF$Biome==23, 2:12] <- quantile(bio23.matrix, prob=seq(0,1, length=11), na.rm=T)
        outDF[outDF$Biome==24, 2:12] <- quantile(bio24.matrix, prob=seq(0,1, length=11), na.rm=T)
        outDF[outDF$Biome==31, 2:12] <- quantile(bio31.matrix, prob=seq(0,1, length=11), na.rm=T)
        outDF[outDF$Biome==32, 2:12] <- quantile(bio32.matrix, prob=seq(0,1, length=11), na.rm=T)
        outDF[outDF$Biome==33, 2:12] <- quantile(bio33.matrix, prob=seq(0,1, length=11), na.rm=T)
        outDF[outDF$Biome==34, 2:12] <- quantile(bio34.matrix, prob=seq(0,1, length=11), na.rm=T)
        outDF[outDF$Biome==35, 2:12] <- quantile(bio35.matrix, prob=seq(0,1, length=11), na.rm=T)
        outDF[outDF$Biome==36, 2:12] <- quantile(bio36.matrix, prob=seq(0,1, length=11), na.rm=T)
        outDF[outDF$Biome==37, 2:12] <- quantile(bio37.matrix, prob=seq(0,1, length=11), na.rm=T)
        outDF[outDF$Biome==40, 2:12] <- quantile(bio40.matrix, prob=seq(0,1, length=11), na.rm=T)
        outDF[outDF$Biome==41, 2:12] <- quantile(bio41.matrix, prob=seq(0,1, length=11), na.rm=T)
        outDF[outDF$Biome==42, 2:12] <- quantile(bio42.matrix, prob=seq(0,1, length=11), na.rm=T)
        outDF[outDF$Biome==0, 2:12] <- quantile(bio0.matrix, prob=seq(0,1, length=11), na.rm=T)
        outDF[outDF$Biome==90, 2:12] <- quantile(bio90.matrix, prob=seq(0,1, length=11), na.rm=T)
        
        
        
    } else if (return.decision=="quantile") {
        
        print("removed")
    }
    
    
    saveRDS(outDF, paste0(destDir, "/biome_temperature_deciles.rds"))
    
    return(outDF)
}