Calculate_biome_specific_deciles <- function(sourceDir, return.decision) {
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
    eq.sites <- bDF$Site_ID[bDF$Biome%in%c(40, 41, 42)]
    tr.sites <- bDF$Site_ID[bDF$Biome%in%c(37, 36, 35)]
    sb.sites <- bDF$Site_ID[bDF$Biome%in%c(34, 33, 32, 31)]
    ds.sites <- bDF$Site_ID[bDF$Biome%in%c(24, 23, 22, 21)]
    gs.sites <- bDF$Site_ID[bDF$Biome%in%c(15, 14, 13, 12, 11)]
    tm.sites <- bDF$Site_ID[bDF$Biome%in%c(9, 8, 7, 6, 5, 4, 3, 2, 1)]
    na.sites <- bDF$Site_ID[bDF$Biome%in%c(90, 0)]
    
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
    eq.matrix <- array(NA, c(length(eq.sites), 12, 90))
    tr.matrix <- array(NA, c(length(tr.sites), 12, 90))
    sb.matrix <- array(NA, c(length(sb.sites), 12, 90))
    ds.matrix <- array(NA, c(length(ds.sites), 12, 90))
    gs.matrix <- array(NA, c(length(gs.sites), 12, 90))
    tm.matrix <- array(NA, c(length(tm.sites), 12, 90))
    na.matrix <- array(NA, c(length(na.sites), 12, 90))
    
    for (i in 1:90) {
      tmpDF <- get(paste0("DF", i))
      eq.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% eq.sites, 3:14])
      tr.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% tr.sites, 3:14])
      sb.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% sb.sites, 3:14])
      ds.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% ds.sites, 3:14])
      gs.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% gs.sites, 3:14])
      tm.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% tm.sites, 3:14])
      na.matrix[,,i] <- as.matrix(tmpDF[tmpDF$ID %in% na.sites, 3:14])
      
    }
    
   ## decide on whether to exclude zero or not.
    
    
    b.list <- c("Equatorial", "Tropical", "Subtropical", "Desert", "Grassland", "Temperate", "Other")
    
    ### Compute decile and quantile information for each biome
    if (return.decision=="decile") {

        ### Create out df
        outDF <- data.frame(b.list, NA, NA, NA, NA, NA, 
                            NA, NA, NA, NA, NA, NA)
        colnames(outDF) <- c("Biome", "D0", "D1", "D2", "D3", "D4", "D5",
                             "D6", "D7", "D8", "D9", "D10") 
        
        outDF[outDF$Biome=="Equatorial", 2:12] <- quantile(eq.data, prob=seq(0,1, length=11))
        outDF[outDF$Biome=="Tropical", 2:12] <- quantile(tr.data, prob=seq(0,1, length=11))
        outDF[outDF$Biome=="Subtropical", 2:12] <- quantile(sb.data, prob=seq(0,1, length=11))
        outDF[outDF$Biome=="Desert", 2:12] <- quantile(ds.data, prob=seq(0,1, length=11))
        outDF[outDF$Biome=="Grassland", 2:12] <- quantile(gs.data, prob=seq(0,1, length=11))
        outDF[outDF$Biome=="Temperate", 2:12] <- quantile(tm.data, prob=seq(0,1, length=11))
        outDF[outDF$Biome=="Other", 2:12] <- quantile(na.data, prob=seq(0,1, length=11))
        
    } else if (return.decision=="quantile") {
        
        ### Create out df
        outDF <- data.frame(b.list, NA, NA, NA, NA, NA)
        colnames(outDF) <- c("Biome", "Q0", "Q1", "Q2", "Q3", "Q4") 
        
        outDF[outDF$Biome=="Equatorial", 2:6] <- quantile(eq.data, prob=seq(0,1, length=5))
        outDF[outDF$Biome=="Tropical", 2:6] <- quantile(tr.data, prob=seq(0,1, length=5))
        outDF[outDF$Biome=="Subtropical", 2:6] <- quantile(sb.data, prob=seq(0,1, length=5))
        outDF[outDF$Biome=="Desert", 2:6] <- quantile(ds.data, prob=seq(0,1, length=5))
        outDF[outDF$Biome=="Grassland", 2:6] <- quantile(gs.data, prob=seq(0,1, length=5))
        outDF[outDF$Biome=="Temperate", 2:6] <- quantile(tm.data, prob=seq(0,1, length=5))
        outDF[outDF$Biome=="Other", 2:6] <- quantile(na.data, prob=seq(0,1, length=5))
        
    }
    
    return(outDF)
}