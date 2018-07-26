Calculate_biome_specific_deciles <- function(sourceDir, return.decision) {
    ### Get biome and grid information
    bDF <- Read_biome_grids()
    bDF$Biome[is.na(bDF$Biome)] <- 90
    bDF$Site_ID <- 1:length(bDF$lon)
    
    ### read precipitation data
    filenames <- list.files(sourceDir, pattern="*.csv", full.names=TRUE)

    ### Read in all files in the input directory
    for (i in seq_along(filenames)) {
        assign(paste0("DF", i), read.csv(filenames[i]))
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

    ### Combine monthly rainfall data for equatorial sites
    m1 <- as.vector(as.matrix(DF1[DF1$ID %in% eq.sites, 3:14]))
    m2 <- as.vector(as.matrix(DF2[DF2$ID %in% eq.sites, 3:14]))
    m3 <- as.vector(as.matrix(DF3[DF3$ID %in% eq.sites, 3:14]))
    m4 <- as.vector(as.matrix(DF4[DF4$ID %in% eq.sites, 3:14]))
    m5 <- as.vector(as.matrix(DF5[DF5$ID %in% eq.sites, 3:14]))
    m6 <- as.vector(as.matrix(DF6[DF6$ID %in% eq.sites, 3:14]))
    m7 <- as.vector(as.matrix(DF7[DF7$ID %in% eq.sites, 3:14]))
    m8 <- as.vector(as.matrix(DF8[DF8$ID %in% eq.sites, 3:14]))
    m9 <- as.vector(as.matrix(DF9[DF9$ID %in% eq.sites, 3:14]))
    m10 <- as.vector(as.matrix(DF10[DF10$ID %in% eq.sites, 3:14]))
    
    m11 <- as.vector(as.matrix(DF11[DF11$ID %in% eq.sites, 3:14]))
    m12 <- as.vector(as.matrix(DF12[DF12$ID %in% eq.sites, 3:14]))
    m13 <- as.vector(as.matrix(DF13[DF13$ID %in% eq.sites, 3:14]))
    m14 <- as.vector(as.matrix(DF14[DF14$ID %in% eq.sites, 3:14]))
    m15 <- as.vector(as.matrix(DF15[DF15$ID %in% eq.sites, 3:14]))
    m16 <- as.vector(as.matrix(DF16[DF16$ID %in% eq.sites, 3:14]))
    m17 <- as.vector(as.matrix(DF17[DF17$ID %in% eq.sites, 3:14]))
    m18 <- as.vector(as.matrix(DF18[DF18$ID %in% eq.sites, 3:14]))
    m19 <- as.vector(as.matrix(DF19[DF19$ID %in% eq.sites, 3:14]))
    m20 <- as.vector(as.matrix(DF20[DF20$ID %in% eq.sites, 3:14]))
    
    m21 <- as.vector(as.matrix(DF21[DF21$ID %in% eq.sites, 3:14]))
    m22 <- as.vector(as.matrix(DF22[DF22$ID %in% eq.sites, 3:14]))
    m23 <- as.vector(as.matrix(DF23[DF23$ID %in% eq.sites, 3:14]))
    m24 <- as.vector(as.matrix(DF24[DF24$ID %in% eq.sites, 3:14]))
    m25 <- as.vector(as.matrix(DF25[DF25$ID %in% eq.sites, 3:14]))
    m26 <- as.vector(as.matrix(DF26[DF26$ID %in% eq.sites, 3:14]))
    m27 <- as.vector(as.matrix(DF27[DF27$ID %in% eq.sites, 3:14]))
    m28 <- as.vector(as.matrix(DF28[DF28$ID %in% eq.sites, 3:14]))
    m29 <- as.vector(as.matrix(DF29[DF29$ID %in% eq.sites, 3:14]))
    m30 <- as.vector(as.matrix(DF30[DF30$ID %in% eq.sites, 3:14]))
    
    m31 <- as.vector(as.matrix(DF31[DF31$ID %in% eq.sites, 3:14]))
    m32 <- as.vector(as.matrix(DF32[DF32$ID %in% eq.sites, 3:14]))
    m33 <- as.vector(as.matrix(DF33[DF33$ID %in% eq.sites, 3:14]))
    m34 <- as.vector(as.matrix(DF34[DF34$ID %in% eq.sites, 3:14]))
    m35 <- as.vector(as.matrix(DF35[DF35$ID %in% eq.sites, 3:14]))
    m36 <- as.vector(as.matrix(DF36[DF36$ID %in% eq.sites, 3:14]))
    m37 <- as.vector(as.matrix(DF37[DF37$ID %in% eq.sites, 3:14]))
    m38 <- as.vector(as.matrix(DF38[DF38$ID %in% eq.sites, 3:14]))
    m39 <- as.vector(as.matrix(DF39[DF39$ID %in% eq.sites, 3:14]))
    m40 <- as.vector(as.matrix(DF40[DF40$ID %in% eq.sites, 3:14]))
    
    m41 <- as.vector(as.matrix(DF41[DF41$ID %in% eq.sites, 3:14]))
    m42 <- as.vector(as.matrix(DF42[DF42$ID %in% eq.sites, 3:14]))
    m43 <- as.vector(as.matrix(DF43[DF43$ID %in% eq.sites, 3:14]))
    m44 <- as.vector(as.matrix(DF44[DF44$ID %in% eq.sites, 3:14]))
    m45 <- as.vector(as.matrix(DF45[DF45$ID %in% eq.sites, 3:14]))
    m46 <- as.vector(as.matrix(DF46[DF46$ID %in% eq.sites, 3:14]))
    m47 <- as.vector(as.matrix(DF47[DF47$ID %in% eq.sites, 3:14]))
    m48 <- as.vector(as.matrix(DF48[DF48$ID %in% eq.sites, 3:14]))
    m49 <- as.vector(as.matrix(DF49[DF49$ID %in% eq.sites, 3:14]))
    m50 <- as.vector(as.matrix(DF50[DF50$ID %in% eq.sites, 3:14]))
    
    m51 <- as.vector(as.matrix(DF51[DF51$ID %in% eq.sites, 3:14]))
    m52 <- as.vector(as.matrix(DF52[DF52$ID %in% eq.sites, 3:14]))
    m53 <- as.vector(as.matrix(DF53[DF53$ID %in% eq.sites, 3:14]))
    m54 <- as.vector(as.matrix(DF54[DF54$ID %in% eq.sites, 3:14]))
    m55 <- as.vector(as.matrix(DF55[DF55$ID %in% eq.sites, 3:14]))
    m56 <- as.vector(as.matrix(DF56[DF56$ID %in% eq.sites, 3:14]))
    m57 <- as.vector(as.matrix(DF57[DF57$ID %in% eq.sites, 3:14]))
    m58 <- as.vector(as.matrix(DF58[DF58$ID %in% eq.sites, 3:14]))
    m59 <- as.vector(as.matrix(DF59[DF59$ID %in% eq.sites, 3:14]))
    m60 <- as.vector(as.matrix(DF60[DF60$ID %in% eq.sites, 3:14]))
    
    m61 <- as.vector(as.matrix(DF61[DF61$ID %in% eq.sites, 3:14]))
    m62 <- as.vector(as.matrix(DF62[DF62$ID %in% eq.sites, 3:14]))
    m63 <- as.vector(as.matrix(DF63[DF63$ID %in% eq.sites, 3:14]))
    m64 <- as.vector(as.matrix(DF64[DF64$ID %in% eq.sites, 3:14]))
    m65 <- as.vector(as.matrix(DF65[DF65$ID %in% eq.sites, 3:14]))
    m66 <- as.vector(as.matrix(DF66[DF66$ID %in% eq.sites, 3:14]))
    m67 <- as.vector(as.matrix(DF67[DF67$ID %in% eq.sites, 3:14]))
    m68 <- as.vector(as.matrix(DF68[DF68$ID %in% eq.sites, 3:14]))
    m69 <- as.vector(as.matrix(DF69[DF69$ID %in% eq.sites, 3:14]))
    m70 <- as.vector(as.matrix(DF70[DF70$ID %in% eq.sites, 3:14]))
    
    m71 <- as.vector(as.matrix(DF71[DF71$ID %in% eq.sites, 3:14]))
    m72 <- as.vector(as.matrix(DF72[DF72$ID %in% eq.sites, 3:14]))
    m73 <- as.vector(as.matrix(DF73[DF73$ID %in% eq.sites, 3:14]))
    m74 <- as.vector(as.matrix(DF74[DF74$ID %in% eq.sites, 3:14]))
    m75 <- as.vector(as.matrix(DF75[DF75$ID %in% eq.sites, 3:14]))
    m76 <- as.vector(as.matrix(DF76[DF76$ID %in% eq.sites, 3:14]))
    m77 <- as.vector(as.matrix(DF77[DF77$ID %in% eq.sites, 3:14]))
    m78 <- as.vector(as.matrix(DF78[DF78$ID %in% eq.sites, 3:14]))
    m79 <- as.vector(as.matrix(DF79[DF79$ID %in% eq.sites, 3:14]))
    m80 <- as.vector(as.matrix(DF80[DF80$ID %in% eq.sites, 3:14]))
    
    eq.data <- c(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10,
                 m11, m12, m13, m14, m15, m16, m17, m18, m19, m20,
                 m21, m22, m23, m24, m25, m26, m27, m28, m29, m30,
                 m31, m32, m33, m34, m35, m36, m37, m38, m39, m40,
                 m41, m42, m43, m44, m45, m46, m47, m48, m49, m50,
                 m51, m52, m53, m54, m55, m56, m57, m58, m59, m60,
                 m61, m62, m63, m64, m65, m66, m67, m68, m69, m70,
                 m71, m72, m73, m74, m75, m76, m77, m78, m79, m80)
    
    eq.data <- eq.data[eq.data>0]
    
    ### Combine monthly rainfall data for tropical sites
    m1 <- as.vector(as.matrix(DF1[DF1$ID %in% tr.sites, 3:14]))
    m2 <- as.vector(as.matrix(DF2[DF2$ID %in% tr.sites, 3:14]))
    m3 <- as.vector(as.matrix(DF3[DF3$ID %in% tr.sites, 3:14]))
    m4 <- as.vector(as.matrix(DF4[DF4$ID %in% tr.sites, 3:14]))
    m5 <- as.vector(as.matrix(DF5[DF5$ID %in% tr.sites, 3:14]))
    m6 <- as.vector(as.matrix(DF6[DF6$ID %in% tr.sites, 3:14]))
    m7 <- as.vector(as.matrix(DF7[DF7$ID %in% tr.sites, 3:14]))
    m8 <- as.vector(as.matrix(DF8[DF8$ID %in% tr.sites, 3:14]))
    m9 <- as.vector(as.matrix(DF9[DF9$ID %in% tr.sites, 3:14]))
    m10 <- as.vector(as.matrix(DF10[DF10$ID %in% tr.sites, 3:14]))
    
    m11 <- as.vector(as.matrix(DF11[DF11$ID %in% tr.sites, 3:14]))
    m12 <- as.vector(as.matrix(DF12[DF12$ID %in% tr.sites, 3:14]))
    m13 <- as.vector(as.matrix(DF13[DF13$ID %in% tr.sites, 3:14]))
    m14 <- as.vector(as.matrix(DF14[DF14$ID %in% tr.sites, 3:14]))
    m15 <- as.vector(as.matrix(DF15[DF15$ID %in% tr.sites, 3:14]))
    m16 <- as.vector(as.matrix(DF16[DF16$ID %in% tr.sites, 3:14]))
    m17 <- as.vector(as.matrix(DF17[DF17$ID %in% tr.sites, 3:14]))
    m18 <- as.vector(as.matrix(DF18[DF18$ID %in% tr.sites, 3:14]))
    m19 <- as.vector(as.matrix(DF19[DF19$ID %in% tr.sites, 3:14]))
    m20 <- as.vector(as.matrix(DF20[DF20$ID %in% tr.sites, 3:14]))
    
    m21 <- as.vector(as.matrix(DF21[DF21$ID %in% tr.sites, 3:14]))
    m22 <- as.vector(as.matrix(DF22[DF22$ID %in% tr.sites, 3:14]))
    m23 <- as.vector(as.matrix(DF23[DF23$ID %in% tr.sites, 3:14]))
    m24 <- as.vector(as.matrix(DF24[DF24$ID %in% tr.sites, 3:14]))
    m25 <- as.vector(as.matrix(DF25[DF25$ID %in% tr.sites, 3:14]))
    m26 <- as.vector(as.matrix(DF26[DF26$ID %in% tr.sites, 3:14]))
    m27 <- as.vector(as.matrix(DF27[DF27$ID %in% tr.sites, 3:14]))
    m28 <- as.vector(as.matrix(DF28[DF28$ID %in% tr.sites, 3:14]))
    m29 <- as.vector(as.matrix(DF29[DF29$ID %in% tr.sites, 3:14]))
    m30 <- as.vector(as.matrix(DF30[DF30$ID %in% tr.sites, 3:14]))
    
    m31 <- as.vector(as.matrix(DF31[DF31$ID %in% tr.sites, 3:14]))
    m32 <- as.vector(as.matrix(DF32[DF32$ID %in% tr.sites, 3:14]))
    m33 <- as.vector(as.matrix(DF33[DF33$ID %in% tr.sites, 3:14]))
    m34 <- as.vector(as.matrix(DF34[DF34$ID %in% tr.sites, 3:14]))
    m35 <- as.vector(as.matrix(DF35[DF35$ID %in% tr.sites, 3:14]))
    m36 <- as.vector(as.matrix(DF36[DF36$ID %in% tr.sites, 3:14]))
    m37 <- as.vector(as.matrix(DF37[DF37$ID %in% tr.sites, 3:14]))
    m38 <- as.vector(as.matrix(DF38[DF38$ID %in% tr.sites, 3:14]))
    m39 <- as.vector(as.matrix(DF39[DF39$ID %in% tr.sites, 3:14]))
    m40 <- as.vector(as.matrix(DF40[DF40$ID %in% tr.sites, 3:14]))
    
    m41 <- as.vector(as.matrix(DF41[DF41$ID %in% tr.sites, 3:14]))
    m42 <- as.vector(as.matrix(DF42[DF42$ID %in% tr.sites, 3:14]))
    m43 <- as.vector(as.matrix(DF43[DF43$ID %in% tr.sites, 3:14]))
    m44 <- as.vector(as.matrix(DF44[DF44$ID %in% tr.sites, 3:14]))
    m45 <- as.vector(as.matrix(DF45[DF45$ID %in% tr.sites, 3:14]))
    m46 <- as.vector(as.matrix(DF46[DF46$ID %in% tr.sites, 3:14]))
    m47 <- as.vector(as.matrix(DF47[DF47$ID %in% tr.sites, 3:14]))
    m48 <- as.vector(as.matrix(DF48[DF48$ID %in% tr.sites, 3:14]))
    m49 <- as.vector(as.matrix(DF49[DF49$ID %in% tr.sites, 3:14]))
    m50 <- as.vector(as.matrix(DF50[DF50$ID %in% tr.sites, 3:14]))
    
    m51 <- as.vector(as.matrix(DF51[DF51$ID %in% tr.sites, 3:14]))
    m52 <- as.vector(as.matrix(DF52[DF52$ID %in% tr.sites, 3:14]))
    m53 <- as.vector(as.matrix(DF53[DF53$ID %in% tr.sites, 3:14]))
    m54 <- as.vector(as.matrix(DF54[DF54$ID %in% tr.sites, 3:14]))
    m55 <- as.vector(as.matrix(DF55[DF55$ID %in% tr.sites, 3:14]))
    m56 <- as.vector(as.matrix(DF56[DF56$ID %in% tr.sites, 3:14]))
    m57 <- as.vector(as.matrix(DF57[DF57$ID %in% tr.sites, 3:14]))
    m58 <- as.vector(as.matrix(DF58[DF58$ID %in% tr.sites, 3:14]))
    m59 <- as.vector(as.matrix(DF59[DF59$ID %in% tr.sites, 3:14]))
    m60 <- as.vector(as.matrix(DF60[DF60$ID %in% tr.sites, 3:14]))
    
    m61 <- as.vector(as.matrix(DF61[DF61$ID %in% tr.sites, 3:14]))
    m62 <- as.vector(as.matrix(DF62[DF62$ID %in% tr.sites, 3:14]))
    m63 <- as.vector(as.matrix(DF63[DF63$ID %in% tr.sites, 3:14]))
    m64 <- as.vector(as.matrix(DF64[DF64$ID %in% tr.sites, 3:14]))
    m65 <- as.vector(as.matrix(DF65[DF65$ID %in% tr.sites, 3:14]))
    m66 <- as.vector(as.matrix(DF66[DF66$ID %in% tr.sites, 3:14]))
    m67 <- as.vector(as.matrix(DF67[DF67$ID %in% tr.sites, 3:14]))
    m68 <- as.vector(as.matrix(DF68[DF68$ID %in% tr.sites, 3:14]))
    m69 <- as.vector(as.matrix(DF69[DF69$ID %in% tr.sites, 3:14]))
    m70 <- as.vector(as.matrix(DF70[DF70$ID %in% tr.sites, 3:14]))
    
    m71 <- as.vector(as.matrix(DF71[DF71$ID %in% tr.sites, 3:14]))
    m72 <- as.vector(as.matrix(DF72[DF72$ID %in% tr.sites, 3:14]))
    m73 <- as.vector(as.matrix(DF73[DF73$ID %in% tr.sites, 3:14]))
    m74 <- as.vector(as.matrix(DF74[DF74$ID %in% tr.sites, 3:14]))
    m75 <- as.vector(as.matrix(DF75[DF75$ID %in% tr.sites, 3:14]))
    m76 <- as.vector(as.matrix(DF76[DF76$ID %in% tr.sites, 3:14]))
    m77 <- as.vector(as.matrix(DF77[DF77$ID %in% tr.sites, 3:14]))
    m78 <- as.vector(as.matrix(DF78[DF78$ID %in% tr.sites, 3:14]))
    m79 <- as.vector(as.matrix(DF79[DF79$ID %in% tr.sites, 3:14]))
    m80 <- as.vector(as.matrix(DF80[DF80$ID %in% tr.sites, 3:14]))
    
    tr.data <- c(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10,
                 m11, m12, m13, m14, m15, m16, m17, m18, m19, m20,
                 m21, m22, m23, m24, m25, m26, m27, m28, m29, m30,
                 m31, m32, m33, m34, m35, m36, m37, m38, m39, m40,
                 m41, m42, m43, m44, m45, m46, m47, m48, m49, m50,
                 m51, m52, m53, m54, m55, m56, m57, m58, m59, m60,
                 m61, m62, m63, m64, m65, m66, m67, m68, m69, m70,
                 m71, m72, m73, m74, m75, m76, m77, m78, m79, m80)
 
    tr.data <- tr.data[tr.data>0]
    
    ### Combine monthly rainfall data for subtropical sites
    m1 <- as.vector(as.matrix(DF1[DF1$ID %in% sb.sites, 3:14]))
    m2 <- as.vector(as.matrix(DF2[DF2$ID %in% sb.sites, 3:14]))
    m3 <- as.vector(as.matrix(DF3[DF3$ID %in% sb.sites, 3:14]))
    m4 <- as.vector(as.matrix(DF4[DF4$ID %in% sb.sites, 3:14]))
    m5 <- as.vector(as.matrix(DF5[DF5$ID %in% sb.sites, 3:14]))
    m6 <- as.vector(as.matrix(DF6[DF6$ID %in% sb.sites, 3:14]))
    m7 <- as.vector(as.matrix(DF7[DF7$ID %in% sb.sites, 3:14]))
    m8 <- as.vector(as.matrix(DF8[DF8$ID %in% sb.sites, 3:14]))
    m9 <- as.vector(as.matrix(DF9[DF9$ID %in% sb.sites, 3:14]))
    m10 <- as.vector(as.matrix(DF10[DF10$ID %in% sb.sites, 3:14]))
    
    m11 <- as.vector(as.matrix(DF11[DF11$ID %in% sb.sites, 3:14]))
    m12 <- as.vector(as.matrix(DF12[DF12$ID %in% sb.sites, 3:14]))
    m13 <- as.vector(as.matrix(DF13[DF13$ID %in% sb.sites, 3:14]))
    m14 <- as.vector(as.matrix(DF14[DF14$ID %in% sb.sites, 3:14]))
    m15 <- as.vector(as.matrix(DF15[DF15$ID %in% sb.sites, 3:14]))
    m16 <- as.vector(as.matrix(DF16[DF16$ID %in% sb.sites, 3:14]))
    m17 <- as.vector(as.matrix(DF17[DF17$ID %in% sb.sites, 3:14]))
    m18 <- as.vector(as.matrix(DF18[DF18$ID %in% sb.sites, 3:14]))
    m19 <- as.vector(as.matrix(DF19[DF19$ID %in% sb.sites, 3:14]))
    m20 <- as.vector(as.matrix(DF20[DF20$ID %in% sb.sites, 3:14]))
    
    m21 <- as.vector(as.matrix(DF21[DF21$ID %in% sb.sites, 3:14]))
    m22 <- as.vector(as.matrix(DF22[DF22$ID %in% sb.sites, 3:14]))
    m23 <- as.vector(as.matrix(DF23[DF23$ID %in% sb.sites, 3:14]))
    m24 <- as.vector(as.matrix(DF24[DF24$ID %in% sb.sites, 3:14]))
    m25 <- as.vector(as.matrix(DF25[DF25$ID %in% sb.sites, 3:14]))
    m26 <- as.vector(as.matrix(DF26[DF26$ID %in% sb.sites, 3:14]))
    m27 <- as.vector(as.matrix(DF27[DF27$ID %in% sb.sites, 3:14]))
    m28 <- as.vector(as.matrix(DF28[DF28$ID %in% sb.sites, 3:14]))
    m29 <- as.vector(as.matrix(DF29[DF29$ID %in% sb.sites, 3:14]))
    m30 <- as.vector(as.matrix(DF30[DF30$ID %in% sb.sites, 3:14]))
    
    m31 <- as.vector(as.matrix(DF31[DF31$ID %in% sb.sites, 3:14]))
    m32 <- as.vector(as.matrix(DF32[DF32$ID %in% sb.sites, 3:14]))
    m33 <- as.vector(as.matrix(DF33[DF33$ID %in% sb.sites, 3:14]))
    m34 <- as.vector(as.matrix(DF34[DF34$ID %in% sb.sites, 3:14]))
    m35 <- as.vector(as.matrix(DF35[DF35$ID %in% sb.sites, 3:14]))
    m36 <- as.vector(as.matrix(DF36[DF36$ID %in% sb.sites, 3:14]))
    m37 <- as.vector(as.matrix(DF37[DF37$ID %in% sb.sites, 3:14]))
    m38 <- as.vector(as.matrix(DF38[DF38$ID %in% sb.sites, 3:14]))
    m39 <- as.vector(as.matrix(DF39[DF39$ID %in% sb.sites, 3:14]))
    m40 <- as.vector(as.matrix(DF40[DF40$ID %in% sb.sites, 3:14]))
    
    m41 <- as.vector(as.matrix(DF41[DF41$ID %in% sb.sites, 3:14]))
    m42 <- as.vector(as.matrix(DF42[DF42$ID %in% sb.sites, 3:14]))
    m43 <- as.vector(as.matrix(DF43[DF43$ID %in% sb.sites, 3:14]))
    m44 <- as.vector(as.matrix(DF44[DF44$ID %in% sb.sites, 3:14]))
    m45 <- as.vector(as.matrix(DF45[DF45$ID %in% sb.sites, 3:14]))
    m46 <- as.vector(as.matrix(DF46[DF46$ID %in% sb.sites, 3:14]))
    m47 <- as.vector(as.matrix(DF47[DF47$ID %in% sb.sites, 3:14]))
    m48 <- as.vector(as.matrix(DF48[DF48$ID %in% sb.sites, 3:14]))
    m49 <- as.vector(as.matrix(DF49[DF49$ID %in% sb.sites, 3:14]))
    m50 <- as.vector(as.matrix(DF50[DF50$ID %in% sb.sites, 3:14]))
    
    m51 <- as.vector(as.matrix(DF51[DF51$ID %in% sb.sites, 3:14]))
    m52 <- as.vector(as.matrix(DF52[DF52$ID %in% sb.sites, 3:14]))
    m53 <- as.vector(as.matrix(DF53[DF53$ID %in% sb.sites, 3:14]))
    m54 <- as.vector(as.matrix(DF54[DF54$ID %in% sb.sites, 3:14]))
    m55 <- as.vector(as.matrix(DF55[DF55$ID %in% sb.sites, 3:14]))
    m56 <- as.vector(as.matrix(DF56[DF56$ID %in% sb.sites, 3:14]))
    m57 <- as.vector(as.matrix(DF57[DF57$ID %in% sb.sites, 3:14]))
    m58 <- as.vector(as.matrix(DF58[DF58$ID %in% sb.sites, 3:14]))
    m59 <- as.vector(as.matrix(DF59[DF59$ID %in% sb.sites, 3:14]))
    m60 <- as.vector(as.matrix(DF60[DF60$ID %in% sb.sites, 3:14]))
    
    m61 <- as.vector(as.matrix(DF61[DF61$ID %in% sb.sites, 3:14]))
    m62 <- as.vector(as.matrix(DF62[DF62$ID %in% sb.sites, 3:14]))
    m63 <- as.vector(as.matrix(DF63[DF63$ID %in% sb.sites, 3:14]))
    m64 <- as.vector(as.matrix(DF64[DF64$ID %in% sb.sites, 3:14]))
    m65 <- as.vector(as.matrix(DF65[DF65$ID %in% sb.sites, 3:14]))
    m66 <- as.vector(as.matrix(DF66[DF66$ID %in% sb.sites, 3:14]))
    m67 <- as.vector(as.matrix(DF67[DF67$ID %in% sb.sites, 3:14]))
    m68 <- as.vector(as.matrix(DF68[DF68$ID %in% sb.sites, 3:14]))
    m69 <- as.vector(as.matrix(DF69[DF69$ID %in% sb.sites, 3:14]))
    m70 <- as.vector(as.matrix(DF70[DF70$ID %in% sb.sites, 3:14]))
    
    m71 <- as.vector(as.matrix(DF71[DF71$ID %in% sb.sites, 3:14]))
    m72 <- as.vector(as.matrix(DF72[DF72$ID %in% sb.sites, 3:14]))
    m73 <- as.vector(as.matrix(DF73[DF73$ID %in% sb.sites, 3:14]))
    m74 <- as.vector(as.matrix(DF74[DF74$ID %in% sb.sites, 3:14]))
    m75 <- as.vector(as.matrix(DF75[DF75$ID %in% sb.sites, 3:14]))
    m76 <- as.vector(as.matrix(DF76[DF76$ID %in% sb.sites, 3:14]))
    m77 <- as.vector(as.matrix(DF77[DF77$ID %in% sb.sites, 3:14]))
    m78 <- as.vector(as.matrix(DF78[DF78$ID %in% sb.sites, 3:14]))
    m79 <- as.vector(as.matrix(DF79[DF79$ID %in% sb.sites, 3:14]))
    m80 <- as.vector(as.matrix(DF80[DF80$ID %in% sb.sites, 3:14]))
    
    sb.data <- c(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10,
                 m11, m12, m13, m14, m15, m16, m17, m18, m19, m20,
                 m21, m22, m23, m24, m25, m26, m27, m28, m29, m30,
                 m31, m32, m33, m34, m35, m36, m37, m38, m39, m40,
                 m41, m42, m43, m44, m45, m46, m47, m48, m49, m50,
                 m51, m52, m53, m54, m55, m56, m57, m58, m59, m60,
                 m61, m62, m63, m64, m65, m66, m67, m68, m69, m70,
                 m71, m72, m73, m74, m75, m76, m77, m78, m79, m80)
    
    sb.data <- sb.data[sb.data>0]
    
    ### Combine monthly rainfall data for desert sites
    m1 <- as.vector(as.matrix(DF1[DF1$ID %in% ds.sites, 3:14]))
    m2 <- as.vector(as.matrix(DF2[DF2$ID %in% ds.sites, 3:14]))
    m3 <- as.vector(as.matrix(DF3[DF3$ID %in% ds.sites, 3:14]))
    m4 <- as.vector(as.matrix(DF4[DF4$ID %in% ds.sites, 3:14]))
    m5 <- as.vector(as.matrix(DF5[DF5$ID %in% ds.sites, 3:14]))
    m6 <- as.vector(as.matrix(DF6[DF6$ID %in% ds.sites, 3:14]))
    m7 <- as.vector(as.matrix(DF7[DF7$ID %in% ds.sites, 3:14]))
    m8 <- as.vector(as.matrix(DF8[DF8$ID %in% ds.sites, 3:14]))
    m9 <- as.vector(as.matrix(DF9[DF9$ID %in% ds.sites, 3:14]))
    m10 <- as.vector(as.matrix(DF10[DF10$ID %in% ds.sites, 3:14]))
    
    m11 <- as.vector(as.matrix(DF11[DF11$ID %in% ds.sites, 3:14]))
    m12 <- as.vector(as.matrix(DF12[DF12$ID %in% ds.sites, 3:14]))
    m13 <- as.vector(as.matrix(DF13[DF13$ID %in% ds.sites, 3:14]))
    m14 <- as.vector(as.matrix(DF14[DF14$ID %in% ds.sites, 3:14]))
    m15 <- as.vector(as.matrix(DF15[DF15$ID %in% ds.sites, 3:14]))
    m16 <- as.vector(as.matrix(DF16[DF16$ID %in% ds.sites, 3:14]))
    m17 <- as.vector(as.matrix(DF17[DF17$ID %in% ds.sites, 3:14]))
    m18 <- as.vector(as.matrix(DF18[DF18$ID %in% ds.sites, 3:14]))
    m19 <- as.vector(as.matrix(DF19[DF19$ID %in% ds.sites, 3:14]))
    m20 <- as.vector(as.matrix(DF20[DF20$ID %in% ds.sites, 3:14]))
    
    m21 <- as.vector(as.matrix(DF21[DF21$ID %in% ds.sites, 3:14]))
    m22 <- as.vector(as.matrix(DF22[DF22$ID %in% ds.sites, 3:14]))
    m23 <- as.vector(as.matrix(DF23[DF23$ID %in% ds.sites, 3:14]))
    m24 <- as.vector(as.matrix(DF24[DF24$ID %in% ds.sites, 3:14]))
    m25 <- as.vector(as.matrix(DF25[DF25$ID %in% ds.sites, 3:14]))
    m26 <- as.vector(as.matrix(DF26[DF26$ID %in% ds.sites, 3:14]))
    m27 <- as.vector(as.matrix(DF27[DF27$ID %in% ds.sites, 3:14]))
    m28 <- as.vector(as.matrix(DF28[DF28$ID %in% ds.sites, 3:14]))
    m29 <- as.vector(as.matrix(DF29[DF29$ID %in% ds.sites, 3:14]))
    m30 <- as.vector(as.matrix(DF30[DF30$ID %in% ds.sites, 3:14]))
    
    m31 <- as.vector(as.matrix(DF31[DF31$ID %in% ds.sites, 3:14]))
    m32 <- as.vector(as.matrix(DF32[DF32$ID %in% ds.sites, 3:14]))
    m33 <- as.vector(as.matrix(DF33[DF33$ID %in% ds.sites, 3:14]))
    m34 <- as.vector(as.matrix(DF34[DF34$ID %in% ds.sites, 3:14]))
    m35 <- as.vector(as.matrix(DF35[DF35$ID %in% ds.sites, 3:14]))
    m36 <- as.vector(as.matrix(DF36[DF36$ID %in% ds.sites, 3:14]))
    m37 <- as.vector(as.matrix(DF37[DF37$ID %in% ds.sites, 3:14]))
    m38 <- as.vector(as.matrix(DF38[DF38$ID %in% ds.sites, 3:14]))
    m39 <- as.vector(as.matrix(DF39[DF39$ID %in% ds.sites, 3:14]))
    m40 <- as.vector(as.matrix(DF40[DF40$ID %in% ds.sites, 3:14]))
    
    m41 <- as.vector(as.matrix(DF41[DF41$ID %in% ds.sites, 3:14]))
    m42 <- as.vector(as.matrix(DF42[DF42$ID %in% ds.sites, 3:14]))
    m43 <- as.vector(as.matrix(DF43[DF43$ID %in% ds.sites, 3:14]))
    m44 <- as.vector(as.matrix(DF44[DF44$ID %in% ds.sites, 3:14]))
    m45 <- as.vector(as.matrix(DF45[DF45$ID %in% ds.sites, 3:14]))
    m46 <- as.vector(as.matrix(DF46[DF46$ID %in% ds.sites, 3:14]))
    m47 <- as.vector(as.matrix(DF47[DF47$ID %in% ds.sites, 3:14]))
    m48 <- as.vector(as.matrix(DF48[DF48$ID %in% ds.sites, 3:14]))
    m49 <- as.vector(as.matrix(DF49[DF49$ID %in% ds.sites, 3:14]))
    m50 <- as.vector(as.matrix(DF50[DF50$ID %in% ds.sites, 3:14]))
    
    m51 <- as.vector(as.matrix(DF51[DF51$ID %in% ds.sites, 3:14]))
    m52 <- as.vector(as.matrix(DF52[DF52$ID %in% ds.sites, 3:14]))
    m53 <- as.vector(as.matrix(DF53[DF53$ID %in% ds.sites, 3:14]))
    m54 <- as.vector(as.matrix(DF54[DF54$ID %in% ds.sites, 3:14]))
    m55 <- as.vector(as.matrix(DF55[DF55$ID %in% ds.sites, 3:14]))
    m56 <- as.vector(as.matrix(DF56[DF56$ID %in% ds.sites, 3:14]))
    m57 <- as.vector(as.matrix(DF57[DF57$ID %in% ds.sites, 3:14]))
    m58 <- as.vector(as.matrix(DF58[DF58$ID %in% ds.sites, 3:14]))
    m59 <- as.vector(as.matrix(DF59[DF59$ID %in% ds.sites, 3:14]))
    m60 <- as.vector(as.matrix(DF60[DF60$ID %in% ds.sites, 3:14]))
    
    m61 <- as.vector(as.matrix(DF61[DF61$ID %in% ds.sites, 3:14]))
    m62 <- as.vector(as.matrix(DF62[DF62$ID %in% ds.sites, 3:14]))
    m63 <- as.vector(as.matrix(DF63[DF63$ID %in% ds.sites, 3:14]))
    m64 <- as.vector(as.matrix(DF64[DF64$ID %in% ds.sites, 3:14]))
    m65 <- as.vector(as.matrix(DF65[DF65$ID %in% ds.sites, 3:14]))
    m66 <- as.vector(as.matrix(DF66[DF66$ID %in% ds.sites, 3:14]))
    m67 <- as.vector(as.matrix(DF67[DF67$ID %in% ds.sites, 3:14]))
    m68 <- as.vector(as.matrix(DF68[DF68$ID %in% ds.sites, 3:14]))
    m69 <- as.vector(as.matrix(DF69[DF69$ID %in% ds.sites, 3:14]))
    m70 <- as.vector(as.matrix(DF70[DF70$ID %in% ds.sites, 3:14]))
    
    m71 <- as.vector(as.matrix(DF71[DF71$ID %in% ds.sites, 3:14]))
    m72 <- as.vector(as.matrix(DF72[DF72$ID %in% ds.sites, 3:14]))
    m73 <- as.vector(as.matrix(DF73[DF73$ID %in% ds.sites, 3:14]))
    m74 <- as.vector(as.matrix(DF74[DF74$ID %in% ds.sites, 3:14]))
    m75 <- as.vector(as.matrix(DF75[DF75$ID %in% ds.sites, 3:14]))
    m76 <- as.vector(as.matrix(DF76[DF76$ID %in% ds.sites, 3:14]))
    m77 <- as.vector(as.matrix(DF77[DF77$ID %in% ds.sites, 3:14]))
    m78 <- as.vector(as.matrix(DF78[DF78$ID %in% ds.sites, 3:14]))
    m79 <- as.vector(as.matrix(DF79[DF79$ID %in% ds.sites, 3:14]))
    m80 <- as.vector(as.matrix(DF80[DF80$ID %in% ds.sites, 3:14]))
    
    ds.data <- c(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10,
                 m11, m12, m13, m14, m15, m16, m17, m18, m19, m20,
                 m21, m22, m23, m24, m25, m26, m27, m28, m29, m30,
                 m31, m32, m33, m34, m35, m36, m37, m38, m39, m40,
                 m41, m42, m43, m44, m45, m46, m47, m48, m49, m50,
                 m51, m52, m53, m54, m55, m56, m57, m58, m59, m60,
                 m61, m62, m63, m64, m65, m66, m67, m68, m69, m70,
                 m71, m72, m73, m74, m75, m76, m77, m78, m79, m80)
    
    ds.data <- ds.data[ds.data>0]
    
    ### Combine monthly rainfall data for grassland sites
    m1 <- as.vector(as.matrix(DF1[DF1$ID %in% gs.sites, 3:14]))
    m2 <- as.vector(as.matrix(DF2[DF2$ID %in% gs.sites, 3:14]))
    m3 <- as.vector(as.matrix(DF3[DF3$ID %in% gs.sites, 3:14]))
    m4 <- as.vector(as.matrix(DF4[DF4$ID %in% gs.sites, 3:14]))
    m5 <- as.vector(as.matrix(DF5[DF5$ID %in% gs.sites, 3:14]))
    m6 <- as.vector(as.matrix(DF6[DF6$ID %in% gs.sites, 3:14]))
    m7 <- as.vector(as.matrix(DF7[DF7$ID %in% gs.sites, 3:14]))
    m8 <- as.vector(as.matrix(DF8[DF8$ID %in% gs.sites, 3:14]))
    m9 <- as.vector(as.matrix(DF9[DF9$ID %in% gs.sites, 3:14]))
    m10 <- as.vector(as.matrix(DF10[DF10$ID %in% gs.sites, 3:14]))
    
    m11 <- as.vector(as.matrix(DF11[DF11$ID %in% gs.sites, 3:14]))
    m12 <- as.vector(as.matrix(DF12[DF12$ID %in% gs.sites, 3:14]))
    m13 <- as.vector(as.matrix(DF13[DF13$ID %in% gs.sites, 3:14]))
    m14 <- as.vector(as.matrix(DF14[DF14$ID %in% gs.sites, 3:14]))
    m15 <- as.vector(as.matrix(DF15[DF15$ID %in% gs.sites, 3:14]))
    m16 <- as.vector(as.matrix(DF16[DF16$ID %in% gs.sites, 3:14]))
    m17 <- as.vector(as.matrix(DF17[DF17$ID %in% gs.sites, 3:14]))
    m18 <- as.vector(as.matrix(DF18[DF18$ID %in% gs.sites, 3:14]))
    m19 <- as.vector(as.matrix(DF19[DF19$ID %in% gs.sites, 3:14]))
    m20 <- as.vector(as.matrix(DF20[DF20$ID %in% gs.sites, 3:14]))
    
    m21 <- as.vector(as.matrix(DF21[DF21$ID %in% gs.sites, 3:14]))
    m22 <- as.vector(as.matrix(DF22[DF22$ID %in% gs.sites, 3:14]))
    m23 <- as.vector(as.matrix(DF23[DF23$ID %in% gs.sites, 3:14]))
    m24 <- as.vector(as.matrix(DF24[DF24$ID %in% gs.sites, 3:14]))
    m25 <- as.vector(as.matrix(DF25[DF25$ID %in% gs.sites, 3:14]))
    m26 <- as.vector(as.matrix(DF26[DF26$ID %in% gs.sites, 3:14]))
    m27 <- as.vector(as.matrix(DF27[DF27$ID %in% gs.sites, 3:14]))
    m28 <- as.vector(as.matrix(DF28[DF28$ID %in% gs.sites, 3:14]))
    m29 <- as.vector(as.matrix(DF29[DF29$ID %in% gs.sites, 3:14]))
    m30 <- as.vector(as.matrix(DF30[DF30$ID %in% gs.sites, 3:14]))
    
    m31 <- as.vector(as.matrix(DF31[DF31$ID %in% gs.sites, 3:14]))
    m32 <- as.vector(as.matrix(DF32[DF32$ID %in% gs.sites, 3:14]))
    m33 <- as.vector(as.matrix(DF33[DF33$ID %in% gs.sites, 3:14]))
    m34 <- as.vector(as.matrix(DF34[DF34$ID %in% gs.sites, 3:14]))
    m35 <- as.vector(as.matrix(DF35[DF35$ID %in% gs.sites, 3:14]))
    m36 <- as.vector(as.matrix(DF36[DF36$ID %in% gs.sites, 3:14]))
    m37 <- as.vector(as.matrix(DF37[DF37$ID %in% gs.sites, 3:14]))
    m38 <- as.vector(as.matrix(DF38[DF38$ID %in% gs.sites, 3:14]))
    m39 <- as.vector(as.matrix(DF39[DF39$ID %in% gs.sites, 3:14]))
    m40 <- as.vector(as.matrix(DF40[DF40$ID %in% gs.sites, 3:14]))
    
    m41 <- as.vector(as.matrix(DF41[DF41$ID %in% gs.sites, 3:14]))
    m42 <- as.vector(as.matrix(DF42[DF42$ID %in% gs.sites, 3:14]))
    m43 <- as.vector(as.matrix(DF43[DF43$ID %in% gs.sites, 3:14]))
    m44 <- as.vector(as.matrix(DF44[DF44$ID %in% gs.sites, 3:14]))
    m45 <- as.vector(as.matrix(DF45[DF45$ID %in% gs.sites, 3:14]))
    m46 <- as.vector(as.matrix(DF46[DF46$ID %in% gs.sites, 3:14]))
    m47 <- as.vector(as.matrix(DF47[DF47$ID %in% gs.sites, 3:14]))
    m48 <- as.vector(as.matrix(DF48[DF48$ID %in% gs.sites, 3:14]))
    m49 <- as.vector(as.matrix(DF49[DF49$ID %in% gs.sites, 3:14]))
    m50 <- as.vector(as.matrix(DF50[DF50$ID %in% gs.sites, 3:14]))
    
    m51 <- as.vector(as.matrix(DF51[DF51$ID %in% gs.sites, 3:14]))
    m52 <- as.vector(as.matrix(DF52[DF52$ID %in% gs.sites, 3:14]))
    m53 <- as.vector(as.matrix(DF53[DF53$ID %in% gs.sites, 3:14]))
    m54 <- as.vector(as.matrix(DF54[DF54$ID %in% gs.sites, 3:14]))
    m55 <- as.vector(as.matrix(DF55[DF55$ID %in% gs.sites, 3:14]))
    m56 <- as.vector(as.matrix(DF56[DF56$ID %in% gs.sites, 3:14]))
    m57 <- as.vector(as.matrix(DF57[DF57$ID %in% gs.sites, 3:14]))
    m58 <- as.vector(as.matrix(DF58[DF58$ID %in% gs.sites, 3:14]))
    m59 <- as.vector(as.matrix(DF59[DF59$ID %in% gs.sites, 3:14]))
    m60 <- as.vector(as.matrix(DF60[DF60$ID %in% gs.sites, 3:14]))
    
    m61 <- as.vector(as.matrix(DF61[DF61$ID %in% gs.sites, 3:14]))
    m62 <- as.vector(as.matrix(DF62[DF62$ID %in% gs.sites, 3:14]))
    m63 <- as.vector(as.matrix(DF63[DF63$ID %in% gs.sites, 3:14]))
    m64 <- as.vector(as.matrix(DF64[DF64$ID %in% gs.sites, 3:14]))
    m65 <- as.vector(as.matrix(DF65[DF65$ID %in% gs.sites, 3:14]))
    m66 <- as.vector(as.matrix(DF66[DF66$ID %in% gs.sites, 3:14]))
    m67 <- as.vector(as.matrix(DF67[DF67$ID %in% gs.sites, 3:14]))
    m68 <- as.vector(as.matrix(DF68[DF68$ID %in% gs.sites, 3:14]))
    m69 <- as.vector(as.matrix(DF69[DF69$ID %in% gs.sites, 3:14]))
    m70 <- as.vector(as.matrix(DF70[DF70$ID %in% gs.sites, 3:14]))
    
    m71 <- as.vector(as.matrix(DF71[DF71$ID %in% gs.sites, 3:14]))
    m72 <- as.vector(as.matrix(DF72[DF72$ID %in% gs.sites, 3:14]))
    m73 <- as.vector(as.matrix(DF73[DF73$ID %in% gs.sites, 3:14]))
    m74 <- as.vector(as.matrix(DF74[DF74$ID %in% gs.sites, 3:14]))
    m75 <- as.vector(as.matrix(DF75[DF75$ID %in% gs.sites, 3:14]))
    m76 <- as.vector(as.matrix(DF76[DF76$ID %in% gs.sites, 3:14]))
    m77 <- as.vector(as.matrix(DF77[DF77$ID %in% gs.sites, 3:14]))
    m78 <- as.vector(as.matrix(DF78[DF78$ID %in% gs.sites, 3:14]))
    m79 <- as.vector(as.matrix(DF79[DF79$ID %in% gs.sites, 3:14]))
    m80 <- as.vector(as.matrix(DF80[DF80$ID %in% gs.sites, 3:14]))
    
    gs.data <- c(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10,
                 m11, m12, m13, m14, m15, m16, m17, m18, m19, m20,
                 m21, m22, m23, m24, m25, m26, m27, m28, m29, m30,
                 m31, m32, m33, m34, m35, m36, m37, m38, m39, m40,
                 m41, m42, m43, m44, m45, m46, m47, m48, m49, m50,
                 m51, m52, m53, m54, m55, m56, m57, m58, m59, m60,
                 m61, m62, m63, m64, m65, m66, m67, m68, m69, m70,
                 m71, m72, m73, m74, m75, m76, m77, m78, m79, m80)
    
    gs.data <- gs.data[gs.data>0]
    
    ### Combine monthly rainfall data for temperate sites
    m1 <- as.vector(as.matrix(DF1[DF1$ID %in% tm.sites, 3:14]))
    m2 <- as.vector(as.matrix(DF2[DF2$ID %in% tm.sites, 3:14]))
    m3 <- as.vector(as.matrix(DF3[DF3$ID %in% tm.sites, 3:14]))
    m4 <- as.vector(as.matrix(DF4[DF4$ID %in% tm.sites, 3:14]))
    m5 <- as.vector(as.matrix(DF5[DF5$ID %in% tm.sites, 3:14]))
    m6 <- as.vector(as.matrix(DF6[DF6$ID %in% tm.sites, 3:14]))
    m7 <- as.vector(as.matrix(DF7[DF7$ID %in% tm.sites, 3:14]))
    m8 <- as.vector(as.matrix(DF8[DF8$ID %in% tm.sites, 3:14]))
    m9 <- as.vector(as.matrix(DF9[DF9$ID %in% tm.sites, 3:14]))
    m10 <- as.vector(as.matrix(DF10[DF10$ID %in% tm.sites, 3:14]))
    
    m11 <- as.vector(as.matrix(DF11[DF11$ID %in% tm.sites, 3:14]))
    m12 <- as.vector(as.matrix(DF12[DF12$ID %in% tm.sites, 3:14]))
    m13 <- as.vector(as.matrix(DF13[DF13$ID %in% tm.sites, 3:14]))
    m14 <- as.vector(as.matrix(DF14[DF14$ID %in% tm.sites, 3:14]))
    m15 <- as.vector(as.matrix(DF15[DF15$ID %in% tm.sites, 3:14]))
    m16 <- as.vector(as.matrix(DF16[DF16$ID %in% tm.sites, 3:14]))
    m17 <- as.vector(as.matrix(DF17[DF17$ID %in% tm.sites, 3:14]))
    m18 <- as.vector(as.matrix(DF18[DF18$ID %in% tm.sites, 3:14]))
    m19 <- as.vector(as.matrix(DF19[DF19$ID %in% tm.sites, 3:14]))
    m20 <- as.vector(as.matrix(DF20[DF20$ID %in% tm.sites, 3:14]))
    
    m21 <- as.vector(as.matrix(DF21[DF21$ID %in% tm.sites, 3:14]))
    m22 <- as.vector(as.matrix(DF22[DF22$ID %in% tm.sites, 3:14]))
    m23 <- as.vector(as.matrix(DF23[DF23$ID %in% tm.sites, 3:14]))
    m24 <- as.vector(as.matrix(DF24[DF24$ID %in% tm.sites, 3:14]))
    m25 <- as.vector(as.matrix(DF25[DF25$ID %in% tm.sites, 3:14]))
    m26 <- as.vector(as.matrix(DF26[DF26$ID %in% tm.sites, 3:14]))
    m27 <- as.vector(as.matrix(DF27[DF27$ID %in% tm.sites, 3:14]))
    m28 <- as.vector(as.matrix(DF28[DF28$ID %in% tm.sites, 3:14]))
    m29 <- as.vector(as.matrix(DF29[DF29$ID %in% tm.sites, 3:14]))
    m30 <- as.vector(as.matrix(DF30[DF30$ID %in% tm.sites, 3:14]))
    
    m31 <- as.vector(as.matrix(DF31[DF31$ID %in% tm.sites, 3:14]))
    m32 <- as.vector(as.matrix(DF32[DF32$ID %in% tm.sites, 3:14]))
    m33 <- as.vector(as.matrix(DF33[DF33$ID %in% tm.sites, 3:14]))
    m34 <- as.vector(as.matrix(DF34[DF34$ID %in% tm.sites, 3:14]))
    m35 <- as.vector(as.matrix(DF35[DF35$ID %in% tm.sites, 3:14]))
    m36 <- as.vector(as.matrix(DF36[DF36$ID %in% tm.sites, 3:14]))
    m37 <- as.vector(as.matrix(DF37[DF37$ID %in% tm.sites, 3:14]))
    m38 <- as.vector(as.matrix(DF38[DF38$ID %in% tm.sites, 3:14]))
    m39 <- as.vector(as.matrix(DF39[DF39$ID %in% tm.sites, 3:14]))
    m40 <- as.vector(as.matrix(DF40[DF40$ID %in% tm.sites, 3:14]))
    
    m41 <- as.vector(as.matrix(DF41[DF41$ID %in% tm.sites, 3:14]))
    m42 <- as.vector(as.matrix(DF42[DF42$ID %in% tm.sites, 3:14]))
    m43 <- as.vector(as.matrix(DF43[DF43$ID %in% tm.sites, 3:14]))
    m44 <- as.vector(as.matrix(DF44[DF44$ID %in% tm.sites, 3:14]))
    m45 <- as.vector(as.matrix(DF45[DF45$ID %in% tm.sites, 3:14]))
    m46 <- as.vector(as.matrix(DF46[DF46$ID %in% tm.sites, 3:14]))
    m47 <- as.vector(as.matrix(DF47[DF47$ID %in% tm.sites, 3:14]))
    m48 <- as.vector(as.matrix(DF48[DF48$ID %in% tm.sites, 3:14]))
    m49 <- as.vector(as.matrix(DF49[DF49$ID %in% tm.sites, 3:14]))
    m50 <- as.vector(as.matrix(DF50[DF50$ID %in% tm.sites, 3:14]))
    
    m51 <- as.vector(as.matrix(DF51[DF51$ID %in% tm.sites, 3:14]))
    m52 <- as.vector(as.matrix(DF52[DF52$ID %in% tm.sites, 3:14]))
    m53 <- as.vector(as.matrix(DF53[DF53$ID %in% tm.sites, 3:14]))
    m54 <- as.vector(as.matrix(DF54[DF54$ID %in% tm.sites, 3:14]))
    m55 <- as.vector(as.matrix(DF55[DF55$ID %in% tm.sites, 3:14]))
    m56 <- as.vector(as.matrix(DF56[DF56$ID %in% tm.sites, 3:14]))
    m57 <- as.vector(as.matrix(DF57[DF57$ID %in% tm.sites, 3:14]))
    m58 <- as.vector(as.matrix(DF58[DF58$ID %in% tm.sites, 3:14]))
    m59 <- as.vector(as.matrix(DF59[DF59$ID %in% tm.sites, 3:14]))
    m60 <- as.vector(as.matrix(DF60[DF60$ID %in% tm.sites, 3:14]))
    
    m61 <- as.vector(as.matrix(DF61[DF61$ID %in% tm.sites, 3:14]))
    m62 <- as.vector(as.matrix(DF62[DF62$ID %in% tm.sites, 3:14]))
    m63 <- as.vector(as.matrix(DF63[DF63$ID %in% tm.sites, 3:14]))
    m64 <- as.vector(as.matrix(DF64[DF64$ID %in% tm.sites, 3:14]))
    m65 <- as.vector(as.matrix(DF65[DF65$ID %in% tm.sites, 3:14]))
    m66 <- as.vector(as.matrix(DF66[DF66$ID %in% tm.sites, 3:14]))
    m67 <- as.vector(as.matrix(DF67[DF67$ID %in% tm.sites, 3:14]))
    m68 <- as.vector(as.matrix(DF68[DF68$ID %in% tm.sites, 3:14]))
    m69 <- as.vector(as.matrix(DF69[DF69$ID %in% tm.sites, 3:14]))
    m70 <- as.vector(as.matrix(DF70[DF70$ID %in% tm.sites, 3:14]))
    
    m71 <- as.vector(as.matrix(DF71[DF71$ID %in% tm.sites, 3:14]))
    m72 <- as.vector(as.matrix(DF72[DF72$ID %in% tm.sites, 3:14]))
    m73 <- as.vector(as.matrix(DF73[DF73$ID %in% tm.sites, 3:14]))
    m74 <- as.vector(as.matrix(DF74[DF74$ID %in% tm.sites, 3:14]))
    m75 <- as.vector(as.matrix(DF75[DF75$ID %in% tm.sites, 3:14]))
    m76 <- as.vector(as.matrix(DF76[DF76$ID %in% tm.sites, 3:14]))
    m77 <- as.vector(as.matrix(DF77[DF77$ID %in% tm.sites, 3:14]))
    m78 <- as.vector(as.matrix(DF78[DF78$ID %in% tm.sites, 3:14]))
    m79 <- as.vector(as.matrix(DF79[DF79$ID %in% tm.sites, 3:14]))
    m80 <- as.vector(as.matrix(DF80[DF80$ID %in% tm.sites, 3:14]))
    
    tm.data <- c(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10,
                 m11, m12, m13, m14, m15, m16, m17, m18, m19, m20,
                 m21, m22, m23, m24, m25, m26, m27, m28, m29, m30,
                 m31, m32, m33, m34, m35, m36, m37, m38, m39, m40,
                 m41, m42, m43, m44, m45, m46, m47, m48, m49, m50,
                 m51, m52, m53, m54, m55, m56, m57, m58, m59, m60,
                 m61, m62, m63, m64, m65, m66, m67, m68, m69, m70,
                 m71, m72, m73, m74, m75, m76, m77, m78, m79, m80)
    
    tm.data <- tm.data[tm.data>0]
    
    ### Combine monthly rainfall data for NA sites
    m1 <- as.vector(as.matrix(DF1[DF1$ID %in% na.sites, 3:14]))
    m2 <- as.vector(as.matrix(DF2[DF2$ID %in% na.sites, 3:14]))
    m3 <- as.vector(as.matrix(DF3[DF3$ID %in% na.sites, 3:14]))
    m4 <- as.vector(as.matrix(DF4[DF4$ID %in% na.sites, 3:14]))
    m5 <- as.vector(as.matrix(DF5[DF5$ID %in% na.sites, 3:14]))
    m6 <- as.vector(as.matrix(DF6[DF6$ID %in% na.sites, 3:14]))
    m7 <- as.vector(as.matrix(DF7[DF7$ID %in% na.sites, 3:14]))
    m8 <- as.vector(as.matrix(DF8[DF8$ID %in% na.sites, 3:14]))
    m9 <- as.vector(as.matrix(DF9[DF9$ID %in% na.sites, 3:14]))
    m10 <- as.vector(as.matrix(DF10[DF10$ID %in% na.sites, 3:14]))
    
    m11 <- as.vector(as.matrix(DF11[DF11$ID %in% na.sites, 3:14]))
    m12 <- as.vector(as.matrix(DF12[DF12$ID %in% na.sites, 3:14]))
    m13 <- as.vector(as.matrix(DF13[DF13$ID %in% na.sites, 3:14]))
    m14 <- as.vector(as.matrix(DF14[DF14$ID %in% na.sites, 3:14]))
    m15 <- as.vector(as.matrix(DF15[DF15$ID %in% na.sites, 3:14]))
    m16 <- as.vector(as.matrix(DF16[DF16$ID %in% na.sites, 3:14]))
    m17 <- as.vector(as.matrix(DF17[DF17$ID %in% na.sites, 3:14]))
    m18 <- as.vector(as.matrix(DF18[DF18$ID %in% na.sites, 3:14]))
    m19 <- as.vector(as.matrix(DF19[DF19$ID %in% na.sites, 3:14]))
    m20 <- as.vector(as.matrix(DF20[DF20$ID %in% na.sites, 3:14]))
    
    m21 <- as.vector(as.matrix(DF21[DF21$ID %in% na.sites, 3:14]))
    m22 <- as.vector(as.matrix(DF22[DF22$ID %in% na.sites, 3:14]))
    m23 <- as.vector(as.matrix(DF23[DF23$ID %in% na.sites, 3:14]))
    m24 <- as.vector(as.matrix(DF24[DF24$ID %in% na.sites, 3:14]))
    m25 <- as.vector(as.matrix(DF25[DF25$ID %in% na.sites, 3:14]))
    m26 <- as.vector(as.matrix(DF26[DF26$ID %in% na.sites, 3:14]))
    m27 <- as.vector(as.matrix(DF27[DF27$ID %in% na.sites, 3:14]))
    m28 <- as.vector(as.matrix(DF28[DF28$ID %in% na.sites, 3:14]))
    m29 <- as.vector(as.matrix(DF29[DF29$ID %in% na.sites, 3:14]))
    m30 <- as.vector(as.matrix(DF30[DF30$ID %in% na.sites, 3:14]))
    
    m31 <- as.vector(as.matrix(DF31[DF31$ID %in% na.sites, 3:14]))
    m32 <- as.vector(as.matrix(DF32[DF32$ID %in% na.sites, 3:14]))
    m33 <- as.vector(as.matrix(DF33[DF33$ID %in% na.sites, 3:14]))
    m34 <- as.vector(as.matrix(DF34[DF34$ID %in% na.sites, 3:14]))
    m35 <- as.vector(as.matrix(DF35[DF35$ID %in% na.sites, 3:14]))
    m36 <- as.vector(as.matrix(DF36[DF36$ID %in% na.sites, 3:14]))
    m37 <- as.vector(as.matrix(DF37[DF37$ID %in% na.sites, 3:14]))
    m38 <- as.vector(as.matrix(DF38[DF38$ID %in% na.sites, 3:14]))
    m39 <- as.vector(as.matrix(DF39[DF39$ID %in% na.sites, 3:14]))
    m40 <- as.vector(as.matrix(DF40[DF40$ID %in% na.sites, 3:14]))
    
    m41 <- as.vector(as.matrix(DF41[DF41$ID %in% na.sites, 3:14]))
    m42 <- as.vector(as.matrix(DF42[DF42$ID %in% na.sites, 3:14]))
    m43 <- as.vector(as.matrix(DF43[DF43$ID %in% na.sites, 3:14]))
    m44 <- as.vector(as.matrix(DF44[DF44$ID %in% na.sites, 3:14]))
    m45 <- as.vector(as.matrix(DF45[DF45$ID %in% na.sites, 3:14]))
    m46 <- as.vector(as.matrix(DF46[DF46$ID %in% na.sites, 3:14]))
    m47 <- as.vector(as.matrix(DF47[DF47$ID %in% na.sites, 3:14]))
    m48 <- as.vector(as.matrix(DF48[DF48$ID %in% na.sites, 3:14]))
    m49 <- as.vector(as.matrix(DF49[DF49$ID %in% na.sites, 3:14]))
    m50 <- as.vector(as.matrix(DF50[DF50$ID %in% na.sites, 3:14]))
    
    m51 <- as.vector(as.matrix(DF51[DF51$ID %in% na.sites, 3:14]))
    m52 <- as.vector(as.matrix(DF52[DF52$ID %in% na.sites, 3:14]))
    m53 <- as.vector(as.matrix(DF53[DF53$ID %in% na.sites, 3:14]))
    m54 <- as.vector(as.matrix(DF54[DF54$ID %in% na.sites, 3:14]))
    m55 <- as.vector(as.matrix(DF55[DF55$ID %in% na.sites, 3:14]))
    m56 <- as.vector(as.matrix(DF56[DF56$ID %in% na.sites, 3:14]))
    m57 <- as.vector(as.matrix(DF57[DF57$ID %in% na.sites, 3:14]))
    m58 <- as.vector(as.matrix(DF58[DF58$ID %in% na.sites, 3:14]))
    m59 <- as.vector(as.matrix(DF59[DF59$ID %in% na.sites, 3:14]))
    m60 <- as.vector(as.matrix(DF60[DF60$ID %in% na.sites, 3:14]))
    
    m61 <- as.vector(as.matrix(DF61[DF61$ID %in% na.sites, 3:14]))
    m62 <- as.vector(as.matrix(DF62[DF62$ID %in% na.sites, 3:14]))
    m63 <- as.vector(as.matrix(DF63[DF63$ID %in% na.sites, 3:14]))
    m64 <- as.vector(as.matrix(DF64[DF64$ID %in% na.sites, 3:14]))
    m65 <- as.vector(as.matrix(DF65[DF65$ID %in% na.sites, 3:14]))
    m66 <- as.vector(as.matrix(DF66[DF66$ID %in% na.sites, 3:14]))
    m67 <- as.vector(as.matrix(DF67[DF67$ID %in% na.sites, 3:14]))
    m68 <- as.vector(as.matrix(DF68[DF68$ID %in% na.sites, 3:14]))
    m69 <- as.vector(as.matrix(DF69[DF69$ID %in% na.sites, 3:14]))
    m70 <- as.vector(as.matrix(DF70[DF70$ID %in% na.sites, 3:14]))
    
    m71 <- as.vector(as.matrix(DF71[DF71$ID %in% na.sites, 3:14]))
    m72 <- as.vector(as.matrix(DF72[DF72$ID %in% na.sites, 3:14]))
    m73 <- as.vector(as.matrix(DF73[DF73$ID %in% na.sites, 3:14]))
    m74 <- as.vector(as.matrix(DF74[DF74$ID %in% na.sites, 3:14]))
    m75 <- as.vector(as.matrix(DF75[DF75$ID %in% na.sites, 3:14]))
    m76 <- as.vector(as.matrix(DF76[DF76$ID %in% na.sites, 3:14]))
    m77 <- as.vector(as.matrix(DF77[DF77$ID %in% na.sites, 3:14]))
    m78 <- as.vector(as.matrix(DF78[DF78$ID %in% na.sites, 3:14]))
    m79 <- as.vector(as.matrix(DF79[DF79$ID %in% na.sites, 3:14]))
    m80 <- as.vector(as.matrix(DF80[DF80$ID %in% na.sites, 3:14]))
    
    na.data <- c(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10,
                 m11, m12, m13, m14, m15, m16, m17, m18, m19, m20,
                 m21, m22, m23, m24, m25, m26, m27, m28, m29, m30,
                 m31, m32, m33, m34, m35, m36, m37, m38, m39, m40,
                 m41, m42, m43, m44, m45, m46, m47, m48, m49, m50,
                 m51, m52, m53, m54, m55, m56, m57, m58, m59, m60,
                 m61, m62, m63, m64, m65, m66, m67, m68, m69, m70,
                 m71, m72, m73, m74, m75, m76, m77, m78, m79, m80)
    
    na.data <- na.data[na.data>0]
    
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