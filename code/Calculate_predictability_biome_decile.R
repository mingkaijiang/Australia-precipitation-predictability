Calculate_predictability_biome_decile<- function(sourceDir, destDir) {
    #### Input data in 3-d format of grid, grid, month 
    #### Output data in format of: site ID, year, jan, feb, ...., dec, ann, for each grid
    ####
    
    ### create destDir if not exists
    if(!dir.exists(destDir)) {
        dir.create(destDir, showWarnings = FALSE)
    }
  
    ### Get biome information
    biomeDF <- Calculate_biome_specific_deciles(sourceDir=sourceDir, 
                                                destDir=destDir,
                                                return.decision="decile")
  
    
    ### Read in all files in the input directory
    filenames <- list.files(sourceDir, pattern="*.rds", full.names=TRUE)
    
    for (i in seq_along(filenames)) {
        assign(paste0("DF", i), readRDS(filenames[i]))
    }
    
    ### Create temporary DF
    tmpDF <- matrix(ncol = 13, nrow = 90)
    tmpDF <- as.data.frame(tmpDF)
    colnames(tmpDF) <- c("Year", "Jan", "Feb", "Mar", "Apr", "May",
                       "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    tmpDF$Year <- c(1930:2019)
    ts <- tmpDF$Year
    
    ### Create a out df to store all data in one file
    out <- matrix(ncol=6, nrow=691*886)
    out <- as.data.frame(out, row.names = NULL, stringsAsFactors = FALSE)
    colnames(out) <- c("Site_ID","lon","lat", "P","C","M")
    out$Site_ID <- c(1:(691*886))
    out$lon <- DF1$lon
    out$lat <- DF1$lat
    
    ### Get some general information
    years <- min(ts)
    yeare <- max(ts)
    yearr <- yeare-years
    
    col_sum <- length(ts)   # total number count of years, i.e. 30
    whole_sum <- col_sum*12
    
    #uncertainty with respect to time H(X)
    HofX <- -((col_sum/whole_sum)*log10(col_sum/whole_sum))*12
    
    
    ### Create matrix to store the frequency table, without defining bin sizes
    interval <- 10
    s.val <- interval
    t.val <- 12
    
    bin <- matrix(0, ncol=14, nrow=interval)
    dimnames(bin) <- list(NULL,c("bin_size","Jan", "Feb", "Mar", "Apr", "May",
                                 "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "whole"))
    
    ### Get biome grids
    bDF <- Read_biome_grids()
    colnames(bDF)[which(names(bDF) == "id")] <- "Site_ID"
    
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
    
    ### biome numbers
    for (j in c(1, 2, 4, 7, 8, 10, 12, 13)) {
      
      biome.sites <- bDF$Site_ID[bDF$Biome==j]
      biome.sites <- biome.sites[!is.na(biome.sites)]

      decile.list <- biomeDF[biomeDF$Biome==j,2:12]
      
      max_top <- decile.list[11]
      min_bot <- decile.list[1]
      diff <- max_top - min_bot
      
      bin[,"bin_size"] <- as.vector(as.matrix(decile.list[2:11]))
      breaks = c(as.numeric(decile.list))
      
      ### biome sites, aka coordinate ID
      for (i in biome.sites) {
        
        ### Fill the temp DF, ready for calculating predictability
        tmpDF[tmpDF$Year == 1930, 2:13] <- DF1[i,3:14]
        tmpDF[tmpDF$Year == 1931, 2:13] <- DF2[i,3:14]
        tmpDF[tmpDF$Year == 1932, 2:13] <- DF3[i,3:14]
        tmpDF[tmpDF$Year == 1933, 2:13] <- DF4[i,3:14]
        tmpDF[tmpDF$Year == 1934, 2:13] <- DF5[i,3:14]
        tmpDF[tmpDF$Year == 1935, 2:13] <- DF6[i,3:14]
        tmpDF[tmpDF$Year == 1936, 2:13] <- DF7[i,3:14]
        tmpDF[tmpDF$Year == 1937, 2:13] <- DF8[i,3:14]
        tmpDF[tmpDF$Year == 1938, 2:13] <- DF9[i,3:14]
        tmpDF[tmpDF$Year == 1939, 2:13] <- DF10[i,3:14]
        
        tmpDF[tmpDF$Year == 1940, 2:13] <- DF11[i,3:14]
        tmpDF[tmpDF$Year == 1941, 2:13] <- DF12[i,3:14]
        tmpDF[tmpDF$Year == 1942, 2:13] <- DF13[i,3:14]
        tmpDF[tmpDF$Year == 1943, 2:13] <- DF14[i,3:14]
        tmpDF[tmpDF$Year == 1944, 2:13] <- DF15[i,3:14]
        tmpDF[tmpDF$Year == 1945, 2:13] <- DF16[i,3:14]
        tmpDF[tmpDF$Year == 1946, 2:13] <- DF17[i,3:14]
        tmpDF[tmpDF$Year == 1947, 2:13] <- DF18[i,3:14]
        tmpDF[tmpDF$Year == 1948, 2:13] <- DF19[i,3:14]
        tmpDF[tmpDF$Year == 1949, 2:13] <- DF20[i,3:14]
        
        tmpDF[tmpDF$Year == 1950, 2:13] <- DF21[i,3:14]
        tmpDF[tmpDF$Year == 1951, 2:13] <- DF22[i,3:14]
        tmpDF[tmpDF$Year == 1952, 2:13] <- DF23[i,3:14]
        tmpDF[tmpDF$Year == 1953, 2:13] <- DF24[i,3:14]
        tmpDF[tmpDF$Year == 1954, 2:13] <- DF25[i,3:14]
        tmpDF[tmpDF$Year == 1955, 2:13] <- DF26[i,3:14]
        tmpDF[tmpDF$Year == 1956, 2:13] <- DF27[i,3:14]
        tmpDF[tmpDF$Year == 1957, 2:13] <- DF28[i,3:14]
        tmpDF[tmpDF$Year == 1958, 2:13] <- DF29[i,3:14]
        tmpDF[tmpDF$Year == 1959, 2:13] <- DF30[i,3:14]
        
        tmpDF[tmpDF$Year == 1960, 2:13] <- DF31[i,3:14]
        tmpDF[tmpDF$Year == 1961, 2:13] <- DF32[i,3:14]
        tmpDF[tmpDF$Year == 1962, 2:13] <- DF33[i,3:14]
        tmpDF[tmpDF$Year == 1963, 2:13] <- DF34[i,3:14]
        tmpDF[tmpDF$Year == 1964, 2:13] <- DF35[i,3:14]
        tmpDF[tmpDF$Year == 1965, 2:13] <- DF36[i,3:14]
        tmpDF[tmpDF$Year == 1966, 2:13] <- DF37[i,3:14]
        tmpDF[tmpDF$Year == 1967, 2:13] <- DF38[i,3:14]
        tmpDF[tmpDF$Year == 1968, 2:13] <- DF39[i,3:14]
        tmpDF[tmpDF$Year == 1969, 2:13] <- DF40[i,3:14]
        
        tmpDF[tmpDF$Year == 1970, 2:13] <- DF41[i,3:14]
        tmpDF[tmpDF$Year == 1971, 2:13] <- DF42[i,3:14]
        tmpDF[tmpDF$Year == 1972, 2:13] <- DF43[i,3:14]
        tmpDF[tmpDF$Year == 1973, 2:13] <- DF44[i,3:14]
        tmpDF[tmpDF$Year == 1974, 2:13] <- DF45[i,3:14]
        tmpDF[tmpDF$Year == 1975, 2:13] <- DF46[i,3:14]
        tmpDF[tmpDF$Year == 1976, 2:13] <- DF47[i,3:14]
        tmpDF[tmpDF$Year == 1977, 2:13] <- DF48[i,3:14]
        tmpDF[tmpDF$Year == 1978, 2:13] <- DF49[i,3:14]
        tmpDF[tmpDF$Year == 1979, 2:13] <- DF50[i,3:14]
        
        tmpDF[tmpDF$Year == 1980, 2:13] <- DF51[i,3:14] 
        tmpDF[tmpDF$Year == 1981, 2:13] <- DF52[i,3:14]
        tmpDF[tmpDF$Year == 1982, 2:13] <- DF53[i,3:14]
        tmpDF[tmpDF$Year == 1983, 2:13] <- DF54[i,3:14]
        tmpDF[tmpDF$Year == 1984, 2:13] <- DF55[i,3:14]
        tmpDF[tmpDF$Year == 1985, 2:13] <- DF56[i,3:14]
        tmpDF[tmpDF$Year == 1986, 2:13] <- DF57[i,3:14]
        tmpDF[tmpDF$Year == 1987, 2:13] <- DF58[i,3:14]
        tmpDF[tmpDF$Year == 1988, 2:13] <- DF59[i,3:14]
        tmpDF[tmpDF$Year == 1989, 2:13] <- DF60[i,3:14]
        
        tmpDF[tmpDF$Year == 1990, 2:13] <- DF61[i,3:14]
        tmpDF[tmpDF$Year == 1991, 2:13] <- DF62[i,3:14]
        tmpDF[tmpDF$Year == 1992, 2:13] <- DF63[i,3:14]
        tmpDF[tmpDF$Year == 1993, 2:13] <- DF64[i,3:14]
        tmpDF[tmpDF$Year == 1994, 2:13] <- DF65[i,3:14]
        tmpDF[tmpDF$Year == 1995, 2:13] <- DF66[i,3:14]
        tmpDF[tmpDF$Year == 1996, 2:13] <- DF67[i,3:14]
        tmpDF[tmpDF$Year == 1997, 2:13] <- DF68[i,3:14]
        tmpDF[tmpDF$Year == 1998, 2:13] <- DF69[i,3:14]
        tmpDF[tmpDF$Year == 1999, 2:13] <- DF70[i,3:14]
        
        tmpDF[tmpDF$Year == 2000, 2:13] <- DF71[i,3:14]
        tmpDF[tmpDF$Year == 2001, 2:13] <- DF72[i,3:14]
        tmpDF[tmpDF$Year == 2002, 2:13] <- DF73[i,3:14]
        tmpDF[tmpDF$Year == 2003, 2:13] <- DF74[i,3:14]
        tmpDF[tmpDF$Year == 2004, 2:13] <- DF75[i,3:14]
        tmpDF[tmpDF$Year == 2005, 2:13] <- DF76[i,3:14]
        tmpDF[tmpDF$Year == 2006, 2:13] <- DF77[i,3:14]
        tmpDF[tmpDF$Year == 2007, 2:13] <- DF78[i,3:14]
        tmpDF[tmpDF$Year == 2008, 2:13] <- DF79[i,3:14]
        tmpDF[tmpDF$Year == 2009, 2:13] <- DF80[i,3:14]
        
        
        tmpDF[tmpDF$Year == 2010, 2:13] <- DF81[i,3:14]
        tmpDF[tmpDF$Year == 2011, 2:13] <- DF82[i,3:14]
        tmpDF[tmpDF$Year == 2012, 2:13] <- DF83[i,3:14]
        tmpDF[tmpDF$Year == 2013, 2:13] <- DF84[i,3:14]
        tmpDF[tmpDF$Year == 2014, 2:13] <- DF85[i,3:14]
        tmpDF[tmpDF$Year == 2015, 2:13] <- DF86[i,3:14]
        tmpDF[tmpDF$Year == 2016, 2:13] <- DF87[i,3:14]
        tmpDF[tmpDF$Year == 2017, 2:13] <- DF88[i,3:14]
        tmpDF[tmpDF$Year == 2018, 2:13] <- DF89[i,3:14]
        tmpDF[tmpDF$Year == 2019, 2:13] <- DF90[i,3:14]
        
        
        ### Cut the tables
        jan_cut = cut(tmpDF[, "Jan"], breaks, include.lowest=TRUE,right=TRUE)
        feb_cut = cut(tmpDF[, "Feb"], breaks, include.lowest=TRUE,right=TRUE)
        mar_cut = cut(tmpDF[, "Mar"], breaks, include.lowest=TRUE,right=TRUE)
        apr_cut = cut(tmpDF[, "Apr"], breaks, include.lowest=TRUE,right=TRUE)
        may_cut = cut(tmpDF[, "May"], breaks, include.lowest=TRUE,right=TRUE)
        jun_cut = cut(tmpDF[, "Jun"], breaks, include.lowest=TRUE,right=TRUE)
        jul_cut = cut(tmpDF[, "Jul"], breaks, include.lowest=TRUE,right=TRUE)
        aug_cut = cut(tmpDF[, "Aug"], breaks, include.lowest=TRUE,right=TRUE)
        sep_cut = cut(tmpDF[, "Sep"], breaks, include.lowest=TRUE,right=TRUE)
        oct_cut = cut(tmpDF[, "Oct"], breaks, include.lowest=TRUE,right=TRUE)
        nov_cut = cut(tmpDF[, "Nov"], breaks, include.lowest=TRUE,right=TRUE)
        dec_cut = cut(tmpDF[, "Dec"], breaks, include.lowest=TRUE,right=TRUE)
        
        ### Frequency tables
        bin[,"Jan"] = table(jan_cut)
        bin[,"Feb"] = table(feb_cut)
        bin[,"Mar"] = table(mar_cut)
        bin[,"Apr"] = table(apr_cut)
        bin[,"May"] = table(may_cut)
        bin[,"Jun"] = table(jun_cut)
        bin[,"Jul"] = table(jul_cut)
        bin[,"Aug"] = table(aug_cut)
        bin[,"Sep"] = table(sep_cut)
        bin[,"Oct"] = table(oct_cut)
        bin[,"Nov"] = table(nov_cut)
        bin[,"Dec"] = table(dec_cut)
        
        bin[,"whole"] = rowSums(bin[,2:13])
        
        #uncertainty with respect to state H(Y)
        V1 <- bin[,"whole"]/whole_sum
        V2 <- log10(bin[,"whole"]/whole_sum)
        for (k in 1:length(V2)) {
          if(is.finite(V2[k])==F) {
            V2[k] <- 0 
          } else {
            V2[k] <- V2[k]
          }
        }
        
        HofY <- -sum(V1*V2)
        
        #uncertainty with respect to interaction of time and state, H(XY)
        M1 <- bin[1:interval,2:13]/whole_sum
        M2 <- log10(M1)
        for (k in 1:length(M2)) {
          if(is.finite(M2[k])==F) {
            M2[k] <- 0
          } else {
            M2[k] <- M2[k]
          }
        }
        
        HofXY <- -sum(M1*M2)
        
        #Conditional uncertainty with regard to state, with time given, HXofY
        HXofY <- HofXY - HofX
        
        #predictability (P), constancy(C) and contingency (M)
        P <- 1-(HXofY/log10(s.val))
        C <- 1-(HofY/log10(s.val))
        M <- (HofX+HofY-HofXY)/log10(s.val)
        
        #mutual information, I(XY)
        IofXY <- HofY - HXofY
        
        #deviation from homogeneity of the columns of the matrix for constancy, GC
        GC <- 2*whole_sum*(log(s.val)-HofY)
        C_free <- s.val-1
        
        #deviation from homogeneity of the columns of the matrix for contingency, GM
        GM <- 2*whole_sum*(HofX+HofY-HofXY)
        M_free <- (s.val-1)*(t.val-1)
        
        #deviation from homogeneity of the columns of the matrix for predictability, GP
        GP <- GM + GC
        P_free <- (s.val-1)*t.val
        
        out[i,"P"] <- P
        out[i,"C"] <- C
        out[i,"M"] <- M
      }
    }
    
    ### merge by biome and then remove NAs
    out <- merge(out, bDF, by=c("Site_ID", "lon", "lat"))
    
    
    ### save output
    saveRDS(out, paste0(destDir, "/Australia_rainfall_predictability_biome_decile.rds"))
    
}