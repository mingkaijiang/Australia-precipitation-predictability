Calculate_predictability_exponential_binning<- function(sourceDir, destDir) {
    #### Input data in 3-d format of grid, grid, month 
    #### Output data in format of: site ID, year, jan, feb, ...., dec, ann, for each grid
    ####
    
    ### create destDir if not exists
    if(!dir.exists(destDir)) {
        dir.create(destDir, showWarnings = FALSE)
    }
    
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
    #interval <- 10
    
    ### Create binning scheme
    max_top <- 3^7
    min_bot <- 0
    interval <- 7
    diff <- max_top - min_bot
    bin <- matrix(0, ncol=14, nrow=interval)
    dimnames(bin) <- list(NULL,c("bin_size","Jan", "Feb", "Mar", "Apr", "May",
                                 "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "whole"))
    for (i in 1:interval) {
        bin[i,"bin_size"] <- 3^i
    }
    breaks = c(0, 3^1, 3^2, 3^3, 3^4, 3^5, 3^6, 3^7)
    
    s <- interval
    t <- 12
    
    ### output in each grid
    for (i in 1:nrow(out)) {

        ### Fill the temp DF, read for calculating predictability
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
        jan_freq = table(jan_cut)
        feb_freq = table(feb_cut)
        mar_freq = table(mar_cut)
        apr_freq = table(apr_cut)
        may_freq = table(may_cut)
        jun_freq = table(jun_cut)
        jul_freq = table(jul_cut)
        aug_freq = table(aug_cut)
        sep_freq = table(sep_cut)
        oct_freq = table(oct_cut)
        nov_freq = table(nov_cut)
        dec_freq = table(dec_cut)
        
        ### Assign onto bins
        bin[,"Jan"] <- jan_freq
        bin[,"Feb"] <- feb_freq
        bin[,"Mar"] <- mar_freq
        bin[,"Apr"] <- apr_freq
        bin[,"May"] <- may_freq
        bin[,"Jun"] <- jun_freq
        bin[,"Jul"] <- jul_freq
        bin[,"Aug"] <- aug_freq
        bin[,"Sep"] <- sep_freq
        bin[,"Oct"] <- oct_freq
        bin[,"Nov"] <- nov_freq
        bin[,"Dec"] <- dec_freq
        
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
        P <- 1-(HXofY/log10(s))
        C <- 1-(HofY/log10(s))
        M <- (HofX+HofY-HofXY)/log10(s)
        
        #mutual information, I(XY)
        IofXY <- HofY - HXofY
        
        #deviation from homogeneity of the columns of the matrix for constancy, GC
        GC <- 2*whole_sum*(log(s)-HofY)
        C_free <- s-1
        
        #deviation from homogeneity of the columns of the matrix for contingency, GM
        GM <- 2*whole_sum*(HofX+HofY-HofXY)
        M_free <- (s-1)*(t-1)
        
        #deviation from homogeneity of the columns of the matrix for predictability, GP
        GP <- GM + GC
        P_free <- (s-1)*t
        
        out[i,"P"] <- P
        out[i,"C"] <- C
        out[i,"M"] <- M

    }   
    
    ### Get biome grids
    bDF <- Read_biome_grids()
    colnames(bDF)[which(names(bDF) == "id")] <- "Site_ID"
    
    
    out <- merge(out, bDF, by=c("Site_ID", "lon", "lat"))
    
    ### save output
    saveRDS(out, paste0(destDir, "/Australia_rainfall_predictability_exp.rds"))
    
}