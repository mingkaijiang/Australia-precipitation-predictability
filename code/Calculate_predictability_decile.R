Calculate_predictability_decile<- function(sourceDir, destDir) {
    #### Input data in 3-d format of grid, grid, month 
    #### Output data in format of: site ID, year, jan, feb, ...., dec, ann, for each grid
    ####
    
    ### create destDir if not exists
    if(!dir.exists(destDir)) {
        dir.create(destDir, showWarnings = FALSE)
    }
    
    ### Read in all files in the input directory
    filenames <- list.files(sourceDir, pattern="*.csv", full.names=TRUE)
    
    for (i in seq_along(filenames)) {
        assign(paste0("DF", i), read.csv(filenames[i]))
    }
    
    ### Create temporary DF
    tmpDF <- matrix(ncol = 13, nrow = 30)
    tmpDF <- as.data.frame(tmpDF)
    colnames(tmpDF) <- c("Year", "Jan", "Feb", "Mar", "Apr", "May",
                       "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    tmpDF$Year <- c(1980:2009)
    ts <- tmpDF$Year
    
    ### Create a out df to store all data in one file
    out <- matrix(ncol=6, nrow=70*89)
    out <- as.data.frame(out, row.names = NULL, stringsAsFactors = FALSE)
    colnames(out) <- c("Site_ID","lon","lat", "P","C","M")
    out$Site_ID <- c(1:(70*89))
    out$lon <- DF1$lon
    out$lat <- DF1$lat
    
    ### Get some general information
    years <- min(ts)
    yeare <- max(ts)
    yearr <- yeare-years
    
    ### Create matrix to store the frequency table, without defining bin sizes
    interval <- 10
    bin <- matrix(0, ncol=14, nrow=interval)
    dimnames(bin) <- list(NULL,c("bin_size","Jan", "Feb", "Mar", "Apr", "May",
                                 "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "whole"))
    
    ### output in each grid
    for (i in 1:nrow(out)) {

            ### Fill the temp DF, read for calculating predictability
            tmpDF[tmpDF$Year == 1980, 2:13] <- DF1[i,3:14]
            tmpDF[tmpDF$Year == 1981, 2:13] <- DF2[i,3:14]
            tmpDF[tmpDF$Year == 1982, 2:13] <- DF3[i,3:14]
            tmpDF[tmpDF$Year == 1983, 2:13] <- DF4[i,3:14]
            tmpDF[tmpDF$Year == 1984, 2:13] <- DF5[i,3:14]
            tmpDF[tmpDF$Year == 1985, 2:13] <- DF6[i,3:14]
            tmpDF[tmpDF$Year == 1986, 2:13] <- DF7[i,3:14]
            tmpDF[tmpDF$Year == 1987, 2:13] <- DF8[i,3:14]
            tmpDF[tmpDF$Year == 1988, 2:13] <- DF9[i,3:14]
            tmpDF[tmpDF$Year == 1989, 2:13] <- DF10[i,3:14]
            
            tmpDF[tmpDF$Year == 1990, 2:13] <- DF11[i,3:14]
            tmpDF[tmpDF$Year == 1991, 2:13] <- DF12[i,3:14]
            tmpDF[tmpDF$Year == 1992, 2:13] <- DF13[i,3:14]
            tmpDF[tmpDF$Year == 1993, 2:13] <- DF14[i,3:14]
            tmpDF[tmpDF$Year == 1994, 2:13] <- DF15[i,3:14]
            tmpDF[tmpDF$Year == 1995, 2:13] <- DF16[i,3:14]
            tmpDF[tmpDF$Year == 1996, 2:13] <- DF17[i,3:14]
            tmpDF[tmpDF$Year == 1997, 2:13] <- DF18[i,3:14]
            tmpDF[tmpDF$Year == 1998, 2:13] <- DF19[i,3:14]
            tmpDF[tmpDF$Year == 1999, 2:13] <- DF20[i,3:14]
            
            tmpDF[tmpDF$Year == 2000, 2:13] <- DF21[i,3:14]
            tmpDF[tmpDF$Year == 2001, 2:13] <- DF22[i,3:14]
            tmpDF[tmpDF$Year == 2002, 2:13] <- DF23[i,3:14]
            tmpDF[tmpDF$Year == 2003, 2:13] <- DF24[i,3:14]
            tmpDF[tmpDF$Year == 2004, 2:13] <- DF25[i,3:14]
            tmpDF[tmpDF$Year == 2005, 2:13] <- DF26[i,3:14]
            tmpDF[tmpDF$Year == 2006, 2:13] <- DF27[i,3:14]
            tmpDF[tmpDF$Year == 2007, 2:13] <- DF28[i,3:14]
            tmpDF[tmpDF$Year == 2008, 2:13] <- DF29[i,3:14]
            tmpDF[tmpDF$Year == 2009, 2:13] <- DF30[i,3:14]

            ### Now can define the deciles
            decile.list <- quantile(data.matrix(tmpDF[,2:13]), 
                                    prob=seq(0,1, length=11))
            
            max_top <- decile.list[11]
            min_bot <- decile.list[0]
            diff <- max_top - min_bot
            
            bin[,"bin_size"] <- decile.list[2:11]
            breaks = decile.list
            
            ### check: if repeated decile, ignore this site
            if (decile.list[1]==decile.list[2]) {
                out[i,"P"] <- NA
                out[i,"C"] <- NA
                out[i,"M"] <- NA
            } else {
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
                
                col_sum <- length(ts)   # total number count of years, i.e. 30
                whole_sum <- col_sum*12
                
                #uncertainty with respect to time H(X)
                HofX <- -((col_sum/whole_sum)*log10(col_sum/whole_sum))*12
                
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
                s <- interval
                t <- 12
                
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
    }   
    
    write.csv(out, paste0(destDir, "/Australia_rainfall_predictability_0.5_resolution_2.csv"),
              row.names=F)
    
}