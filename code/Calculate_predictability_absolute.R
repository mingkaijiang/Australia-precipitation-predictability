Calculate_predictability_absolute <- function(sourceDir, destDir) {
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
    tmpDF <- matrix(ncol = 13, nrow = 30)
    tmpDF <- as.data.frame(tmpDF)
    colnames(tmpDF) <- c("Year", "Jan", "Feb", "Mar", "Apr", "May",
                       "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    tmpDF$Year <- c(1980:2009)
    ts <- tmpDF$Year
    
    ### Create a out df to store all data in one file
    out <- matrix(ncol=11, nrow=691 * 886)
    out <- as.data.frame(out, row.names = NULL, stringsAsFactors = FALSE)
    colnames(out) <- c("Site_ID", "P","C","M",
                       "Mutual","GC","C_freedom","GM","M_freedom","GP","P_freedom")
    out$Site_ID <- c(1:691*886)
    #out$i <- rep(1:691, each=886) 
    #out$j <- rep(1:886, by = 691)
    
    ### Get some general information
    years <- min(ts)
    yeare <- max(ts)
    yearr <- yeare-years
    max_top <- 1
    min_bot <- 0
    interval <- 10
    diff <- max_top - min_bot
    bin <- matrix(0, ncol=14, nrow=interval)
    dimnames(bin) <- list(NULL,c("bin_size","Jan", "Feb", "Mar", "Apr", "May",
                                 "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "whole"))
    bin[,"bin_size"] <- seq(0.1,1,0.1)
    breaks = seq(0,1,0.1)
    
    l <- 1

    ### output in each grid
    for (i in 1:691) {
        for (j in 1:886) {
            
            ### Fill the temp DF, read for calculating predictability
            tmpDF[tmpDF$Year == 1980, 2:13] <- DF1[i,j,]
            tmpDF[tmpDF$Year == 1981, 2:13] <- DF2[i,j,]
            tmpDF[tmpDF$Year == 1982, 2:13] <- DF3[i,j,]
            tmpDF[tmpDF$Year == 1983, 2:13] <- DF4[i,j,]
            tmpDF[tmpDF$Year == 1984, 2:13] <- DF5[i,j,]
            tmpDF[tmpDF$Year == 1985, 2:13] <- DF6[i,j,]
            tmpDF[tmpDF$Year == 1986, 2:13] <- DF7[i,j,]
            tmpDF[tmpDF$Year == 1987, 2:13] <- DF8[i,j,]
            tmpDF[tmpDF$Year == 1988, 2:13] <- DF9[i,j,]
            tmpDF[tmpDF$Year == 1989, 2:13] <- DF10[i,j,]
            
            tmpDF[tmpDF$Year == 1990, 2:13] <- DF11[i,j,]
            tmpDF[tmpDF$Year == 1991, 2:13] <- DF12[i,j,]
            tmpDF[tmpDF$Year == 1992, 2:13] <- DF13[i,j,]
            tmpDF[tmpDF$Year == 1993, 2:13] <- DF14[i,j,]
            tmpDF[tmpDF$Year == 1994, 2:13] <- DF15[i,j,]
            tmpDF[tmpDF$Year == 1995, 2:13] <- DF16[i,j,]
            tmpDF[tmpDF$Year == 1996, 2:13] <- DF17[i,j,]
            tmpDF[tmpDF$Year == 1997, 2:13] <- DF18[i,j,]
            tmpDF[tmpDF$Year == 1998, 2:13] <- DF19[i,j,]
            tmpDF[tmpDF$Year == 1999, 2:13] <- DF20[i,j,]
            
            tmpDF[tmpDF$Year == 2000, 2:13] <- DF21[i,j,]
            tmpDF[tmpDF$Year == 2001, 2:13] <- DF22[i,j,]
            tmpDF[tmpDF$Year == 2002, 2:13] <- DF23[i,j,]
            tmpDF[tmpDF$Year == 2003, 2:13] <- DF24[i,j,]
            tmpDF[tmpDF$Year == 2004, 2:13] <- DF25[i,j,]
            tmpDF[tmpDF$Year == 2005, 2:13] <- DF26[i,j,]
            tmpDF[tmpDF$Year == 2006, 2:13] <- DF27[i,j,]
            tmpDF[tmpDF$Year == 2007, 2:13] <- DF28[i,j,]
            tmpDF[tmpDF$Year == 2008, 2:13] <- DF29[i,j,]
            tmpDF[tmpDF$Year == 2009, 2:13] <- DF30[i,j,]

            ### calculate as percentage of annual total
            tmpDF2 <- tmpDF
            tmpDF[,14] <- rowSums(tmpDF[,2:13])
            
            for (k in 2:13) {
                tmpDF2[,k] <- tmpDF[,k] / tmpDF[,14]
            } 
            
            ### Cut the tables
            jan_cut = cut(tmpDF2[, "Jan"], breaks, include.lowest=TRUE,right=TRUE)
            feb_cut = cut(tmpDF2[, "Feb"], breaks, include.lowest=TRUE,right=TRUE)
            mar_cut = cut(tmpDF2[, "Mar"], breaks, include.lowest=TRUE,right=TRUE)
            apr_cut = cut(tmpDF2[, "Apr"], breaks, include.lowest=TRUE,right=TRUE)
            may_cut = cut(tmpDF2[, "May"], breaks, include.lowest=TRUE,right=TRUE)
            jun_cut = cut(tmpDF2[, "Jun"], breaks, include.lowest=TRUE,right=TRUE)
            jul_cut = cut(tmpDF2[, "Jul"], breaks, include.lowest=TRUE,right=TRUE)
            aug_cut = cut(tmpDF2[, "Aug"], breaks, include.lowest=TRUE,right=TRUE)
            sep_cut = cut(tmpDF2[, "Sep"], breaks, include.lowest=TRUE,right=TRUE)
            oct_cut = cut(tmpDF2[, "Oct"], breaks, include.lowest=TRUE,right=TRUE)
            nov_cut = cut(tmpDF2[, "Nov"], breaks, include.lowest=TRUE,right=TRUE)
            dec_cut = cut(tmpDF2[, "Dec"], breaks, include.lowest=TRUE,right=TRUE)
            
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
            
            bin[,"whole"] = (bin[,"Jan"]+bin[,"Feb"]+bin[,"Mar"]+bin[,"Apr"]+
                                 bin[,"May"]+bin[,"Jun"]+bin[,"Jul"]+bin[,"Aug"]+
                                 bin[,"Sep"]+bin[,"Oct"]+bin[,"Nov"]+bin[,"Dec"])
            
            col_sum <- sum(table(tmpDF2[, "Jan"]))
            whole_sum <- col_sum*12
            
            #uncertainty with respect to time H(X)
            HofX <- -((col_sum/whole_sum)*log10(col_sum/whole_sum))*12
            
            #uncertainty with respect to state H(Y)
            V1 <- bin[,"whole"]/whole_sum
            V2 <- log10(bin[,"whole"]/whole_sum)
            for (i in 1:length(V2)) {
                if(is.finite(V2[i])==F) {
                    V2[i] <- 0 
                } else {
                    V2[i] <- V2[i]
                }
            }
            
            HofY <- -sum(V1*V2)
            
            #uncertainty with respect to interaction of time and state, H(XY)
            M1 <- bin[1:interval,2:13]/whole_sum
            M2 <- log10(M1)
            for (i in 1:length(M2)) {
                if(is.finite(M2[i])==F) {
                    M2[i] <- 0
                } else {
                    M2[i] <- M2[i]
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
            CoverP <- C/P
            MoverP <- M/P
            
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
            
            out[l,"P"] <- P
            out[l,"C"] <- C
            out[l,"M"] <- M
            out[l,"Mutual"] <- IofXY
            out[l,"GC"] <- GC
            out[l,"C_freedom"] <- C_free
            out[l,"GM"] <- GM
            out[l,"M_freedom"] <- M_free
            out[l,"GP"] <- GP
            out[l,"P_freedom"] <- P_free
            
            l <- l + 1
        }   # j
    }        # i
    
    write.csv(out, paste0(destDir, "/Australia_rainfall_predictability_0.05_resolution.csv"),
              row.names=F)
    
}