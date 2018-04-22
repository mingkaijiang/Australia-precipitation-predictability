Calculate_annual_precipitation <- function(sourceDir, destDir) {
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
    tmpDF <- matrix(ncol = 2, nrow = 30)
    tmpDF <- as.data.frame(tmpDF)
    colnames(tmpDF) <- c("Year", "Sum")
    tmpDF$Year <- c(1980:2009)
    ts <- tmpDF$Year
    
    ### Create a out df to store all data in one file
    out <- matrix(ncol=4, nrow=691 * 886)
    out <- as.data.frame(out, row.names = NULL, stringsAsFactors = FALSE)
    colnames(out) <- c("Site_ID", "annual_prec", "i", "j")
    out$Site_ID <- c(1:691*886)
    out$i <- rep(1:691, each=886) 
    out$j <- rep(1:886, by = 691)
    
    l <- 1

    ### output in each grid
    for (i in 1:691) {
        for (j in 1:886) {
            
            ### Fill the temp DF, read for calculating predictability
            tmpDF[tmpDF$Year == 1980, "Sum"] <- sum(DF1[i,j,],na.rm=T)
            tmpDF[tmpDF$Year == 1981, "Sum"] <- sum(DF2[i,j,],na.rm=T)
            tmpDF[tmpDF$Year == 1982, "Sum"] <- sum(DF3[i,j,],na.rm=T)
            tmpDF[tmpDF$Year == 1983, "Sum"] <- sum(DF4[i,j,],na.rm=T)
            tmpDF[tmpDF$Year == 1984, "Sum"] <- sum(DF5[i,j,],na.rm=T)
            tmpDF[tmpDF$Year == 1985, "Sum"] <- sum(DF6[i,j,],na.rm=T)
            tmpDF[tmpDF$Year == 1986, "Sum"] <- sum(DF7[i,j,],na.rm=T)
            tmpDF[tmpDF$Year == 1987, "Sum"] <- sum(DF8[i,j,],na.rm=T)
            tmpDF[tmpDF$Year == 1988, "Sum"] <- sum(DF9[i,j,],na.rm=T)
            tmpDF[tmpDF$Year == 1989, "Sum"] <- sum(DF10[i,j,],na.rm=T)
            
            tmpDF[tmpDF$Year == 1990, "Sum"] <- sum(DF11[i,j,],na.rm=T)
            tmpDF[tmpDF$Year == 1991, "Sum"] <- sum(DF12[i,j,],na.rm=T)
            tmpDF[tmpDF$Year == 1992, "Sum"] <- sum(DF13[i,j,],na.rm=T)
            tmpDF[tmpDF$Year == 1993, "Sum"] <- sum(DF14[i,j,],na.rm=T)
            tmpDF[tmpDF$Year == 1994, "Sum"] <- sum(DF15[i,j,],na.rm=T)
            tmpDF[tmpDF$Year == 1995, "Sum"] <- sum(DF16[i,j,],na.rm=T)
            tmpDF[tmpDF$Year == 1996, "Sum"] <- sum(DF17[i,j,],na.rm=T)
            tmpDF[tmpDF$Year == 1997, "Sum"] <- sum(DF18[i,j,],na.rm=T)
            tmpDF[tmpDF$Year == 1998, "Sum"] <- sum(DF19[i,j,],na.rm=T)
            tmpDF[tmpDF$Year == 1999, "Sum"] <- sum(DF20[i,j,],na.rm=T)
            
            tmpDF[tmpDF$Year == 2000, "Sum"] <- sum(DF21[i,j,],na.rm=T)
            tmpDF[tmpDF$Year == 2001, "Sum"] <- sum(DF22[i,j,],na.rm=T)
            tmpDF[tmpDF$Year == 2002, "Sum"] <- sum(DF23[i,j,],na.rm=T)
            tmpDF[tmpDF$Year == 2003, "Sum"] <- sum(DF24[i,j,],na.rm=T)
            tmpDF[tmpDF$Year == 2004, "Sum"] <- sum(DF25[i,j,],na.rm=T)
            tmpDF[tmpDF$Year == 2005, "Sum"] <- sum(DF26[i,j,],na.rm=T)
            tmpDF[tmpDF$Year == 2006, "Sum"] <- sum(DF27[i,j,],na.rm=T)
            tmpDF[tmpDF$Year == 2007, "Sum"] <- sum(DF28[i,j,],na.rm=T)
            tmpDF[tmpDF$Year == 2008, "Sum"] <- sum(DF29[i,j,],na.rm=T)
            tmpDF[tmpDF$Year == 2009, "Sum"] <- sum(DF30[i,j,],na.rm=T)

            out[l,"annual_prec"] <- mean(tmpDF$Sum)
            
            l <- l + 1
        }   # j
    }       # i
    
    write.csv(out, paste0(destDir, "/Australia_rainfall_annual_0.05_resolution.csv"),
              row.names=F)
    
}