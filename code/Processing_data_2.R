Processing_data_2 <- function(sourceDir, destDir) {
    #### Input data in 3-d format of grid, grid, month 
    #### Output data in format of: site ID, year, jan, feb, ...., dec, ann, for each grid
    ####
    
    ### create destDir if not exists
    if(!dir.exists(destDir)) {
        dir.create(destDir, showWarnings = FALSE)
    }
    
    ### Read in all files in the input directory
    filenames <- list.files("processed_data", pattern="*.rds", full.names=TRUE)
    
    for (i in seq_along(filenames)) {
        assign(paste0("DF", i), readRDS(filenames[i]))
    }
    
    ### Create a big output file
    # out <- matrix(ncol = 14, nrow = 691 * 886 * 20)
    # out <- as.data.frame(out)
    # colnames(out) <- c("Site_ID", "Year", "Jan", "Feb", "Mar", "Apr", "May",
    #                    "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    # out$Year <- rep(c(1990:2009), by = 691*886)
    
    ### Create temporary DF
    tmpDF <- matrix(ncol = 13, nrow = 20)
    tmpDF <- as.data.frame(tmpDF)
    colnames(tmpDF) <- c("Year", "Jan", "Feb", "Mar", "Apr", "May",
                       "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    tmpDF$Year <- c(1990:2009)
    ts <- tmpDF$Year
    
    ### output in each grid
    for (i in 1:691) {
        for (j in 1:886) {
            tmpDF[tmpDF$Year == 1990, 2:13] <- DF1[i,j,]
            tmpDF[tmpDF$Year == 1991, 2:13] <- DF2[i,j,]
            tmpDF[tmpDF$Year == 1992, 2:13] <- DF3[i,j,]
            tmpDF[tmpDF$Year == 1993, 2:13] <- DF4[i,j,]
            tmpDF[tmpDF$Year == 1994, 2:13] <- DF5[i,j,]
            tmpDF[tmpDF$Year == 1995, 2:13] <- DF6[i,j,]
            tmpDF[tmpDF$Year == 1996, 2:13] <- DF7[i,j,]
            tmpDF[tmpDF$Year == 1997, 2:13] <- DF8[i,j,]
            tmpDF[tmpDF$Year == 1998, 2:13] <- DF9[i,j,]
            tmpDF[tmpDF$Year == 1999, 2:13] <- DF10[i,j,]
            tmpDF[tmpDF$Year == 2001, 2:13] <- DF11[i,j,]
            tmpDF[tmpDF$Year == 2002, 2:13] <- DF12[i,j,]
            tmpDF[tmpDF$Year == 2003, 2:13] <- DF13[i,j,]
            tmpDF[tmpDF$Year == 2004, 2:13] <- DF14[i,j,]
            tmpDF[tmpDF$Year == 2005, 2:13] <- DF15[i,j,]
            tmpDF[tmpDF$Year == 2006, 2:13] <- DF16[i,j,]
            tmpDF[tmpDF$Year == 2007, 2:13] <- DF17[i,j,]
            tmpDF[tmpDF$Year == 2008, 2:13] <- DF18[i,j,]
            tmpDF[tmpDF$Year == 2009, 2:13] <- DF19[i,j,]
            tmpDF[tmpDF$Year == 2010, 2:13] <- DF20[i,j,]

            
        }
    }
    
}