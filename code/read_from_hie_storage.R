Read_from_HIE_storage <- function(current.year) {
    
    ### Set up command lines
    ssh.command <- "ssh 30046137@hie-storage.intersect.org.au"
    hie.dir1 <- "/data/hiestorage/Common/AWAP/rain/"
    hie.dir2 <- paste0(hie.dir1, current.year)
    
    ### Set up data file names
    s <- as.Date(paste0(current.year, "-01-01"), "%Y-%m-%d")
    e <- as.Date(paste0(current.year, "-12-31"), "%Y-%m-%d")
    tseries <- seq(s, e, by = 1)
    tseries <- gsub("-", "", tseries)
    
    for (j in 1:length(tseries)) {
        ### Create file name
        file.name <- paste0(hie.dir2, "/", current.year, "/rain_",
                            tseries[j], ".grid.Z")
        
        ### Create access command
        access.command <- paste0(ssh.command, " cat ", file.name)
        
        ### Unzip the file
        actual.access.command <- "scp -r 30046137@hie-storage.intersect.org.au:/data/hiestorage/Common/AWAP/rain/1990/ /Users/mingkaijiang/Documents/Research/Projects/Australia_precipitation_predictability/Git/data/"
        
    }
    
    

    
    
}