Read_from_HIE_storage <- function(s.yr, e.yr) {
    
    #### Don't run this unless you are crazing
    #### Takes very long too run
    
    tseries <- seq(s.yr, e.yr, by=1)
    
    shell.command <- "scp -r 30046137@hie-storage.intersect.org.au:"
    sourceDir <- "/data/hiestorage/Common/AWAP/rain/"
    destDir <- "/Users/mingkaijiang/Documents/Research/Projects/Australia_precipitation_predictability/Git/data/"
    
    for (j in tseries) {
        ### download the data
        download.command <- paste0(shell.command, sourceDir, j, "/ ", destDir)
        system(download.command)
    }
    
}