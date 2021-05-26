
download_AWAP_temperature_data <- function(destDir) {
    
    ### remote url
    url1 <- "http://www.bom.gov.au/web03/ncc/www/awap/temperature/minave/daily/grid/0.05/history/nat/"
    
    ### file names
    day.list <- seq.Date(as.Date("2019/01/01"), 
                         as.Date("2019/12/31"), 
                         by="day")
    
    day.list <- gsub("-", "", day.list)

    
    ### download command
    for (i in day.list) {
      
      withr::with_options(list(HTTPUserAgent="My settings"), 
                          download.file(url=paste0(url1, i, i, ".grid.Z"),
                                        destfile=paste0(destDir, "/", "temp_min_", i, ".grid.Z"),
                                        method="auto"))
      
    }
    
}

