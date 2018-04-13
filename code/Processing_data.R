Processing_data <- function(sourceDir, destDir) {
    #### To process the raw data into format easily readable
    
    for (i in 1990:2000) {
        ### complete the path
        sDir <- paste0(sourceDir, "/", i)
        dDir <- paste0(destDir, "/", i)
        
        ### create destDir if not exists
        if(!dir.exists(dDir)) {
            dir.create(dDir, showWarnings = FALSE)
        }
        
        ### Source all files in input folder
        DatFiles <- list.files(path = sDir, pattern = "\\.grid")
        
        ### Prepare output array
        if (leap_year(i)) {
            daily.tmp <- array(NA, c(691, 886, 366))
        } else {
            daily.tmp <- array(NA, c(691, 886, 365))
        }
        
        ### Prepare monthly output array
        out <- array(NA, c(691, 886, 12))
        
        ### Read in data
        for (j in 1:length(DatFiles)) {
            inName <- file.path(sDir, DatFiles[j], fsep = .Platform$file.sep)
            myDF <- read.ascii.grid(inName)
            
            for (k in 1:dim(out)[1]) {
                for (l in 1:dim(out)[2]) {
                    daily.tmp[k, l, j] <- myDF$data[k, l]
                }   # l loop
            }    # k loop
        }   # j loop
        
        
        ### calculate monthly sum precipitation for each grid
        
        
    }  # i loop
}   # function loop