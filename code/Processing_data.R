Processing_data <- function(sourceDir, destDir) {
    #### To process the raw data into format easily readable
    
    ### create destDir if not exists
    dir.create(destDir, showWarnings = FALSE)
    
    ### Source all files in input folder
    DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
    
    
    
}