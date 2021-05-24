unzip_file <- function(sourceDir) {
  #### To process the raw data into format easily readable
  
  ### unzip
  for (i in 1966:1970) {
    ### complete the path
    sDir <- paste0(sourceDir, "/", i)
    
    DatFiles <- list.files(path = sDir, pattern = "\\.Z")
    
    for (j in 1:length(DatFiles)) {
      system(paste0("uncompress ", sDir, "/", DatFiles[j]))
      
    }
  }
  
}