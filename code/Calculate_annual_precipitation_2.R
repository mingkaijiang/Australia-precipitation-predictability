Calculate_annual_precipitation_2 <- function(sourceDir, destDir) {
    #### Input data from csv file

    ### create destDir if not exists
    if(!dir.exists(destDir)) {
        dir.create(destDir, showWarnings = FALSE)
    }
    
    ### Read in all files in the input directory
    filenames <- list.files(sourceDir, pattern="*.csv", full.names=TRUE)
    
    for (i in seq_along(filenames)) {
        assign(paste0("DF", i), read.csv(filenames[i]))
    }
    
    ### Prepare output dataframe
    outDF <- DF1[,1:3]
    colnames(outDF) <- c("lon", "lat", "mean")
    
    ### Calculate 30-year total rainfall average for each grid
    n <- nrow(DF1)
    for (i in 1:n) {
        outDF[i, "mean"] <- mean(DF1[i,"ann"],DF2[i,"ann"],DF3[i,"ann"],DF4[i,"ann"],DF5[i,"ann"],
                                 DF5[i,"ann"],DF6[i,"ann"],DF7[i,"ann"],DF8[i,"ann"],DF9[i,"ann"],DF10[i,"ann"],
                                 DF11[i,"ann"],DF12[i,"ann"],DF13[i,"ann"],DF14[i,"ann"],DF15[i,"ann"],
                                 DF15[i,"ann"],DF16[i,"ann"],DF17[i,"ann"],DF18[i,"ann"],DF19[i,"ann"],DF20[i,"ann"],
                                 DF21[i,"ann"],DF22[i,"ann"],DF23[i,"ann"],DF24[i,"ann"],DF25[i,"ann"],
                                 DF25[i,"ann"],DF26[i,"ann"],DF27[i,"ann"],DF28[i,"ann"],DF29[i,"ann"],DF30[i,"ann"], na.rm=T)
    }
    
    write.csv(outDF, paste0(destDir, "/Australia_rainfall_annual_0.5_resolution.csv"),
              row.names=F)
    
}