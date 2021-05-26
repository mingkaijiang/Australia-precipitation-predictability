Make_basic_plots_0.1_degree_resolution <- function(infile, outfile) {
    #### Read in 0.1 resolution gridded predictability data
    myDF <- readRDS(infile)
    
    #### Prepare P data
    pdf(paste0(outfile, ".pdf"))

    ### 0.05 degree P
    with(myDF, quilt.plot(lon, lat, P, nx=400, ny=300,  nlevel=10,
                          xlim=c(110,160), ylim=c(-45,-5),
                          main="Predictability", add.legend=T))
    world(add=T)
    
    ### 0.05 degree C
    with(myDF, quilt.plot(lon, lat, C, nx=400, ny=300,  nlevel=100,
                          xlim=c(110,160), ylim=c(-45,-5),
                          main="Constancy", add.legend=T))
    world(add=T)
    
    ### 0.05 degree M
    with(myDF, quilt.plot(lon, lat, M, nx=400, ny=300,  nlevel=100,
                          xlim=c(110,160), ylim=c(-45,-5),
                          main="Contingency", add.legend=T))
    world(add=T)
    

    dev.off()

    
    test <- subset(myDF, P < 0.2)
    
    ### 0.05 degree P
    with(test, quilt.plot(lon, lat, P, nx=400, ny=300,  nlevel=100,
                          xlim=c(110,160), ylim=c(-45,-5),
                          main="Predictability", add.legend=T))
    world(add=T)
    
    
    with(myDF, quilt.plot(lon, lat, Biome, nx=400, ny=300,  nlevel=30,
                          xlim=c(110,160), ylim=c(-45,-5),
                          main="Predictability", add.legend=T))
    world(add=T)
    
}