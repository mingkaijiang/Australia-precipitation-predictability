Make_basic_plots_0.5_degree_resolution <- function() {
    
    #### Read in 0.5 resolution gridded predictability data
    myDF <- read.csv("output/Australia_rainfall_predictability_0.5_resolution.csv")
    
    #### Prepare P data
    pdf("output/basic_plots_0.5_degree_resolution.pdf")
    
    ### 0.5 degree P
    with(myDF, quilt.plot(x, y, P, nx=82, ny=66,  nlevel=100,
                          xlim=c(110,160), ylim=c(5,45),
                          main="Predictability", add.legend=T))
    
    ### 0.5 degree C
    with(myDF, quilt.plot(x, y, C, nx=82, ny=66,  nlevel=100,
                          xlim=c(110,160), ylim=c(5,45),
                          main="Constancy", add.legend=T))
    
    ### 0.5 degree M
    with(myDF, quilt.plot(x, y, M, nx=82, ny=66,  nlevel=100,
                          xlim=c(110,160), ylim=c(5,45),
                          main="Contingency", add.legend=T))
    
    dev.off()
    
}