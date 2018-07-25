######  This is a repo to create Australia precipitation predictability
######  based on Colwell index and AWAP data (Australia Water Availability Project)
######  Key refs:
######  
######  Data source: http://www.csiro.au/awap/
######  Data readme: http://www.csiro.au/awap/doc/AWAP_readme_v8.txt
######  Data storage: HIE storage
######  Author: Mingkai Jiang (m.jiang@westernsydney.edu.au)
###################################################################################

###### clear wk space
rm(list=ls(all=TRUE))


###### 1. Download the data 
#### source the set up stuffs
source("prepare.R")

#### Read in data
#Read_from_HIE_storage(s.yr = 1979, e.yr=1979) don't use this!!!

###### 2. Process the data into the right format
#### 2.1 Convert from single file for each day to single file for each year
####     the data is stored in external hard drive, because it's bloody big
Processing_data(sourceDir = "/Volumes/Seagate Backup Plus Drive/Australia_predictability_data", 
                destDir = "processed_data")

#### 2.2. Process the data into the right format - scale up to 10 km resolution
scale_up_first(sourceDir = "processed_data", 
               destDir = "scaled_data")

#### 2.3 Calculate 80-year average annual precipitation, 
####     return a single csv file in 10 km resolution (0.1 degree resolution)
Calculate_annual_precipitation_2(sourceDir = "scaled_data",
                                 destDir = "output")

###### 3. Calculate predictability
#### 3.1 This is the first way, based on monthly % of annual total
Calculate_predictability_percent_2(sourceDir = "scaled_data",
                                  destDir = "output")

#### 3.2 Second way, bin monthly data using power of 3
Calculate_predictability_exponential_binning(sourceDir = "scaled_data",
                                             destDir = "output_exp")

#### 3.3 Third way, bin monthly data using quantile of the entire data
Calculate_predictability_decile(sourceDir = "scaled_data",
                                destDir = "output_decile")

#### 3.4 Fourth way, bin monthly data using biome-specific quantile
Calculate_predictability_biome_decile(sourceDir = "scaled_data",
                                      destDir = "output_biome_decile")

###### 4. Make basic plot
Make_basic_plots_0.1_degree_resolution(infile="output/Australia_rainfall_predictability_10km_resolution_2.csv",
                                       outfile="basic_plots_10km_degree_resolution_percent")

Make_basic_plots_0.1_degree_resolution(infile="output_exp/Australia_rainfall_predictability_10km_resolution_exp.csv",
                                       outfile="basic_plots_10km_degree_resolution_exp")

Make_basic_plots_0.5_degree_resolution(infile="output_decile/Australia_rainfall_predictability_10km_resolution_quantile.csv",
                                       outfile="basic_plots_10km_degree_resolution_quantile")

Make_basic_plots_0.5_degree_resolution(infile="output_biome_decile/Australia_rainfall_predictability_10km_resolution_biome_quantile.csv",
                                       outfile="basic_plots_10km_degree_resolution_biome_quantile")

###### End.
