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
#unzip_file(sourceDir = paste0(getwd(), "/data/AWAP/rain"))

#### 2.1 Convert from single file for each day to single file for each year
Processing_data(sourceDir = paste0("/Volumes/TOSHIBAEXT/AWAP/rain"), 
                destDir = paste0(getwd(), "/processed_data"))

#### 2.2. Process the data into the right format 
#### formatting the data - convert into long format
scale_up_first(sourceDir = paste0(getwd(), "/processed_data"), 
               destDir = paste0(getwd(), "/scaled_rainfall_data"))


#### 2.3 Calculate 90-year average annual precipitation, 
####     return a single csv file in 5 km resolution (0.05 degree resolution)
Calculate_annual_precipitation_2(sourceDir = paste0(getwd(), "/scaled_rainfall_data"),
                                 destDir = paste0(getwd(), "/output"))

###### 3. Calculate predictability
#### 3.1 This is the first way, based on monthly % of annual total
#Calculate_predictability_percent_2(sourceDir = paste0(getwd(), "/scaled_data"),
#                                  destDir = paste0(getwd(), "/output"))

#### 3.2 Second way, bin monthly data using power of 3
#Calculate_predictability_exponential_binning(sourceDir = paste0(getwd(), "scaled_data"),
#                                             destDir = paste0(getwd(), "output_exp"))

#### 3.3 Third way, bin monthly data using quantile of the entire data
#Calculate_predictability_decile(sourceDir = paste0(getwd(), "/scaled_rainfall_data"),
#                                destDir = paste0(getwd(), "/output_rainfall_decile"))

#### 3.4 Fourth way, bin monthly data using biome-specific quantile
Calculate_predictability_biome_decile(sourceDir = paste0(getwd(), "/scaled_rainfall_data"),
                                      destDir = paste0(getwd(), "/output_rainfall_biome_decile"))

###### 4. Make basic plot
Make_basic_plots_0.1_degree_resolution(infile=paste0(getwd(), "/output_rainfall_biome_decile/Australia_rainfall_predictability_biome_decile.rds"),
                                       outfile=paste0(getwd(), "/output_rainfall_biome_decile/basic_rainfall_plots_biome_decile"))


#### 6. select locations based on site coordinates
select_sites(sourceDir = paste0(getwd(), "/output_rainfall_biome_decile"), 
             destDir = paste0(getwd(), "/output_rainfall_biome_decile"),
             resp.variable = "rainfall")



###################################################################################
####### Temperature - Tmax and Tmins

####### 1. download data
#download_AWAP_temperature_data(destDir=paste0("/Volumes/TOSHIBAEXT/AWAP/tmin"))
#unzip_file(sourceDir = paste0("/Volumes/TOSHIBAEXT/AWAP/tmin"))

###### 2. Process the data into the right format

#### 2.1 Convert from single file for each day to single file for each year
Processing_data_temperature(sourceDir = paste0("/Volumes/TOSHIBAEXT/AWAP/tmax"), 
                            destDir = paste0(getwd(), "/processed_data_tmax"))


Processing_data_temperature(sourceDir = paste0("/Volumes/TOSHIBAEXT/AWAP/tmin"), 
                            destDir = paste0(getwd(), "/processed_data_tmin"))


### calculate mean based on min and max, then convert into long format
calculate_monthly_mean_temperature(sourceDir1 = paste0(getwd(), "/processed_data_tmax"),
                                   sourceDir2 = paste0(getwd(), "/processed_data_tmin"),
                                   destDir = paste0(getwd(), "/processed_data_tmean"))


#### 2.3 Calculate 80-year average annual temperature
####     return a single csv file in 5 km resolution (0.05 degree resolution)
Calculate_annual_temperature(sourceDir = paste0(getwd(), "/processed_data_tmean"),
                             destDir = paste0(getwd(), "/output"))


#### 3.4 Fourth way, bin monthly data using biome-specific quantile
Calculate_temperature_predictability_decile(sourceDir = paste0(getwd(), "/processed_data_tmean"),
                                            destDir = paste0(getwd(), "/output_temperature_decile"))


Calculate_temperature_predictability_biome_decile(sourceDir = paste0(getwd(), "/processed_data_tmean"),
                                                  destDir = paste0(getwd(), "/output_biome_temperature_decile"))

###### 4. Make basic plot
Make_basic_plots_0.1_degree_resolution(infile=paste0(getwd(), "/output_biome_temperature_decile/Australia_temperature_predictability_biome_decile.rds"),
                                       outfile=paste0(getwd(), "/output_biome_temperature_decile/basic_temperature_plots_decile"))


#### 6. select locations based on site coordinates
select_sites(sourceDir = paste0(getwd(), "/output_biome_temperature_decile"), 
             destDir = paste0(getwd(), "/output_biome_temperature_decile"),
             resp.variable = "temperature")


###### End.
