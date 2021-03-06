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
Calculate_annual_precipitation(sourceDir = paste0(getwd(), "/scaled_rainfall_data"),
                               destDir = paste0(getwd(), "/output"))

###### 3. Calculate predictability
#### 3.2 Second way, bin monthly data using power of 3
Calculate_predictability_exponential_binning(sourceDir = paste0(getwd(), "/scaled_rainfall_data"),
                                             destDir = paste0(getwd(), "/output_rainfall_exp"))

#### 3.4 Fourth way, bin monthly data using biome-specific quantile
Calculate_biome_specific_deciles_koppen(sourceDir=paste0(getwd(), "/scaled_rainfall_data"), 
                                        destDir=paste0(getwd(), "/output_rainfall_biome_decile"),
                                        biome.decision="koppen_subgroup")

Calculate_biome_specific_deciles_koppen(sourceDir=paste0(getwd(), "/scaled_rainfall_data"), 
                                        destDir=paste0(getwd(), "/output_rainfall_biome_decile"),
                                        biome.decision="koppen_broadgroup")

Calculate_biome_specific_deciles_koppen(sourceDir=paste0(getwd(), "/scaled_rainfall_data"), 
                                        destDir=paste0(getwd(), "/output_rainfall_biome_decile"),
                                        biome.decision="wwf_group")

Calculate_predictability_biome_decile(sourceDir = paste0(getwd(), "/scaled_rainfall_data"),
                                      destDir = paste0(getwd(), "/output_rainfall_biome_decile"),
                                      biome.decision = "koppen_subgroup")

Calculate_predictability_biome_decile(sourceDir = paste0(getwd(), "/scaled_rainfall_data"),
                                      destDir = paste0(getwd(), "/output_rainfall_biome_decile"),
                                      biome.decision = "koppen_broadgroup")

Calculate_predictability_biome_decile(sourceDir = paste0(getwd(), "/scaled_rainfall_data"),
                                      destDir = paste0(getwd(), "/output_rainfall_biome_decile"),
                                      biome.decision = "wwf_group")

###### 4. Make basic plot
Make_basic_plots_0.1_degree_resolution(infile=paste0(getwd(), "/output_rainfall_exp/Australia_rainfall_predictability_exp.rds"),
                                       outfile=paste0(getwd(), "/output_rainfall_exp/basic_rainfall_plots_exp"))

Make_basic_plots_0.1_degree_resolution(infile=paste0(getwd(), "/output_rainfall_biome_decile/Australia_rainfall_predictability_biome_koppen_subgroup_decile.rds"),
                                       outfile=paste0(getwd(), "/output_rainfall_biome_decile/basic_rainfall_plots_biome_koppen_subgroup_decile"))

Make_basic_plots_0.1_degree_resolution(infile=paste0(getwd(), "/output_rainfall_biome_decile/Australia_rainfall_predictability_biome_koppen_broadgroup_decile.rds"),
                                       outfile=paste0(getwd(), "/output_rainfall_biome_decile/basic_rainfall_plots_biome_koppen_broadgroup_decile"))

Make_basic_plots_0.1_degree_resolution(infile=paste0(getwd(), "/output_rainfall_biome_decile/Australia_rainfall_predictability_biome_wwf_group_decile.rds"),
                                       outfile=paste0(getwd(), "/output_rainfall_biome_decile/basic_rainfall_plots_biome_wwf_group_decile"))



#### 6. select locations based on site coordinates
select_sites_exp(sourceDir = paste0(getwd(), "/output_rainfall_exp"), 
                 destDir = paste0(getwd(), "/output_rainfall_exp"),
                 resp.variable = "rainfall")


select_sites_biome(sourceDir = paste0(getwd(), "/output_rainfall_biome_decile"), 
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
Calculate_temperature_predictability_fixed_bin(sourceDir = paste0(getwd(), "/processed_data_tmean"),
                                               destDir = paste0(getwd(), "/output_temperature_fixed_bin"))


Calculate_temperature_predictability_decile(sourceDir = paste0(getwd(), "/processed_data_tmean"),
                                            destDir = paste0(getwd(), "/output_temperature_decile"))


Calculate_temperature_predictability_biome_decile(sourceDir = paste0(getwd(), "/processed_data_tmean"),
                                                  destDir = paste0(getwd(), "/output_biome_temperature_decile"))

###### 4. Make basic plot
Make_basic_plots_0.1_degree_resolution(infile=paste0(getwd(), "/output_biome_temperature_decile/Australia_temperature_predictability_biome_decile.rds"),
                                       outfile=paste0(getwd(), "/output_biome_temperature_decile/basic_temperature_plots_biome_decile"))

Make_basic_plots_0.1_degree_resolution(infile=paste0(getwd(), "/output_temperature_decile/Australia_temperature_predictability_decile.rds"),
                                       outfile=paste0(getwd(), "/output_temperature_decile/basic_temperature_plots_decile"))

Make_basic_plots_0.1_degree_resolution(infile=paste0(getwd(), "/output_temperature_fixed_bin/Australia_temperature_predictability_fixed_bin.rds"),
                                       outfile=paste0(getwd(), "/output_temperature_fixed_bin/basic_temperature_plots_fixed_bin"))

#### 6. select locations based on site coordinates
select_sites(sourceDir = paste0(getwd(), "/output_biome_temperature_decile"), 
             destDir = paste0(getwd(), "/output_biome_temperature_decile"),
             resp.variable = "temperature")


select_sites_no_biome(sourceDir = paste0(getwd(), "/output_temperature_decile"), 
                       destDir = paste0(getwd(), "/output_temperature_decile"),
                       resp.variable = "temperature") 


select_sites_fixed_bin(sourceDir = paste0(getwd(), "/output_temperature_fixed_bin"), 
                      destDir = paste0(getwd(), "/output_temperature_fixed_bin"),
                      resp.variable = "temperature") 

###### End.
