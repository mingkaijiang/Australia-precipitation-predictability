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
Read_from_HIE_storage(s.yr = 1979, e.yr=1979)

###### 2. Process the data into the right format
#### 2.1 Convert from single file for each day to single file for each year
Processing_data(sourceDir = "data", 
                destDir = "processed_data")


###### There 2 ways of proceeding from this point
###### The first way is to process data at 0.05 degree resolution first, 
###### meaning we need to calculate predictability at 0.05 degree, 
###### and then scale up to 0.5 degree resolution.
###### The second way is to do scale up first, and then calculate predictability.
###### I am doing two ways. Below is the first way of calculating predictability.

#### 2.2. Calculate 30-year average annual precipitation
Calculate_annual_precipitation(sourceDir = "processed_data",
                               destDir = "output")
    
###### 3. Calculate predictability
Calculate_predictability_percent(sourceDir = "processed_data",
                                 destDir = "output")


###### 4. Convert from 0.05 degree resolution to 0.5 degree resolution
Scaling_up_to_half_degree_resolution(inFile = "output/Australia_rainfall_predictability_0.05_resolution.csv",
                                     outFile = "output/Australia_rainfall_predictability_0.5_resolution.csv")


###### 5. Make some basic plots
Make_basic_plots_0.05_degree_resolution()


###### Below is the 2nd way of calculating predictability
###### i.e. scale up first, then make calculations of predictability. 

#### 2.2. Process the data into the right format
scale_up_first(sourceDir = "processed_data", 
               destDir = "scaled_data")

#### 2.3 Calculate 30-year average annual precipitation
Calculate_annual_precipitation_2(sourceDir = "scaled_data",
                                 destDir = "output")

###### 3. Calculate predictability
Calculate_predictability_percent_2(sourceDir = "scaled_data",
                                  destDir = "output")

#### Make basic plot
Make_basic_plots_0.5_degree_resolution()

###### Below is the 3rd way of calculating predictability
###### Use the scaled up data,
###### Bin monthly rainfall data using power of 3

###### 3. Calculate predictability
Calculate_predictability_exponential_binning(sourceDir = "scaled_data",
                                             destDir = "output_exp")



###### Below is the 4th way of calculating predictability
###### Use the scaled up data,
###### Bin monthly rainfall data using decile at each site

###### 3. Calculate predictability
Calculate_predictability_decile(sourceDir = "scaled_data",
                                destDir = "output_decile")

###### End.