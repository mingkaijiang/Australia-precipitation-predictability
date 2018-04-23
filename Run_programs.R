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
#Read_from_HIE_storage(s.yr = 1993, e.yr=1993)

###### 2. Process the data into the right format
#### 2.1 Convert from single file for each day to single file for each year
Processing_data(sourceDir = "data", 
                destDir = "processed_data")

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
Make_basic_plots_0.5_degree_resolution()
Make_basic_plots_0.05_degree_resolution()


###### End.