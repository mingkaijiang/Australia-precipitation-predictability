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
Processing_data(sourceDir = "data", 
                destDir = "processed_data/")
    



###### 3. Calculate predictability for each grid
Calculate_predictability(sourceDir, destDir)


###### 4. Make some plots



###### End.