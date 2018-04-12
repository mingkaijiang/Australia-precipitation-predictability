######  This is a repo to create Australia precipitation predictability
######  based on Colwell index and AWAP data (Australia Water Availability Project)
######  Key refs:
######  
######  Data source: http://www.csiro.au/awap/
######  Data readme: http://www.csiro.au/awap/doc/AWAP_readme_v8.txt
######  Data storage: HIE storage
######  Author: Mingkai Jiang (m.jiang@westernsydney.edu.au)
###################################################################################

###### 1. Download the data 
#### source the set up stuffs
source("prepare.R")

#### Set up time frame
s.yr <- 1990
e.yr <- 1999

#### Read in data
for (i in c(s.yr : e.yr)) {
    Read_from_HIE_storage(current.year == i)
}


###### 2. Process the data into the right format



###### 3. Calculate predictability for each grid
Calculate_predictability(sourceDir, destDir)


###### 4. Make some plots



###### End.