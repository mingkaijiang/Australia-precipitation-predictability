
# Set up data directory
if(!dir.exists("data")) {
    dir.create("data")
}

# load libraries
if(!require(pacman))install.packages("pacman")
pacman::p_load(RCurl) # add other packages needed to this list


# Sourcing all code functions
sourcefiles <- dir("code", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z in sourcefiles)source(z)
