# PROJECT: Micro-brewery analysis
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# Purpose
# Construct micro-brewery database using Jochem Kroezen database with update from Cambrinus
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# `````````````````````````````````````````````````````````````````````````````````````````````````` 

# PACKAGES
BasePackages <- c("foreign", "stringr", "gdata", "car", "zoo", "tidyr", "RColorBrewer", "plyr", "dplyr", "ggplot2")
lapply(BasePackages, library, character.only = TRUE)
SpatialPackages <- c("rgdal", "gdalUtils", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4")
lapply(SpatialPackages, library, character.only = TRUE)
#AdditionalPackages <- c("RCurl", "R.utils", "GSIF", "XML", "plotKML", "raincpc")
#lapply(AdditionalPackages, library, character.only = TRUE)

# SET WORKING DIRECTORY
wdpath<-"D:\\Dijk158\\Dropbox\\Michiel_research\\Micro Brewery"
setwd(wdpath)

# SOURCE functions
#source("")

# R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

# PROCESS CAMBRINUS DATABASE
# Add new columns, set codes for missing data and remove non-essential columns

# read data
Cambr.raw<-read.xls("Analysis\\Data\\Bierbrouwerijen_NL_raw.xlsx", sheet=1, na.strings=c(""))
# Add new columns
# It is assumed that 2014 is the 'present' 
Brew.df <- Cambr.raw %>%
            mutate(IDCode = ZOEK_BRW,
                   ENTRY = ifelse(STRT_BRW == 0, NA, STRT_BRW),
                   EXIT = ifelse(STOP_BRW == 9999, NA, STOP_BRW),
                   EXIT = ifelse(EXIT == 0, 9999, EXIT)) %>%
            dplyr::select(IDCode, ENTRY, EXIT)  




Brew.PhD <- read.xls("Analysis\\Data\\Breweries_PhD.xlsx", sheet=1, na.strings=c(""))

check <- filter(Brew.raw, ZOEK_BRW %in% Brew.PhD$IDCode)
check <- filter(Brew.PhD, !(IDCode %in% Brew.df$IDCode))

check <- filter(Brew.PhD, (IDCode %in% Brew.df$IDCode)) %>%
          left_join(., Brew.df) %>%
          filter(ENTRY != FoundYO) %>%
          dplyr::select(IDCode, FoundYO, ENTRY)

check <- filter(Brew.PhD, (IDCode %in% Brew.df$IDCode)) %>%
  left_join(., Brew.df) %>%
  filter(EXIT != FailYO & FailYO != 9999) %>%
  dplyr::select(IDCode, FailYO, EXIT)

# Recode ENTRY using information from Jochem
# Recode EXIT using information from Jochem
# Recode large breweries



& !(FoundYO == Brew.df$ENTRY))
