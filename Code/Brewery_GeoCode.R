# PROJECT: Micro-brewery
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# Purpose
# Geocode location of breweries 
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# `````````````````````````````````````````````````````````````````````````````````````````````````` 

# PACKAGES
BasePackages <- c("foreign", "stringr", "gdata", "car", "zoo", "tidyr", "RColorBrewer", "plyr", "dplyr", "ggplot2")
lapply(BasePackages, library, character.only = TRUE)
SpatialPackages <- c("maps", "rgdal", "gdalUtils", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4")
lapply(SpatialPackages, library, character.only = TRUE)
#AdditionalPackages <- c("RCurl", "R.utils", "GSIF", "XML", "plotKML", "raincpc")
#lapply(AdditionalPackages, library, character.only = TRUE)

# SET WORKING DIRECTORY
wdpath<-"D:\\Dijk158\\Dropbox\\Michiel_research\\Micro Brewery\\"
setwd(wdpath)

# SOURCE functions
#source("")

# R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

# FUNCTIONS
# Geocode and plot location of breweries
#CHECK!! http://stackoverflow.com/questions/22335893/get-map-does-not-download-full-extent-of-map-wanted

# Function to gecode and put coordinates in data.frame
geocode.f<-function(address){
  if(!is.na(address)){gc<-geocode(address)} else{gc<-NA}
  gc<-as.numeric(gc)
  df<-data.frame(x=gc[1], y=gc[2])
  return(df)
}

# function to load google maps and store them so you do can work offline.
load.map <- function(Locale, Lon, Lat, MapZoom){
  MapName <- paste("Map", gsub(" ", "", Locale), MapZoom, sep = "")
  
  FileName <- paste(MapName,".RData", sep = "")
  if (file.exists(FileName) & ReloadMaps == 0)
  {
    load(FileName, envir = .GlobalEnv)
  } else 
  {
    Map <- get_googlemap(center=c(lon = Lon, lat = Lat), zoom=MapZoom, scale = 2,
                         size = c(640, 640), maptype = "roadmap", color = "color", format = "png8")
    assign(MapName, Map,  envir = .GlobalEnv)
    save(list = MapName, file = FileName, envir = .GlobalEnv)
  }  
} 

# CODE

# GET DATA
Brew.raw<-read.xls(".\\Analysis\\Data\\Bierbrouwerijen_NL_clean.xlsx", sheet=1, na.strings=c(""))

# CLEAN DATA
Brew.df<-Brew.raw
Brew.df$X<-NULL
Brew.df<-Brew.df[is.na(Brew.df$REMOVE),]
Brew.df<-Brew.df[!(Brew.df$STOP_BRW==9999),] # remove breweries for which Exit date is not know

# Geocode exact address
# Select adres. First the standard, if not available, the alternative adres.
Geocode<-Brew.df[,c(1:3)]
Geocode$Address<-apply(Brew.df[,c(6,7,8)], 1, function(x) paste(na.omit(x),collapse=", ") )
Geocode$Address.alt<-apply(Brew.df[,c(22,23,24)], 1, function(x) paste(na.omit(x), collapse=", ") )
Geocode$Address.final<-ifelse(Geocode$Address!="", Geocode$Address, Geocode$Address.alt)
Geocode$Address.final<-ifelse(Geocode$Address.final!="", paste(Geocode$Address.final, "The Netherlands", sep=" "),NA)

# Geocode city only
# Select city. First the standard, if not available, the alternative adres.
Geocode$City <-ifelse(!is.na(Brew.df$PLTS_BRW), Brew.df$PLTS_BRW, Brew.df$XPLA_BRW)
Geocode$City <-ifelse(!is.na(Geocode$City), paste(Geocode$City, "The Netherlands", sep=", "),NA)

# Obtain geocodes
gc<-ddply(Geocode,.(ID), function(x) geocode.f(x$City))
save(gc, file="geocodes.RData")
load(".\\Analysis\\Maps\\geocodes.RData") 
gc <- rename(gc, lon = x, lat = y)

# Merge database and geocodes
Brew.map <- merge(Brew.df, gc, by=c("ID")) %>%
            filter(GROP_BRW == "Onafhankelijk") %>%
            mutate(City = ifelse(!is.na(PLTS_BRW), PLTS_BRW, XPLA_BRW)) %>%           
            dplyr::select(ID, NAAM_BRW, City, STRT_BRW, STOP_BRW, PLTS_BRW, lon, lat) %>%
            arrange(City)

Brew.mapp1 <- group_by(Brew.map, City) %>%
                filter(STRT_BRW <= 1985 & (STOP_BRW>1985 | STOP_BRW==0)) %>%
                summarize(count=n())

Brew.mapp2 <- group_by(Brew.map, City) %>%
              filter(STRT_BRW < 2003 & (STOP_BRW>=2003 | STOP_BRW==0)) %>%
              summarize(count=n())

Brew.mapp3<- group_by(Brew.map, City) %>%
            filter(STRT_BRW < 2013 & (STOP_BRW>=2013 | STOP_BRW==0)) %>%
            summarize(count=n())

# Compute number of breweries per year
NLD.map<-qmap("Netherlands", zoom=7, color = "color", legend = "topleft") # qmap is identical to getmap and ggmap combined.
gglocator(2) # to idenify corners of map with mouse.
NLD.map<-map

library(rgeos) # alternative for gpclib
library(raster)
library(rgdal)
library(sp)
library(mapproj)
library(maptools)
library(proj4)
library(RColorBrewer)

# SETUP MAPS
# select cities in the Netherlands
data(world.cities) # world city database from Maps
NLD.cities<-subset(world.cities, country.etc=="Netherlands" & pop>50000)
NLD.map+geom_point(data=NLD.cities,aes(x=long, y=lat, colour=capital, size=pop))

NLD.GADM0<-getData('GADM',country="NLD",level=0)
NLD.GADM1<-getData('GADM',country="NLD",level=1)
NLD.GADM2<-getData('GADM',country="NLD",level=2)
NLD.GADM0<-fortify(NLD.GADM1)
NLD.GADM1<-fortify(NLD.GADM1)
NLD.GADM2<-fortify(NLD.GADM2)
save(NLD.GADM0, file="NLD.GADM0.rda")
save(NLD.GADM1, file="NLD.GADM1.rda")
save(NLD.GADM2, file="NLD.GADM2.rda")
save(NLD.map, file="NLD.rda")
#load("Data\\Spatial\\TZA Maps\\TZA.GADM1.rda")
#load("Data\\Spatial\\TZA Maps\\TZA.GADM2.rda")
#projection(TZA.GADM1) # find projection for TZA
#projection(TZA.GADM) # projection is the same as google maps: WGS84.

NLD.coord <- as.vector(bbox(NLD.GADM0))
NLD.map<-qmap(NLD.coord, zoom=8, color = "color", legend = "topleft", source = c("osm")) # qmap is identical to getmap and ggmap combined.
NLD.map

NLD.map2<-NLD.map+geom_polygon(data=NLD.GADM0, aes(x=long, y=lat, group=group), fill="grey40", colour="black", alpha = .3, size = .1)+
  geom_path(data=NLD.GADM0, aes(x=long, y=lat, group=group), colour="black", size = 1)
NLD.map2

ggplot()+geom_polygon(data=NLD.GADM2, aes(x=long, y=lat, group=group), fill="grey40", colour="black", alpha = .3, size = .1)+
  geom_path(data=NLD.GADM1, aes(x=long, y=lat, group=group), colour="black", size = 1)
NLD.map2

sel<-subset(Brew.df, STRT_BRW<=2014 & (STOP_BRW>=2014 | STOP_BRW==0))
sel<-subset(Brew.df, STRT_BRW<=1600 & (STOP_BRW>=1600 | STOP_BRW==0))
sel<-subset(Brew.df, STRT_BRW<=1990 & (STOP_BRW>=1990 | STOP_BRW==0))

overlay <- stat_density2d(
  aes(x = x, y = y, fill = ..level.., alpha = ..level..),
  bins = 4, geom = "polygon",
  data = sel)


NLD.map+geom_point(data=sel,aes(x=x, y=y), colour="red")
ggplot()+geom_point(data=Geocode,aes(x=x, y=y), colour="red")+coord("Mercator")+
  geom_path(data=NLD.GADM1, aes(x=long, y=lat, group=group), colour="black", size=1)
