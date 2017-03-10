# PROJECT: Micro-brewery
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# Purpose
# Prepare plots
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# `````````````````````````````````````````````````````````````````````````````````````````````````` 

# PACKAGES
BasePackages <- c("foreign", "stringr", "gdata", "car", "zoo", "tidyr", "RColorBrewer", "plyr", "dplyr", "ggplot2", "scales", "openxlsx")
lapply(BasePackages, library, character.only = TRUE)
SpatialPackages <- c("maps", "rgdal", "gdalUtils", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4")
lapply(SpatialPackages, library, character.only = TRUE)
#AdditionalPackages <- c("RCurl", "R.utils", "GSIF", "XML", "plotKML", "raincpc")
#lapply(AdditionalPackages, library, character.only = TRUE)

# SET WORKING DIRECTORY
wdpath<-"D:\\Dropbox\\Michiel_research\\Micro Brewery"
setwd(wdpath)

# R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


# Micro-breweries and client brewers
Brew.raw <- read.xlsx("Analysis\\Data\\20160406_Brouwerijen_NL_raw.xlsx")

# CLEAN DATA
# remove breweries for which Exit date is not know but which are no longer in operation
Brew.df <- Brew.raw %>%
            filter(STOP_BRW != 9999) 

# Remove breweries that are part of a conglomerate so should not be counted
# Probably need to look for takeover years to do this properly.
Brew.df <- Brew.df %>% filter(!(NUMM_BRW %in% c(317, 399, 284, 395, 389, 396, 369,
                              163, 164, 
                              157, 393,
                              1234,
                              325, 323,
                              160, 1019, 398, 394, 391, 382, 386, 314, 359, 344, 342, 345, 337, 1031, 354,
                              152, 175, 180, 178, 149)))
                              

# Number of breweries in 1980. Should be 14 according to PINT but I only find 13
Number1970 <- filter(Brew.df, STRT_BRW<=1970 & (STOP_BRW>=1970 | STOP_BRW==0))
Number1981 <- filter(Brew.df, STRT_BRW<=1981 & (STOP_BRW>=1981 | STOP_BRW==0))
Number1986 <- filter(Brew.df, STRT_BRW<=1986 & (STOP_BRW>=1986 | STOP_BRW==0))
Number2003 <- filter(Brew.df, STRT_BRW<=2003 & (STOP_BRW>=2003 | STOP_BRW==0))
Add19862003 <- filter(Brew.df, STRT_BRW>=1986 & STRT_BRW<=2003 & (STOP_BRW>=2003 | STOP_BRW==0))
Number2015 <- filter(Brew.df, STRT_BRW<=2015 & (STOP_BRW>=2015 | STOP_BRW==0))
check <- filter(Number2015, CATE_BRW=="Brouwerijhuurder")

# Breweries by city in 2015
Amsterdam <- filter(Number2015, PLTS_BRW == "Amsterdam" | XPLA_BRW =="Amsterdam")
Rotterdam <- filter(Number2015, PLTS_BRW == "Rotterdam" | XPLA_BRW =="Rotterdam")
DenHaag <- filter(Number2015, PLTS_BRW == "Den Haag" | XPLA_BRW =="Den Haag")
Utrecht <- filter(Number2015, PLTS_BRW == "Utrecht" | XPLA_BRW =="Utrecht")
Nijmegen <- filter(Number2015, PLTS_BRW == "Nijmegen" | XPLA_BRW =="Nijmegen")
PLTS <- as.data.frame(table(Number2015$PLTS_BRW))
XPLA <- as.data.frame(table(Number2015$XPLA_BRW))

# Number of breweries: 1819-2014
# Total number from Unger and Jochem. We use Unger for <1900 and Cambrinus for 1900>
Brewery_Density <- read.xls("Analysis\\Data\\Brewery_Density.xlsx", sheet=1, na.strings=c("")) %>%
  do(filter(., complete.cases(.))) %>%
  filter(Year < 1900) %>%
  dplyr::select(-Source)



# COMPUTE ENTRY, EXIT AND NUMBER OF BREWERIES USING CAMBRINUS
# Total number of breweries
Total.Count <- data.frame(Year = c(1901:2015))
Total.Count$Number<-0
for (yr in Total.Count$Year){
  TMP <- Brew.df[(Brew.df$STRT_BRW<=yr & (Brew.df$STOP_BRW>=yr|Brew.df$STOP_BRW==0)),]
  Total.Count$Number[Total.Count$Year==yr] <- nrow(TMP)
  rm(TMP)
}

Brewery_Density <- rbind(Brewery_Density, Total.Count)

p = ggplot() +
  geom_line(data = Brewery_Density, aes(x = Year, y = Number), size = 1.5) +
  theme_bw() +
  ylab("Number of breweries")+xlab("")+
  geom_rect(aes(xmin = 1978, xmax = 2018, ymin = 0, ymax = 420), alpha=0, colour="dark grey", size = 1, linetype=1) +   
  scale_x_continuous(limits=c(1800,2018), breaks=seq(1800,2018, 10))+
  annotate("text", x = 1815, y = 900, label = "Decline", size=4) +     
  geom_vline(xintercept = 1850, linetype="dashed") +
  annotate("text", x = 1910, y = 900, label = "Modernisation, concentration and upscaling", size=4) + 
  geom_vline(xintercept = 1970, linetype="dashed") +
  annotate("text", x = 1975, y = 900, label = "Foundations\n for renewal", size=4) + 
  geom_vline(xintercept = 1981, linetype="dashed") +
  annotate("text", x = 1992, y = 750, label = "Emergence of\n Microbreweries", size=4) + 
  geom_vline(xintercept = 2003, linetype="dashed") +
  annotate("text", x = 2011, y = 900, label = "Rapid expansion\n of Microbreweries", size=4) + 
  scale_y_continuous(labels = comma) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(p)
ggsave(plot = p, ".\\Figures\\Number_of_Breweries.png", h = 15, w = 25, unit="cm", type = "cairo-png")
write.csv(Brewery_Density, "Analysis/Data/Brewery_density_final.csv")

### NUMBER OF MICROBREWERIES
# Number of breweries that were established after 1981, including entry and exit patterns
# Note that the Arcense Stoombrouwerij is not counted as entry in 1981 because it already existed before and later turned into Hertog Jan
micro.Count <- data.frame(Year = c(1981:2015))
micro.df <- filter(Brew.df, STRT_BRW >= 1981)
micro.Count$Number<-0

for (yr in micro.Count$Year){
  TMP <- micro.df[(micro.df$STRT_BRW<=yr & (micro.df$STOP_BRW>=yr|micro.df$STOP_BRW==0)),]
  micro.Count$Number[micro.Count$Year==yr] <- nrow(TMP)
  rm(TMP)
}

write.csv(micro.Count, "Analysis/Data/micro_density_final.csv")

# Number of Brouwerijhuurders
# COMPUTE ENTRY, EXIT AND NUMBER OF BREWERIES
# Total number of breweries
Huur.Count <- data.frame(Year=c(1900:2015))
Huur.Count$Number<-0
for (yr in Huur.Count$Year){
  TMP<-Brew.df[(Brew.df$CATE_BRW=="Brouwerijhuurder" & Brew.df$STRT_BRW<=yr & (Brew.df$STOP_BRW>=yr|Brew.df$STOP_BRW==0)),]
  Huur.Count$Number[Huur.Count$Year==yr]<-nrow(TMP)
  rm(TMP)
}

Huur.Count$Type<-"Contract brewery"


# Number of Bierbrouwerijen
# Total number of breweries, includes multinationals etc
Brew.Count<-data.frame(Year=c(1900:2015))
Brew.Count$Number<-0
for (yr in Brew.Count$Year){
  TMP<-Brew.df[(Brew.df$CATE_BRW !="Brouwerijhuurder" & Brew.df$STRT_BRW<=yr & (Brew.df$STOP_BRW>=yr|Brew.df$STOP_BRW==0)),]
  Brew.Count$Number[Brew.Count$Year==yr]<-nrow(TMP)
  rm(TMP)
}

Brew.Count$Type<-"Brewery"
Total.Count$Type<-"Total"


Count <- rbind(Brew.Count, Huur.Count, Total.Count) %>%
          filter(Year>=1970)

# Set order of factor levels for plot
Count$Type<-factor(Count$Type)
print(levels(Count$Type))
Count$Type <-factor(Count$Type,levels(Count$Type)[c(1,3,2)])

# PLots
# Perhaps split total breweries in: (1) start-up before 1980 and (2) start-up after 1980
library(gridExtra)
ggplot()+geom_line(data=Total.Count, aes(x=Year, y=Number))
ggplot()+geom_line(data=Brew.Count, aes(x=Year, y=Number))
ggplot()+geom_line(data=Huur.Count, aes(x=Year, y=Number))

# NB: legend for both graphs is based on legend for p2 so make sure layout and colours match
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

p1=ggplot()+
  geom_line(data=Count[Count$Year %in% c(1900:2016) & Count$Type %in% c("Total"),], aes(x=Year, y=Number, colour=Type), size=1)+
  scale_colour_manual(values=c("black", "black", "blue"))+
  ylab("Number")+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  theme(legend.title=element_blank())+
  theme(legend.key = element_rect(colour = NA))
#scale_linetype_manual(values=c("dotdash", "dotted"))+ 
p1

p2=ggplot()+
  geom_line(data=Count[Count$Year %in% c(1975:2016),], aes(x=Year, y=Number, linetype=Type),size=1)+
  #geom_point(data=Count[Count$Year %in% c(1975:2016),], aes(x=Year, y=Number, shape=Type),size=1)+
  # scale_colour_manual(values=c("blue", "black", " green"), breaks=c("Total", "Brewery", "Contract brewery"), 
  #                     labels=c("Total", "Brewery", "Contract brewery"))+
  scale_linetype_manual(values=c("longdash", "solid", "dotdash"), breaks=c("Total", "Brewery", "Contract brewery"), 
                        labels=c("Total", "Brewery", "Contract brewery")) + 
  theme_bw()+
  theme(axis.title.x = element_blank()) +   # Remove x-axis label
  theme(legend.title=element_blank())+
  theme(legend.key = element_rect(colour = NA)) +
  theme(axis.line = element_line(colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()) +
  ylab("Number of breweries")+
  theme(legend.position="bottom")+
  geom_vline(xintercept = 1981, linetype="dashed") +
  annotate("text", x = 1992, y = 350, label = "Emergence of Microbreweries", size=4) + 
  geom_vline(xintercept = 2003, linetype="dashed") +
  annotate("text", x = 2009, y = 350, label = "Expansion of Microbreweries", size=4) + 
  scale_y_continuous(labels = comma) +
  scale_x_continuous(limits=c(1975,2015), breaks=seq(1975,2015, 5))


p2
ggsave(plot = p2, ".\\Figures\\Type_of_Breweries.png", h = 15, w = 25, unit="cm", type = "cairo-png")
write.csv(Count, "Analysis/Data/Brewery_count_final.csv")

mylegend<-g_legend(p2)
lwidth <- sum(mylegend$width)

ppi <- 300
#png("Breweries.png", width=8*ppi, height=6*ppi, res=ppi)
p3 <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                               p2 + theme(legend.position="none"),
                               nrow=1),
                   mylegend,
                   nrow=2,heights=c(10, 1))

#dev.off()

pdf(".\\Analysis\\Maps\\Breweries.pdf")
p3 <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                               p2 + theme(legend.position="none"),
                               nrow=1),
                   mylegend,
                   nrow=2,heights=c(10, 1))

dev.off()


p3

# Geocode and plot location of breweries
require(maps)
require(ggmap)
library(mapproj)

geocode.f<-function(address){
  if(!is.na(address)){gc<-geocode(address)} else{gc<-NA}
  gc<-as.numeric(gc)
  df<-data.frame(x=gc[1], y=gc[2])
  return(df)
}


load(".\\Analysis\\Maps\\geocodes.RData") 
Brew.map <- merge(Brew.df, gc, by=c("ID")) %>%
  filter(GROP_BRW == "Onafhankelijk") %>%
  dplyr::rename(lon=x, lat=y) %>%
  mutate(City = ifelse(!is.na(PLTS_BRW), PLTS_BRW, XPLA_BRW)) %>%           
  dplyr::select(ID, NAAM_BRW, City, STRT_BRW, STOP_BRW, PLTS_BRW, lon, lat) %>%
  arrange(City)

Brew.mapp1 <- group_by(Brew.map, City) %>%
  filter(STRT_BRW <= 1990 & (STOP_BRW>1990 | STOP_BRW==0)) %>%
  summarize(count=n()) %>% 
  ddply(.,.(City, count), function(x) geocode.f(x$City))

Brew.mapp2 <- group_by(Brew.map, City) %>%
  filter(STRT_BRW < 2003 & (STOP_BRW>=2003 | STOP_BRW==0)) %>%
  summarize(count=n()) %>%
  ddply(.,.(City, count), function(x) geocode.f(x$City))


Brew.mapp3 <- group_by(Brew.map, City) %>%
  filter(STRT_BRW < 2013 & (STOP_BRW>=2013 | STOP_BRW==0)) %>%
  summarize(count=n()) %>%
  ddply(.,.(City, count), function(x) geocode.f(x$City)) 

# Merge database and geocodes
# Get maps
NLD.map<-qmap("Netherlands", zoom=7, color = "color", legend = "topleft") # qmap is identical to getmap and ggmap combined.
NLD.GADM1<-getData('GADM',country="NLD",level=1)
bbox(NLD.GADM1)
NLD.GADM1_fort<-fortify(NLD.GADM1)
NLD2<- openmap(c(lat=53.555, lon= 3.361), c(lat=50.755,lon=7.293), type="osm-bw")
plot(NLD2)
# SETUP MAPS
# select cities in the Netherlands
data(world.cities) # world city database from Maps
NLD.cities<-subset(world.cities, country.etc=="Netherlands" & pop>100000)

# Base map with all cities
autoplot(NLD2) + geom_point(data=Brew.map2013, aes(x=x, y=y, size=count))
NLD.map+geom_point(data=Brew.map2013, aes(x=x, y=y, size=count))

Brew.mapp1$class<-cut(Brew.mapp1$count, breaks=c(0,1,2, 8))
ggplot() + 
  geom_polygon(data=NLD.GADM1[NLD.GADM1@data$ID_1 != 6 & NLD.GADM1@data$ID_1 != 13,], aes(x=long, y=lat, group=group), colour="black", fill="grey") +
  coord_map("mercator") +
  labs(x="", y="")+
  theme_bw() +
  geom_point(data= subset(Brew.mapp1,x>3 & y>50), aes(x=x, y=y, size=class), colour = "red") +
  scale_size_manual(name = "Number of \n breweries", labels = c("1", "2", ">2"), values= c(2,4,6)) +
  geom_text(data= subset(Brew.mapp1,x>3 & y>50 & count>0), aes(x = x, y = y*1.001, label=City),hjust=0.5, vjust=0, size=3)+
  theme(legend.key = element_blank(),
        line = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        legend.position="none")
ggsave(".\\Figures\\Location_p1.png", h = 20, w = 25, unit="cm", type = "cairo-png")


Brew.mapp2$class<-cut(Brew.mapp2$count, breaks=c(0,1,2, 8))
ggplot() +
  geom_polygon(data=NLD.GADM1[NLD.GADM1@data$ID_1 != 6 & NLD.GADM1@data$ID_1 != 13,], aes(x=long, y=lat, group=group), colour="black", fill="grey") +
  coord_map("mercator") +
  labs(x="", y="")+
  theme_bw() +
  geom_point(data= subset(Brew.mapp2,x>3 & y>50), aes(x=x, y=y, size=class), colour = "red") +
  scale_size_manual(name = "Number of \n breweries", labels = c("1", "2", ">2"), values= c(2,4,6)) +
  geom_text(data= subset(Brew.mapp2,x>3 & y>50 & count>0), aes(x = x, y = y*1.001, label=City), hjust=0.5, vjust=0, size=3)+
  theme(legend.key = element_blank(),
        line = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        legend.position="none")
ggsave(".\\Figures\\Location_p2.png", h = 20, w = 25, unit="cm", type = "cairo-png")

Brew.mapp3$class<-cut(Brew.mapp3$count, breaks=c(0,1,2, 8))
ggplot() +
  geom_polygon(data=NLD.GADM1[NLD.GADM1@data$ID_1 != 6 & NLD.GADM1@data$ID_1 != 13,], aes(x=long, y=lat, group=group), colour="black", fill="grey") +
  coord_map("mercator") +
  labs(x="", y="")+
  theme_bw() +
  geom_point(data= subset(Brew.mapp3,x>3 & y>50), aes(x=x, y=y, size=class), colour = "red") +
  scale_size_manual(name = "Number of \n breweries", labels = c("1", "2", ">2"), values= c(2,4,6)) +
  geom_text(data= subset(Brew.mapp3,x>3 & y>50 & count>1), aes(x = x, y = y*1.001, label=City),hjust=0.5, vjust=0, size=3)+
  theme(legend.key = element_blank(),
        line = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank())
ggsave(".\\Figures\\Location_p3.png", h = 20, w = 25, unit="cm", type = "cairo-png")

+
  scale_size_discrete(name  ="Number of breweries")
,
labels=c("Woman", "Man", "test"))

geom_point(data=NLD.cities, aes(x=long, y=lat))

+
  theme(legend.position="none")



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
