---
  title: "Modeling SST at sampling sites"
author: "Jill Carr"
date: "1/7/2025"

---
  
#extract ERDDAP satellite SST data

install.packages("rerddap")
library(rerddap)

#extract data from NOAA ERDDAP
dataInfo <- rerddap::info("jplMURSST41", url="coastwatch.pfeg.noaa.gov/erddap")
MassSST <- griddap(dataInfo, fields = "analysed_sst", latitude = c(41,43), longitude = c(-72,-69), time = c("2024-12-08","2024-01-01"))
#covert datetime from character to date
MassSST$data$time <- as.Date(MassSST$data$time)

str(MassSST$data)

#viz ERDDAP data

install.packages("plotdap")
library(plotdap)

add_griddap(plotdap(crs = "EPSG:6492"), 
  MassSST, 
  ~analysed_sst,
  time = identity,
  maxpixels = 100000,
  animate = FALSE)

#TBD: rerddaoXtracto can extract data from an irregularly shaped polygon (or from a 3D box, or data along a trajectory)
#use rxtractogon() for 3D polygon. 

install.packages("rerddapXtracto")
library(rerddapXtracto)

#extract data from NOAA ERDDAP (returns data in array rather than melted)
dataInfo <- rerddap::info("jplMURSST41", url="coastwatch.pfeg.noaa.gov/erddap")
MassSST <- rxtracto_3D(dataInfo, parameter = "analysed_sst", ycoord = c(41,43), xcoord = c(-72,-69), tcoord = c("2024-12-08","2024-01-01"))
#need to figure out how to define the shape


#compare HOBO and satellite data

install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")
library(dplyr)
library(lubridate)
library(ggplot2)

#read in HOBO data
#JC working directory
setwd("C:/Users/jillian.carr/OneDrive - University of Massachusetts Boston/Documents/MassBays/eelgrass seeding/WHOI_Sea_Grant/modeling")
d.t.AQ <- read.csv("C:/Users/jillian.carr/OneDrive - University of Massachusetts Boston/Documents/MassBays/eelgrass seeding/WHOI_Sea_Grant/modeling/hobo/21177827_AQ.csv")
d.t.CB <- read.csv("C:/Users/jillian.carr/OneDrive - University of Massachusetts Boston/Documents/MassBays/eelgrass seeding/WHOI_Sea_Grant/modeling/hobo/21258498_CB.csv")
d.t.CI <- read.csv("C:/Users/jillian.carr/OneDrive - University of Massachusetts Boston/Documents/MassBays/eelgrass seeding/WHOI_Sea_Grant/modeling/hobo/20536645_CI.csv")
d.t.PC <- read.csv("C:/Users/jillian.carr/OneDrive - University of Massachusetts Boston/Documents/MassBays/eelgrass seeding/WHOI_Sea_Grant/modeling/hobo/21177829_PC.csv")
d.t.DC <- read.csv("C:/Users/jillian.carr/OneDrive - University of Massachusetts Boston/Documents/MassBays/eelgrass seeding/WHOI_Sea_Grant/modeling/hobo/21258490_DC.csv")
d.t.LH <- read.csv("C:/Users/jillian.carr/OneDrive - University of Massachusetts Boston/Documents/MassBays/eelgrass seeding/WHOI_Sea_Grant/modeling/hobo/10942817_LH.csv")
d.t.SH <- read.csv("C:/Users/jillian.carr/OneDrive - University of Massachusetts Boston/Documents/MassBays/eelgrass seeding/WHOI_Sea_Grant/modeling/hobo/21177828_SH.csv")
sites <- read.csv("C:/Users/jillian.carr/OneDrive - University of Massachusetts Boston/Documents/MassBays/eelgrass seeding/WHOI_Sea_Grant/modeling/site.coords.csv")
#clean and filter HOBO data
d.t.AQ <- d.t.AQ %>%
  mutate(site.id = "AQ")
d.t.CB <- d.t.CB %>%
  mutate(site.id = "CB")
d.t.CI <- d.t.CI %>%
  mutate(site.id = "CI")
d.t.DC <- d.t.DC %>%
  mutate(site.id = "DC")
d.t.LH <- d.t.LH %>%
  mutate(site.id = "LH")
d.t.PC <- d.t.PC %>%
  mutate(site.id = "PC")
d.t.SH <- d.t.SH %>%
  mutate(site.id = "SH")

str(d.t.SH)

d.t.2024 <- bind_rows(d.t.AQ, d.t.CB, d.t.CI, d.t.DC, d.t.LH, d.t.PC, d.t.SH) %>%
  mutate(date.time = mdy_hm(date.time)) %>%
  filter(date.time > "2024-05-15 19:45") %>%
  filter(date.time < "2024-09-29 20:00") %>%
  rename(light = Intensity.lum.per.sqft)

str(d.t.2024)

#merge site coordinates into dataframe
d.t.2024.merged <- merge(d.t.2024, sites, all = TRUE)

#plot HOBO data
plot(d.t.2024.merged$date.time, d.t.2024.merged$temp.C, type='l', xlab='Date', ylab='(ºC)',main='WHOI HOBO temps')

#Convert HOBO data to daily means
d.t.2024.merged.day = d.t.2024.merged %>%
  mutate(date = as.character(floor_date(date.time, unit="days"))) %>%
  group_by(d.t.2024.merged$site.id, date) %>%
  summarize(
    lon=mean(d.t.2024.merged$longitude),
    lat=mean(d.t.2024.merged$latitude),
    temp.daily.C=mean(temp.C, na.omit = TRUE),
  )

colnames(d.t.2024.merged.day) <- c("site.id", "date", "lon", "lat", "temp.daily.C")

#omit NA rows
d.t.2024.merged.day <- na.omit(d.t.2024.merged.day)

head(d.t.2024.merged.day)

#visualize downsampled hobo data

ggplot(d.t.2024.merged.day, aes(x = date, y = temp.daily.C, group = site.id)) + 
         geom_line(aes(color = d.t.2024.merged.day$site.id)) +
  geom_point(aes(color = d.t.2024.merged.day$site.id))



#match up satellite data to hobo data

dataInfo <- rerddap::info("jplMURSST41", url="coastwatch.pfeg.noaa.gov/erddap") #from earlier step

#set parameters
parameter <- 'analysed_sst'     #from jplMURSST41
xcoord <- d.t.2024.merged.day$lon   #from HOBO data
ycoord <- d.t.2024.merged.day$lat     #from HOBO data
tcoord <- d.t.2024.merged.day$date    #from HOBO data

#extract satellite data - not working yet, seeking help from dev
extract <- rxtracto(dataInfo, parameter=parameter, 
                    tcoord=tcoord,
                    xcoord=xcoord,
                    ycoord=ycoord,
                    xlen=0.1,ylen=0.1)

#add new sst field to df
d.t.2024.merged.day$sst<-extract$'mean analysed_sst'

str(d.t.2024.merged.day)


#plot Hobo vs SST temperatures
ggplot(d.t.2024.merged.day, aes(x=temp.daily.C, y=sst, color=site.id)) + 
  coord_fixed(xlim=c(8,25),ylim=c(8,25)) +
  geom_point()

#still working on plot aesthetic
  ylab('Satellite SST')  + 
  xlab('HOBO average daily temp') +
  scale_x_continuous(minor_breaks = seq(8, 25)) + 
  scale_y_continuous(minor_breaks = seq(8, 25)) + 
  #geom_abline(a=fit[1],b=fit[2]) +
  #annotation_custom(my_grob) + 
  #scale_color_gradientn(colours = "viridis", name="HOBO\Latitude") +
  scale_color_viridis(discrete = FALSE, name="HOBO\Latitude") +
  labs(title=main) + theme(plot.title = element_text(size=20, face="bold", vjust=2)) 

#linear regression
lmHeight = lm(sst~temp.daily.C, data = d.t.2024.merged.day)
summary(lmHeight)

#Create a map of SST and overlay the buoy data
#tutorial: https://github.com/coastwatch-training/CoastWatch-Tutorials/blob/main/matchup-satellite-buoy-data/R/matchup_satellite_buoy_data.md