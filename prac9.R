#library a bunch of packages we may (or may not) use - install them first if not installed already. 
library(tidyverse)
library(tmap)
library(geojsonio)
library(plotly)
library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(sf)
library(sp)
library(spdep)
library(car)

#Set up some spatial data
getwd()

#read some ward data in

#download a zip file containing some boundaries we want to use
download.file("https://data.london.gov.uk/download/statistical-gis-boundary-files-london/9ba8c833-6370-4b11-abdc-314aa020d5e0/statistical-gis-boundaries-london.zip", destfile="prac9_data/statistical-gis-boundaries-london.zip")

#unzip it
unzip("prac9_data/statistical-gis-boundaries-london.zip", exdir="prac9_data")

#look at the subfolders in the directory to work out where we want to get our shapefile from
list.dirs("prac9_data/statistical-gis-boundaries-london")

#read in the boundaries from the file you have just unzipped into your working directory
LondonWardsss <- readOGR("prac9_data/statistical-gis-boundaries-london/ESRI/London_Ward_CityMerged.shp", layer="London_Ward_CityMerged")

#convert it to a simple features object
LondonWardsssSF <- st_as_sf(LondonWardsss)

#check coordinate reference system
LondonWardsssSF

#Proj4 string tells me it's in wgs84, so Convert to British National Grid
BNG = "+init=epsg:27700"
LondonWardsssSFBNG <- st_transform(LondonWardsssSF, BNG)

#check the data
qtm(LondonWardsssSFBNG)

#read in some attribute data
LondonWardProfiles <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv", col_names = TRUE, locale = locale(encoding = 'Latin1'))

#check all of the columns have been read in correctly
str(LondonWardProfiles)

#We can use readr to deal with the issues in this dataset - which are to do with text values being stored in columns containing numeric values

#read in some data - couple of things here. Read in specifying a load of likely 'n/a' values, also specify Latin1 as encoding as there is a pound sign (£) in one of the column headers - just to make things fun!
LondonWardProfiles <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv", na = c("", "NA", "n/a"), locale = locale(encoding = 'Latin1'), col_names = TRUE)

#check all of the columns have been read in OK
str(LondonWardProfiles)

#merge boundaries and data
LonWardProfiles <- left_join(LondonWardsssSFBNG, LondonWardProfiles, by = c("GSS_CODE" = "New code"))

tmap_mode("view")

qtm(LonWardProfiles, fill = "Average GCSE capped point scores - 2014", borders = NULL)

#might be a good idea to see where the secondary schools are in London too
london_schools <- read_csv("https://data.london.gov.uk/download/london-schools-atlas/57046151-39a0-45d9-8dc0-27ea7fd02de8/all_schools_xy_2016.csv")

#from the coordinate values stored in the x and y columns, which look like they are latitude and longitude values, create a new points dataset
lon_schools_sf <- st_as_sf(london_schools, coords = c("x","y"), crs = 4326)

#now pull out the secondary schools
#these are the same - one uses grep() and one uses the stringr() package

lond_sec_schools_sf <- lon_schools_sf[str_which(lon_schools_sf[["PHASE"]],"Secondary"),]
lond_sec_schools_sf <- lon_schools_sf[grep("Secondary",lon_schools_sf[["PHASE"]]),]

tmap_mode("view")

qtm(lond_sec_schools_sf)

q <- qplot(x = `Unauthorised Absence in All Schools (%) - 2013`, y = `Average GCSE capped point scores - 2014`, data=LonWardProfiles)

#plot with a regression line - note, I've added some jitter here as the x-scale is rounded
q + stat_smooth(method="lm", se=FALSE, size=1) + geom_jitter()








