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

#run the linear regression model and store its outputs in an object called model1
model1 <- lm(`Average GCSE capped point scores - 2014` ~ `Unauthorised Absence in All Schools (%) - 2013`, data = LonWardProfiles)

#show the summary of those outputs
summary(model1)

model1_res <- tidy(model1)

#let's check the distribution of these variables first
ggplot(LonWardProfiles, aes(x=`Average GCSE capped point scores - 2014`)) + geom_histogram(aes(y = ..density..),binwidth = 5) + geom_density(colour="red", size=1, adjust=1)

ggplot(LonWardProfiles, aes(x=`Unauthorised Absence in All Schools (%) - 2013`)) + geom_histogram(aes(y = ..density..),binwidth = 0.1) + geom_density(colour="red", size=1, adjust=1)

ggplot(LonWardProfiles, aes(x=`Median House Price (£) - 2014`)) + geom_histogram()

qplot(x = `Median House Price (£) - 2014`, y = `Average GCSE capped point scores - 2014`, data=LonWardProfiles)

ggplot(LonWardProfiles, aes(x=log(`Median House Price (£) - 2014`))) + geom_histogram()

symbox(~`Median House Price (£) - 2014`, LonWardProfiles, na.rm=T, powers=seq(-3,3,by=.5))

ggplot(LonWardProfiles, aes(x=(`Median House Price (£) - 2014`)^-1)) + geom_histogram()

qplot(x = (`Median House Price (£) - 2014`)^-1, y = `Average GCSE capped point scores - 2014`, data=LonWardProfiles)

qplot(x = log(`Median House Price (£) - 2014`), y = `Average GCSE capped point scores - 2014`, data=LonWardProfiles)

#save the residuals into your dataframe
LonWardProfiles$model1_resids <- model1$residuals

qplot(model1$residuals) + geom_histogram() 

model2 <- lm(`Average GCSE capped point scores - 2014` ~ `Unauthorised Absence in All Schools (%) - 2013` + log(`Median House Price (£) - 2014`), data = LonWardProfiles)

#show the summary of those outputs
summary(model2)

LonWardProfiles$model2_resids <- residuals(model2)

install.packages("corrplot")

library(corrplot)

#first drop the geometry column from the dataframe as it will cause problems
tempdf <- st_set_geometry(LonWardProfiles,NULL)

colnames(tempdf)[37] <-  "House Price"

#pull out the columns we want to check for multicolinearity
tempdf <- tempdf[,c("House Price", "Unauthorised Absence in All Schools (%) - 2013")]

#log houseprice
tempdf[1] <- log(tempdf[1])
#rename the columns to something shorter
names(tempdf) <- c("Med House Price", "Un Auth Absence")

#compute the correlation matrix for the two variables of interest
cormat <- cor(tempdf[,1:2], use="complete.obs", method="pearson")

#visualise the correlation matrix
corrplot(cormat)

vif(model2)

tempdf <- st_set_geometry(LonWardProfiles,NULL)

cormat <- cor(tempdf[,10:74], use="complete.obs", method="pearson")

str(tempdf)

#plot the matrix, but reduce the label sizes
corrplot(cormat, type = "lower", tl.cex = 0.5)

plot(model2)

durbinWatsonTest(model2)

#now plot the residuals
tmap_mode("view")

#qtm(LonWardProfiles, fill = "model1_resids")

tm_shape(LonWardProfiles) +
  tm_polygons("model2_resids",
              palette = "RdYlBu") +
  tm_shape(lond_sec_schools_sf) + tm_dots(col = "TYPE")

#####

#Firstly convert our SF object into an SP object:
LonWardProfilesSP <- as(LonWardProfiles,"Spatial")
names(LonWardProfilesSP)

#and calculate the centroids of all Wards in London
coordsW <- coordinates(LonWardProfilesSP)
plot(coordsW)

#Now we need to generate a spatial weights matrix (remember from the lecture a couple of weeks ago). We'll start with a simple binary matrix of queen's case neighbours

#create a neighbours list of queens contiguity
LWard_nb <- poly2nb(LonWardProfilesSP, queen=T)

#or nearest neighbours
knn_wards <- knearneigh(coordsW, k=4)

LWard_knn <- knn2nb(knn_wards)

#plot them
plot(LWard_nb, coordinates(coordsW), col="red")

plot(LWard_knn, coordinates(coordsW), col="blue")

#add a map underneath
plot(LonWardProfilesSP)

#create a spatial weights matrix object from these weights
Lward.queens_weight <- nb2listw(LWard_nb, style="C")
Lward.knn_4_weight <- nb2listw(LWard_knn, style="C")

#now run a moran's I test on the residuals
#first using queens neighbours
moran.test(LonWardProfilesSP@data$model2_resids, Lward.queens_weight)

#then knn = 4
moran.test(LonWardProfilesSP@data$model2_resids, Lward.knn_4_weight)

#Original Model
model2 <- lm(`Average GCSE capped point scores - 2014` ~ `Unauthorised Absence in All Schools (%) - 2013` + log(`Median House Price (£) - 2014`), data = LonWardProfiles)

summary(model2)


install.packages("spatialreg")
library(spatialreg)

#run a spatially-lagged regression model with a queen's case weights matrix
slag_dv_model2_queen <- lagsarlm(`Average GCSE capped point scores - 2014` ~ `Unauthorised Absence in All Schools (%) - 2013` + log(`Median House Price (£) - 2014`), data = LonWardProfiles, nb2listw(LWard_nb, style="C"), method = "eigen")

#what do the outputs show?
summary(slag_dv_model2_queen)

#run a spatially-lagged regression model
slag_dv_model2_knn4 <- lagsarlm(`Average GCSE capped point scores - 2014` ~ `Unauthorised Absence in All Schools (%) - 2013` + log(`Median House Price (£) - 2014`), data = LonWardProfiles, nb2listw(LWard_knn, style="C"), method = "eigen")

#what do the outputs show?
summary(slag_dv_model2_knn4)

#write out the residuals
LonWardProfilesSP@data$slag_dv_model2_knn_resids <- slag_dv_model2_knn4$residuals

#now test for spatial autocorrelation
moran.test(LonWardProfilesSP@data$slag_dv_model2_knn_resids, Lward.knn_4_weight)

sem_model1 <- errorsarlm(`Average GCSE capped point scores - 2014` ~ `Unauthorised Absence in All Schools (%) - 2013` + log(`Median House Price (£) - 2014`), data = LonWardProfiles, nb2listw(LWard_knn, style="C"), method = "eigen")

summary(sem_model1)

extradata <- read_csv("https://www.dropbox.com/s/qay9q1jwpffxcqj/LondonAdditionalDataFixed.csv?raw=1")

#add the extra data too
LonWardProfiles <- left_join(LonWardProfiles, extradata, by = c("GSS_CODE" = "Wardcode"))

names(LonWardProfiles)

p <- ggplot(LonWardProfiles, aes(x=`UnauthAbsenceSchools11`, y=`AvgGCSE2011`))
p + geom_point(aes(colour = InnerOuter)) 

#first, let's make sure R is reading our InnerOuter variable as a factor
LonWardProfiles$InnerOuter <- as.factor(LonWardProfiles$InnerOuter)

#now run the model
model3 <- lm(`Average GCSE capped point scores - 2014` ~ `Unauthorised Absence in All Schools (%) - 2013` + log(`Median House Price (£) - 2014`) + `InnerOuter`, data = LonWardProfiles)

summary(model3)

contrasts(LonWardProfiles$InnerOuter)

LonWardProfiles$InnerOuter <- relevel(LonWardProfiles$InnerOuter, ref="Outer")

model3 <- lm(`Average GCSE capped point scores - 2014` ~ `Unauthorised Absence in All Schools (%) - 2013` + log(`Median House Price (£) - 2014`) + `InnerOuter`, data = LonWardProfiles)

summary(model3)

#select some variables from the data file
myvars <- c("Average GCSE capped point scores - 2014","Unauthorised Absence in All Schools (%) - 2013","Median House Price (£) - 2014","Rate of JobSeekers Allowance (JSA) Claimants - 2015", "% with Level 4 qualifications and above - 2011")

#check their correlations are OK
cormat <- cor(tempdf[myvars], use="complete.obs", method="pearson")

#run a final OLS model
model_final <- lm(`Average GCSE capped point scores - 2014` ~ `Unauthorised Absence in All Schools (%) - 2013` + log(`Median House Price (£) - 2014`) + `InnerOuter` + `Rate of JobSeekers Allowance (JSA) Claimants - 2015` + `% with Level 4 qualifications and above - 2011`, data = LonWardProfiles)

summary(model_final)

#save residuals
LonWardProfiles$model_final_res <- model_final$residuals

plot(model_final)

qtm(LonWardProfiles, fill = "model_final_res")

LonWardProfilesSP <- as(LonWardProfiles,"Spatial")
moran.test(LonWardProfilesSP@data$model_final_res, Lward.knn_4_weight)

install.packages("spgwr")

library(spgwr)

#calculate kernel bandwidth
GWRbandwidth <- gwr.sel(`Average GCSE capped point scores - 2014` ~ `Unauthorised Absence in All Schools (%) - 2013` + log(`Median House Price (£) - 2014`) + `InnerOuter` + `Rate of JobSeekers Allowance (JSA) Claimants - 2015` + `% with Level 4 qualifications and above - 2011`, data = LonWardProfiles, coords=coordsW,adapt=T)

#run the gwr model
gwr.model = gwr(`Average GCSE capped point scores - 2014` ~ `Unauthorised Absence in All Schools (%) - 2013` + log(`Median House Price (£) - 2014`) + `InnerOuter` + `Rate of JobSeekers Allowance (JSA) Claimants - 2015` + `% with Level 4 qualifications and above - 2011`, data = LonWardProfiles, coords=coordsW, adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE)

#print the results of the model
gwr.model

results<-as.data.frame(gwr.model$SDF)
names(results)

tm_shape(LonWardProfilesSP) + tm_polygons(col = "coefUnauthAbs", palette = "RdBu", alpha = 0.5)

tm_shape(LonWardProfilesSP) +
  tm_polygons(col = "coefHousePrice", palette = "RdBu")

tm_shape(LonWardProfilesSP) +
  tm_polygons(col = "coefJSA", palette = "PuOr")

tm_shape(LonWardProfilesSP) +
  tm_polygons(col = "coefLev4Qual", palette = "PRGn")

#**NOTE** when you run the code below, it's likely that column headers with `column_names` labelled with lots of quotations - as happens when using read_csv, errors will be caused when trying to reference these in gwr.model. To get over these errors, rename your columns as something simple and run the regression again. 

#run the significance test
sigTest = abs(gwr.model$SDF$"log(`Median House Price (£) - 2014`)") -2 * gwr.model$SDF$"log(`Median House Price (£) - 2014`)_se"

#store significance results
LonWardProfilesSP$GWRUnauthSig<-sigTest

tm_shape(LonWardProfilesSP) +
  tm_polygons(col = "GWRUnauthSig", palette = "RdYlBu")






