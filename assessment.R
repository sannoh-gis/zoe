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
download.file("https://data.london.gov.uk/download/statistical-gis-boundary-files-london/9ba8c833-6370-4b11-abdc-314aa020d5e0/statistical-gis-boundaries-london.zip", destfile="report_data/statistical-gis-boundaries-london.zip")

#unzip it
unzip("report_data/statistical-gis-boundaries-london.zip", exdir="report_data")

#look at the subfolders in the directory to work out where we want to get our shapefile from
list.dirs("report_data/statistical-gis-boundaries-london")

#read in the boundaries from the file you have just unzipped into your working directory
LondonBoroughsss <- readOGR("report_data/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp", layer="London_Borough_Excluding_MHW")

#convert it to a simple features object
LondonBoroughsssSF <- st_as_sf(LondonBoroughsss)

#check coordinate reference system
LondonBoroughsssSF

#Proj4 string tells me it's in wgs84, so Convert to British National Grid
BNG = "+init=epsg:27700"
LondonBoroughsssSFBNG <- st_transform(LondonBoroughsssSF, BNG)

#check the data
qtm(LondonBoroughsssSFBNG)

LondonBoroughProfiles<- read.csv("report_data/LondonBoroughProfiles1.csv")

str(LondonBoroughProfiles)

#merge boundaries and data
LonBorouProfiles <- left_join(LondonBoroughsssSFBNG, LondonBoroughProfiles, by = c("GSS_CODE" = "Code"))

tmap_mode("view")

qtm(LonBorouProfiles, fill = "Percentage.who.were.bullied.in.the.past.couple.of.months", borders = NULL)



#regression work

q <- qplot(x = `Percentage.who.have.been.drunk.in.the.last.4.weeks`, y = `Percentage.who.were.bullied.in.the.past.couple.of.months`, data=LonBorouProfiles)

#plot with a regression line - note, I've added some jitter here as the x-scale is rounded
q + stat_smooth(method="lm", se=FALSE, size=1) + geom_jitter()

#multiple regression model

model <- lm(`Percentage.who.were.bullied.in.the.past.couple.of.months` ~ `Percentage.who.have.ever.had.an.alcoholic.drink` + `Non.British` + `Percentage.of.those.with.3.or.more.risky.behaviours`, data = LonBorouProfiles)

#show the summary of those outputs
summary(model)

model_res <- tidy(model)


#Assumptions Underpinning Linear Regression
#Assumption 1 - There is a linear relationship between the dependent and Independent variables

#let's check the distribution of these variables first
ggplot(LonBorouProfiles, aes(x=`Percentage.who.were.bullied.in.the.past.couple.of.months`)) + geom_histogram(aes(y = ..density..),binwidth = 2) + geom_density(colour="red", size=1, adjust=1)

ggplot(LonBorouProfiles, aes(x=`Percentage.who.have.ever.had.an.alcoholic.drink`)) + geom_histogram(aes(y = ..density..),binwidth = 3) + geom_density(colour="red", size=1, adjust=1)

ggplot(LonBorouProfiles, aes(x=`Non.British`)) + geom_histogram(aes(y = ..density..),binwidth = 5) + geom_density(colour="red", size=1, adjust=1)

ggplot(LonBorouProfiles, aes(x=`Percentage.of.those.with.3.or.more.risky.behaviours`)) + geom_histogram(aes(y = ..density..),binwidth = 3) + geom_density(colour="red", size=1, adjust=1)

#Assumption 2 - The residuals in your model should be normally distributed

#save the residuals into your dataframe
LonBorouProfiles$model_resids <- model$residuals

qplot(model$residuals) + geom_histogram() 
#Examining the histogram above, we can be happy that our residuals look to be relatively normally distributed.

#Assumption 3 - No Multicolinearity in the independent variables

install.packages("corrplot")

library(corrplot)

#first drop the geometry column from the dataframe as it will cause problems
tempdf <- st_set_geometry(LonBorouProfiles,NULL)
#pull out the columns we want to check for multicolinearity
tempdf <- tempdf[,c("Percentage.who.have.ever.had.an.alcoholic.drink" , "Non.British" , "Percentage.of.those.with.3.or.more.risky.behaviours")]

#rename the columns to something shorter
names(tempdf) <- c("alcoholic.drink", "Non.British","risky.behaviours")

#compute the correlation matrix for the two variables of interest
cormat <- cor(tempdf[,1:3], use="complete.obs", method="pearson")

#visualise the correlation matrix
corrplot(cormat)

#If we have VIF values for any variable exceeding 10, then we may need to worry and perhaps remove that variable from the analysis.
vif(model)

#Assumption 4 - Homoscedasticity

plot(model)

#Assumption 5 - Independence of Errors

#Firstly convert our SF object into an SP object:
LonBorouProfilesSP <- as(LonBorouProfiles,"Spatial")
names(LonBorouProfilesSP)

#and calculate the centroids of all boroughs in London
coordsB <- coordinates(LonBorouProfilesSP)
plot(coordsB)

#Now we need to generate a spatial weights matrix. We'll start with a simple binary matrix of queen's case neighbours

#create a neighbours list of queens contiguity
LBorough_nb <- poly2nb(LonBorouProfilesSP, queen=T)

#or nearest neighbours
knn_boroughs <- knearneigh(coordsB, k=4)

LBorough_knn <- knn2nb(knn_boroughs)

#plot them
plot(LBorough_nb, coordinates(coordsB), col="red")

plot(LBorough_knn, coordinates(coordsB), col="blue")

#add a map underneath
plot(LonBorouProfilesSP)

#create a spatial weights matrix object from these weights
LBorough.queens_weight <- nb2listw(LBorough_nb, style="C")
LBorough.knn_4_weight <- nb2listw(LBorough_knn, style="C")

#now run a moran's I test on the residuals
#first using queens neighbours

moran.test(LonBorouProfilesSP@data$model_resids, LBorough.queens_weight)

#then knn = 4
moran.test(LonBorouProfilesSP@data$model_resids, LBorough.knn_4_weight)

#Spatial Regression Models
#Dealing with Spatially Autocorrelated Residuals - Spatial Lag and Spatial Error models

library(spatialreg)

#run a spatially-lagged regression model with a queen's case weights matrix
slag_dv_model_queen <- lagsarlm(`Percentage.who.were.bullied.in.the.past.couple.of.months` ~ `Percentage.who.have.ever.had.an.alcoholic.drink` + `Non.British` + `Percentage.of.those.with.3.or.more.risky.behaviours`, data = LonBorouProfiles, nb2listw(LBorough_nb, style="C"), method = "eigen")

#what do the outputs show?
summary(slag_dv_model_queen)

#run a spatially-lagged regression model
slag_dv_model_knn4 <- lagsarlm(`Percentage.who.were.bullied.in.the.past.couple.of.months` ~ `Percentage.who.have.ever.had.an.alcoholic.drink` + `Non.British` + `Percentage.of.those.with.3.or.more.risky.behaviours`, data = LonBorouProfiles, nb2listw(LBorough_knn, style="C"), method = "eigen")

#what do the outputs show?
summary(slag_dv_model_knn4)

#write out the residuals
LonBorouProfilesSP@data$slag_dv_model_knn_resids <- slag_dv_model_knn4$residuals

#now test for spatial autocorrelation
moran.test(LonBorouProfilesSP@data$slag_dv_model_knn_resids, LBorough.knn_4_weight)



qtm(LonBorouProfiles, fill = "model_resids")

LonWardProfilesSP <- as(LonWardProfiles,"Spatial")
moran.test(LonWardProfilesSP@data$model_final_res, Lward.knn_4_weight)



library(spgwr)

#calculate kernel bandwidth
GWRbandwidth <- gwr.sel(`Percentage.who.were.bullied.in.the.past.couple.of.months` ~ `Percentage.who.have.ever.had.an.alcoholic.drink` + `Non.British` + `Percentage.of.those.with.3.or.more.risky.behaviours`, data = LonBorouProfiles, coords=coordsB,adapt=T)

#run the gwr model
gwr.model = gwr(`Percentage.who.were.bullied.in.the.past.couple.of.months` ~ `Percentage.who.have.ever.had.an.alcoholic.drink` + `Non.British` + `Percentage.of.those.with.3.or.more.risky.behaviours`, data = LonBorouProfiles, coords=coordsB, adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE)

#print the results of the model
gwr.model

results<-as.data.frame(gwr.model$SDF)
names(results)

#attach coefficients to original dataframe
LonBorouProfilesSP@data$coefAlcoholicDrink<-results$Percentage.who.have.ever.had.an.alcoholic.drink
LonBorouProfilesSP@data$coefNonBritish<-results$Non.British
LonBorouProfilesSP@data$coef3RiskyBehaviours<-results$Percentage.of.those.with.3.or.more.risky.behaviours


tm_shape(LonBorouProfilesSP) +
  tm_polygons(col = "coefAlcoholicDrink", palette = "RdBu", alpha = 0.5)

tm_shape(LonBorouProfilesSP) +
  tm_polygons(col = "coefNonBritish", palette = "RdBu")

tm_shape(LonBorouProfilesSP) +
  tm_polygons(col = "coef3RiskyBehaviours", palette = "PuOr")

#they are all positive or negative








