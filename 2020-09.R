setwd("C:\\Users\\ASUS\\Desktop\\Fiverr\\john\\New folder (3)")

library("rgdal")
library(raster)
library(adehabitatHR)
input<- read.csv("2020-09-north-wales-street.csv")
input<- input[,1:10] #We only need the first 10 columns
input<- input[complete.cases(input),] #This line of code removes rows with NA values in the data.
Crime.Spatial<- SpatialPointsDataFrame(input[,5:6], input, proj4string = CRS("+init=epsg:4326"))
Crime.Spatial<- spTransform(Crime.Spatial, CRS("+init=epsg:27700")) #We now project from WGS84 for to British National Grid
plot(Crime.Spatial) #Plot the data
Northwales<- readOGR(".", layer="North_Wales")
Extent<- extent(Northwales) #this is the geographic extent of the grid. It is based on the Northwales object.
#Here we specify the size of each grid cell in metres (since those are the units our data are projected in).
resolution<- 500
#This is some magic that creates the empty grid
x <- seq(Extent[1],Extent[2],by=resolution)  # where resolution is the pixel size you desire
y <- seq(Extent[3],Extent[4],by=resolution)
xy <- expand.grid(x=x,y=y)
coordinates(xy) <- ~x+y
gridded(xy) <- TRUE
#You can see the grid here (this may appear solid black if the cells are small)
plot(xy)
plot(Northwales, border="black", add=T)
all <- raster(kernelUD(Crime.Spatial, h="href", grid = xy)) #Note we are running two functions here - first KernelUD then converting the result to a raster object.
#First results

writeRaster(all,filename="file1.asc",datatype='FLT4S',format="ascii",overwrite=TRUE)
plot(all)
plot(Northwales, border="black", add=T)
plot(Crime.Spatial[Crime.Spatial$Crime.type=="Burglary",]) # quick plot of burglary points
Burglary<- raster(kernelUD(Crime.Spatial[Crime.Spatial$Crime.type=="Burglary",], h="href", grid = xy))

writeRaster(Burglary,filename="file2.asc",datatype='FLT4S',format="ascii",overwrite=TRUE)

plot(Burglary)
plot(Northwales, border="black", add=T)
both<-Burglary/all

writeRaster(both,filename="file3.asc",datatype='FLT4S',format="ascii",overwrite=TRUE)

plot(both)
plot(Northwales, border="black", add=T)
both2 <- both
both2[both <= 0] <- NA
both2[both >= 1] <- NA

writeRaster(both2,filename="file4.asc",datatype='FLT4S',format="ascii",overwrite=TRUE)

#Now we can see the hotspots much more clearly.
plot(both2)
plot(Northwales, add=T)


#writeRaster(both2,filename=pointpatternhotspot.asc,datatype='FLT4S',format="ascii",overwrite=TRUE)