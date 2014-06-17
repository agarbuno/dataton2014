library(gstat)
library(MASS)
library(RColorBrewer)
library(classInt)
library(ggplot2)
library(plyr)
library(maptools)
library(maps)
library(SpatialEpi)
library(fields)
library(rgdal)
library(proto)
library(sp)

munmap <- readShapePoly("/Users/alfredogarbuno/dataton/inegi/Municipios/jal_municipal.shp",
                      verbose = TRUE, proj4string = CRS("+proj=longlat"))

map <- readShapeLines("/Users/alfredogarbuno/dataton/inegi/Vialidades/jal_eje_vial.shp",
                     verbose = TRUE, proj4string = CRS("+proj=longlat"))

View(submap@data)

submap <- subset(map, substr(CVEGEO,1,5) %in% c(14120))

shape.fort <- fortify(submap) 
shape.fort <- shape.fort[order(shape.fort$order), ] 

ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), colour='black', fill='white') +
  labs(title = "Zapopan", x = "", y = "") +
  geom_point(data = social, aes(x = lon, y = lat, color = social), size = .5)

