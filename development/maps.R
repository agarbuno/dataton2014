#install.packages(c('gstat', 'MASS', 'RColorBrewer', 'classInt', 'ggplot2', 'dplyr', 'maptools', 'maps', 'SpatialEpi', 'fields', 'rgdal', 'proto', 'sp', 'ggmaps'))

# Instalar en ubuntu la dependencia libgeos-dev, libgdal-dev
# sudo apt-get install libgdal1-dev libproj-dev 

#install.packages('gstat')
#install.packages('rgdal')
#install.packages('png')

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

conv_sp_lines_to_seg <- function(spLines) {
  library(plyr)
  extract <- data.frame(do.call(rbind, 
                                llply(unlist(coordinates(spLines), 
                                             recursive = F), unlist)))
  names(extract) <- c("slon", "slat")
  n <- length(extract$slon)
  tmplon <- extract$slon[n]
  tmplat <- extract$slat[n]
  extract <- extract[-n, ]
  extract$elon <- c(extract$slon[-1], tmplon)
  extract$elat <- c(extract$slat[-1], tmplat)
  
  length <- do.call(rbind, 
                    llply(unlist(coordinates(spLines), 
                                 recursive = F), nrow))
  length <- cumsum(length)
  length <- length[-length(length)]
  extract[-length, ] 
}

GeomSegment2 <- proto(ggplot2:::GeomSegment, {
  objname <- "geom_segment2"
  draw <- function(., data, scales, coordinates, arrow=NULL, ...) {
    if (is.linear(coordinates)) {
      return(with(coord_transform(coordinates, data, scales),
                  segmentsGrob(x, y, xend, yend, default.units = "native",
                               gp = gpar(col = alpha(colour, alpha), 
                                         lwd = size * .pt,
                                         lty = linetype, lineend = "round"),
                               arrow = arrow)))
    }
  }
})

geom_segment2 <- function(mapping = NULL, data = NULL, stat = "identity", 
                          position = "identity", arrow = NULL, ...) {
  GeomSegment2$new(mapping = mapping, data = data, stat = stat, 
                   position = position, arrow = arrow, ...)
}

# munmap <- readShapePoly("/Users/alfredogarbuno/dataton/inegi/Municipios/jal_municipal.shp",
#                       verbose = TRUE, proj4string = CRS("+proj=longlat"))
# 
# map <- readShapeLines("/Users/alfredogarbuno/dataton/inegi/Vialidades/jal_eje_vial.shp",
#                      verbose = TRUE, proj4string = CRS("+proj=longlat"))
# 
# submap <- subset(map, substr(CVEGEO,1,5) %in% c(14120, 14098, 14039))
# 
# map.2 <- conv_sp_lines_to_seg(submap)
# 
# streets <- geom_segment2(data=map.2, 
#                          size=.25, 
#                          aes(xend=elon, yend=elat), 
#                          color="black")
# 
# #shape.fort <- fortify(submap) 
# #shape.fort <- shape.fort[order(shape.fort$order), ] 
# 
# ggplot(data = shape.fort, aes(x = long, y = lat)) + 
#   geom_polygon(aes(group = group), colour='black', fill='white') +
#   labs(title = "Zapopan", x = "", y = "") +
#   geom_point(data = social, aes(x = lon, y = lat, color = social), size = .5)
# 
# image <- ggplot(map.2, aes(x=slon,y=slat)) + 
#   streets +
#   labs(title = "Zapopan", x = "", y = "") + coord_fixed() + 
#   geom_point(data = social, aes(x = lon, y = lat, color = social), size = .5)
# 
# image
