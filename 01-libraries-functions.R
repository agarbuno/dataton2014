#####  Manejo de datos y rendimiento ####

library(data.table)
library(lubridate)
library(plyr)
library(reshape2)

library(doParallel)
registerDoParallel(cores=detectCores())

##### Herramientas de geoestadistica ####

library(gstat)
library(MASS)
library(RColorBrewer)
library(classInt)
library(ggplot2)
library(maptools)
library(maps)
library(SpatialEpi)
library(fields)
library(rgdal)
library(proto)
library(sp)
library(ggmap)

###### Análisis de redes #####

library(igraph)

###### Funciones #####

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

deg_to_dms<- function (degfloat){
  deg <- as.integer(degfloat)
  minfloat <- abs(60*(degfloat - deg))
  min <- as.integer(minfloat)
  secfloat <- abs(60*(minfloat - min))
  ### Round seconds to desired accuracy:
  secfloat <- round(secfloat, digits=3 )
  ### After rounding, the seconds might become 60
  ### The following if-tests are not necessary if no 
  ### rounding is done.
  if (secfloat == 60) {
    min <- min + 1
    secfloat <- 0
  }
  if (min == 60){
    deg <- deg + 1
    min <- 0
  }
  dm<-paste(deg,min,sep="º ")
  dms<-paste(dm,secfloat,sep="' ")
  return (dms)
}