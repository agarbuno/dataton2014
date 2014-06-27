# Agebs, rutas, tren, y puntos importantes.
#load('~/dataton/session1.Rdata')

##### Lectura de agebs #####
# Los heredas de in.polygons.2.R
theme <- theme(panel.grid.minor = element_blank(), axis.ticks = element_blank(), 
               axis.title.x = element_blank(), axis.title.y = element_blank(), 
               axis.text.x = element_blank(), axis.text.y = element_blank()
)

##### Lineas de tren metropolitano #####
serviciosmap <- readShapeLines("/Users/alfredogarbuno/dataton/inegi/Servicios/jal_servicios_l.shp",
                               verbose = TRUE, proj4string = CRS("+proj=longlat"))
subtrenmap <- subset(serviciosmap, substr(CVEGEO,1,5) %in% c(14120, 14098, 14039, 14097) &
                       as.numeric(GEOGRAFICO) == 6)
subtrenlines <- conv_sp_lines_to_seg(subtrenmap)
metrolines <- geom_segment2(data=subtrenlines, 
                            size=2.5, 
                            aes(x = slon, y = slat, xend=elon, yend=elat), 
                            color='gold', 
                            alpha = .3)

##### Poligonos y centroides de Areas verdes, plazas y templos ####
servpolmap <- readShapePoly("/Users/alfredogarbuno/dataton/inegi/Servicios/jal_servicios_a.shp",
                            verbose = TRUE, proj4string = CRS("+proj=longlat"))
subservpol <- subset(servpolmap, substr(CVEGEO,1,5) %in% c(14120, 14098, 14039, 14097))

aux.dt <- data.table(subservpol@data)
aux.dt <- aux.dt[, list(GEOGRAFICO, OID, TIPO)]
aux.dt$OID <- aux.dt$OID - 1
setnames(aux.dt, 'OID', 'id')
shape.serv <- fortify(subservpol) 
shape.serv <- shape.serv[order(shape.serv$order), ] 
shape.serv <- plyr:::join(shape.serv, aux.dt, by = 'id')
rm(aux.dt)

shape.serv$GEOGRAFICO <- factor(shape.serv$GEOGRAFICO, labels = c('Area Verde', 'Camellón', 'Cementerio', 
                                                                  'Cuerpo de Agua', 'Escuela', 'Instalacion Deportiva', 
                                                                  'Mercado', 'Plaza', 'Tanque de Agua', 'Templo'))
shape.serv <- subset(shape.serv, GEOGRAFICO %in% c('Area Verde', 'Cuerpo de agua', 'Mercado', 'Plaza', 'Templo'))
shape.serv <- data.table(shape.serv)
serv.points <- shape.serv[, data.table(kmeans(cbind(long,lat),centers=1)$centers) , by=list(id, GEOGRAFICO)]
serv.points$GEOGRAFICO <- factor(serv.points$GEOGRAFICO)
rm (shape.serv, servpolmap, subservpol)
serv.points <- serv.points[, list(long, lat, GEOGRAFICO)]
serv.points

##### Puntos de servicios #####
servpoints <- readShapePoints("~/dataton/inegi/Servicios/jal_servicios_p.shp",
                              verbose = TRUE, proj4string = CRS("+proj=longlat"))
subservpoints <- subset(servpoints, substr(CVEGEO,1,5) %in% c(14120, 14098, 14039, 14097))
subservpoints@data$GEOGRAFICO <- factor(subservpoints@data$GEOGRAFICO)
subservpoints@data$GEOGRAFICO <- factor(subservpoints@data$GEOGRAFICO, labels = c('Cementerio', 'Centro médico', 'Escuela', 'Deportivo o Recreativo'
                                                                                  , 'Mercado', 'Palacio Gobierno', 'Plaza', 'Tanque de Agua', 'Templo'))
subserv.points <- subset(subservpoints, GEOGRAFICO %in% c('Deportivo o Recreativo', 'Mercado', 'Palacio Gobierno', 'Plaza', 'Templo', 'Centro médico'))
sub.points <- data.table(subserv.points@coords)
setnames(sub.points, c('long', 'lat'))
sub.points$GEOGRAFICO  <- subserv.points@data$GEOGRAFICO
sub.points

interest <- rbind(serv.points, sub.points)
points <- SpatialPoints(data.frame(interest[,long],interest[,lat]),  proj4string = CRS("+proj=longlat"))
int.ageb <- over(points, subagebmap)
interest$ageb <- int.ageb$CVEGEO
interest
rm(int.ageb)

ex <- data.table(dcast(interest, ageb~GEOGRAFICO))
colSums(data.frame(ex)[, 2:8])

##### Lectura de parques #####
parques <- read.csv('~/dataton/zapopan/Parques.csv', header = FALSE)
parques <- parques[2:nrow(parques), c(8,7)]
parques$V8 <- as.numeric(as.character(parques$V8))
parques$V7 <- as.numeric(as.character(parques$V7))
parques <- parques[!row.names(parques) %in% c(9, 30),]
points.parques <- SpatialPoints(parques, proj4string = CRS("+proj=longlat"))
parques.ageb <- over(points.parques, subagebmap)
parques$ageb <- parques.ageb[,1]
parques$GEOGRAFICO <- 'Parque'
parques <- data.table(parques)
setcolorder(parques, c('V8', 'V7', 'GEOGRAFICO', 'ageb'))
setnames(parques, c('V8', 'V7'), c('long', 'lat'))
parques <- parques[!is.na(parques$ageb),]

interest <- rbind(interest, parques)

##### Mapa con los puntos ####
theme <- theme(panel.grid.minor = element_blank(), axis.ticks = element_blank(), 
               axis.title.x = element_blank(), axis.title.y = element_blank(), 
               axis.text.x = element_blank(), axis.text.y = element_blank()
)

ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "", x = "", y = "") + coord_equal() + theme + 
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  geom_point(data = interest, aes(x = long, y = lat, colour = GEOGRAFICO), size = 5, alpha = .3)

##### Mapa con líneas de metro #####

ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "", x = "", y = "") + theme + coord_equal() + 
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  geom_segment(
    aes(x = startLon, y = startLat, xend = endLon, yend = endLat, group = id, color = factor(id)),
    alpha = .1, data = routes) +
  metrolines



#save.image('~/dataton/session2.Rdata')
