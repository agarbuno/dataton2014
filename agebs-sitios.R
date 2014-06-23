# Agebs, rutas, tren, y puntos importantes.

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
                            size=.75, 
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
subserv.points <- subset(subservpoints, GEOGRAFICO %in% c('Deportivo o Recreativo', 'Mercado', 'Palacio de Gobierno', 'Plaza', 'Templo'))
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
colSums(data.frame(ex)[, 2:6])
##### Mapa con los puntos ####

ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "Zapopan", x = "", y = "") + coord_equal() + theme + 
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  geom_point(data = interest, aes(x = long, y = lat, colour = GEOGRAFICO), size = 3, alpha = .3)
