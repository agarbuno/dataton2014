# Lectura de mapas municipales y de agebs

munmap <- readShapePoly("/Users/alfredogarbuno/dataton/inegi/Municipios/jal_municipal.shp",
                        verbose = TRUE, proj4string = CRS("+proj=longlat"))
submunmap <- subset(munmap, substr(CVEGEO,1,5) %in% c(14120, 14098, 14039, 14097))
mun.fort <- fortify(submunmap)

theme <- theme(panel.grid.minor = element_blank(), axis.ticks = element_blank(), 
               axis.title.x = element_blank(), axis.title.y = element_blank(), 
               axis.text.x = element_blank(), axis.text.y = element_blank())

agebmap <- readShapePoly("~/dataton/inegi/Ageb/jal_ageb_urb.shp",
                         verbose = TRUE, proj4string = CRS("+proj=longlat"))
subagebmap <- subset(agebmap, substr(CVEGEO,1,5) %in% c(14120, 14098, 14039, 14097))
shape.ageb <- fortify(subagebmap) 
shape.ageb <- shape.ageb[order(shape.ageb$order), ] 

##### Lectura y construccion de puntos de interés #####

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

#quartz()
ggplot(data = shape.ageb, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "Zapopan", x = "", y = "") + coord_equal() + xlim(c(min(shape.ageb$long),max(shape.ageb$long))) +
  ylim(c(min(shape.ageb$lat),max(shape.ageb$lat)))  + 
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  geom_polygon(data = shape.serv, aes(x = long, y = lat, group = group, colour = GEOGRAFICO, fill = GEOGRAFICO)) +
  geom_point(data = serv.points, aes(x = long, y = lat, colour = GEOGRAFICO), size = 5, alpha = .3)

##### Puntos de servicios #####
servpoints <- readShapePoints("/Users/alfredogarbuno/dataton/inegi/Servicios/jal_servicios_p.shp",
                              verbose = TRUE, proj4string = CRS("+proj=longlat"))
subservpoints <- subset(servpoints, substr(CVEGEO,1,5) %in% c(14120, 14098, 14039, 14097))
table(subservpoints@data$GEOGRAFICO )
subservpoints@data$GEOGRAFICO <- factor(subservpoints@data$GEOGRAFICO)
subservpoints@data$GEOGRAFICO <- factor(subservpoints@data$GEOGRAFICO, labels = c('Cementerio', 'Centro médico', 'Escuela', 'Deportivo o Recreativo'
                                        , 'Mercado', 'Palacio Gobierno', 'Plaza', 'Tanque de Agua', 'Templo'))
subserv.points <- subset(subservpoints, GEOGRAFICO %in% c('Deportivo o Recreativo', 'Mercado', 'Palacio de Gobierno', 'Plaza', 'Templo'))
sub.points <- data.table(subserv.points@coords)
setnames(sub.points, c('long', 'lat'))
sub.points$GEOGRAFICO  <- subserv.points@data$GEOGRAFICO

ggplot(data = shape.ageb, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "Zapopan", x = "", y = "") + coord_equal() + xlim(c(min(shape.ageb$long),max(shape.ageb$long))) +
  ylim(c(min(shape.ageb$lat),max(shape.ageb$lat)))  + 
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  geom_polygon(data = shape.serv, aes(x = long, y = lat, group = group, colour = GEOGRAFICO, fill = GEOGRAFICO)) +
  geom_point(data = sub.points, aes(x = long, y = lat, colour = GEOGRAFICO), size = 5, alpha = .3)

##### Lectura y construccion de cuerpos de agua y ríos ####

##### Cuerpos de agua #####
aguamap <- readShapePoly("/Users/alfredogarbuno/dataton/inegi/Topografia/hidrografia/cuerpo de agua/cuerpo_de_agua_perenne.shp",
                         verbose = TRUE, proj4string = CRS("+proj=longlat"))
subagua <- subset(aguamap, ESTADO == 14)
shape.agua <- fortify(subagua) 
shape.agua <- shape.agua[order(shape.agua$order), ] 

##### Rios #####
riversmap <- readShapeLines("/Users/alfredogarbuno/dataton/inegi/Topografia/hidrografia/corriente de agua/perenne.shp",
                          verbose = TRUE, proj4string = CRS("+proj=longlat"))
subrivers <- subset(riversmap, ESTADO == 14)
subrivers <- conv_sp_lines_to_seg(subrivers)
rivers <- geom_segment2(data=subrivers, 
                        size=.75, 
                        aes(x = slon, y = slat, xend=elon, yend=elat), 
                        color="steelblue", 
                        alpha = .8)

