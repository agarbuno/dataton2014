library(data.table)
library(ggmap)
library(igraph)
library(reshape2)

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

aguamap <- readShapePoly("/Users/alfredogarbuno/dataton/inegi/Topografia/hidrografia/cuerpo de agua/cuerpo_de_agua_perenne.shp",
                         verbose = TRUE, proj4string = CRS("+proj=longlat"))
subagua <- subset(aguamap, ESTADO == 14)

shape.agua <- fortify(subagua) 
shape.agua <- shape.agua[order(shape.agua$order), ] 

ggplot(data = shape.ageb, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "Zapopan", x = "", y = "") + coord_equal() + xlim(c(min(shape.ageb$long),max(shape.ageb$long))) +
  ylim(c(min(shape.ageb$lat),max(shape.ageb$lat))) +
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  geom_polygon(data = shape.agua, aes(x = long, y = lat, group = group), colour='black', fill='steelblue')

aguamap <- readShapeLines("/Users/alfredogarbuno/dataton/inegi/Topografia/hidrografia/corriente de agua/perenne.shp",
                         verbose = TRUE, proj4string = CRS("+proj=longlat"))
subagua <- subset(aguamap, ESTADO == 14)
subagua2 <- conv_sp_lines_to_seg(subagua)

rivers <- geom_segment2(data=subagua2, 
                         size=.75, 
                         aes(x = slon, y = slat, xend=elon, yend=elat), 
                         color="steelblue", 
                        alpha = .8)

ggplot(data = shape.ageb, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "Zapopan", x = "", y = "") + coord_equal() + xlim(c(min(shape.ageb$long),max(shape.ageb$long))) +
  ylim(c(min(shape.ageb$lat),max(shape.ageb$lat))) +
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  rivers

serviciosmap <- readShapeLines("/Users/alfredogarbuno/dataton/inegi/Servicios/jal_servicios_l.shp",
                          verbose = TRUE, proj4string = CRS("+proj=longlat"))
subtrenmap <- subset(serviciosmap, substr(CVEGEO,1,5) %in% c(14120, 14098, 14039, 14097) &
                       as.numeric(GEOGRAFICO) == 6)

subserv <- subset(serviciosmap, substr(CVEGEO,1,5) %in% c(14120, 14098, 14039, 14097))
table(serviciosmap@data$GEOGRAFICO)
table(subserv@data$GEOGRAFICO)

subservlines <- conv_sp_lines_to_seg(subtrenmap)
servlines <- geom_segment2(data=subservlines, 
                        size=.75, 
                        aes(x = slon, y = slat, xend=elon, yend=elat), 
                        color='gold', 
                        alpha = .3)

ggplot(data = shape.ageb, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "Zapopan", x = "", y = "") + coord_equal() + xlim(c(min(shape.ageb$long),max(shape.ageb$long))) +
  ylim(c(min(shape.ageb$lat),max(shape.ageb$lat))) +
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  servlines

servpolmap <- readShapePoly("/Users/alfredogarbuno/dataton/inegi/Servicios/jal_servicios_a.shp",
                               verbose = TRUE, proj4string = CRS("+proj=longlat"))

View(servpolmap@data)
subservpol <- subset(servpolmap, substr(CVEGEO,1,5) %in% c(14120, 14098, 14039, 14097))
table(subservpol@data$GEOGRAFICO)
aux.dt <- data.table(subservpol@data)
aux.dt <- aux.dt[, list(GEOGRAFICO, OID, TIPO)]
aux.dt$OID <- aux.dt$OID - 1
setnames(aux.dt, 'OID', 'id')

shape.serv <- fortify(subservpol) 
shape.serv <- shape.serv[order(shape.serv$order), ] 
shape.serv <- plyr:::join(shape.serv, aux.dt, by = 'id')
rm(aux.dt)

shape.serv$GEOGRAFICO <- factor(shape.serv$GEOGRAFICO, labels = c('Area Verde', 'Camellón', 'Cementerio', 'Cuerpo de Agua', 'Escuela', 'Instalacion Deportiva', 'Mercado', 'Plaza', 'Tanque de Agua', 'Templo'))

ggplot(data = shape.ageb, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "Zapopan", x = "", y = "") + coord_equal() + xlim(c(min(shape.ageb$long),max(shape.ageb$long))) +
  ylim(c(min(shape.ageb$lat),max(shape.ageb$lat)))  + 
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  geom_polygon(data = shape.serv, aes(x = long, y = lat, group = group, colour = GEOGRAFICO, fill = GEOGRAFICO))


servpoints <- readShapePoints("/Users/alfredogarbuno/dataton/inegi/Servicios/jal_servicios_p.shp",
                            verbose = TRUE, proj4string = CRS("+proj=longlat"))
head(servpoints@data)
subservpoints <- subset(servpoints, substr(CVEGEO,1,5) %in% c(14120, 14098, 14039, 14097))
table(subservpoints@data$GEOGRAFICO)

# pruebas <- readShapePoints("/Users/alfredogarbuno/dataton/inegi/Topografia/cultura/sitios arqueologicos/rasgo_arqueologico-p.shp",
#                               verbose = TRUE, proj4string = CRS("+proj=longlat"))
# subpruebas <- subset(pruebas, ESTADO == 14)
# subpruebas.points <- data.frame(subpruebas@coords)
# 
# ggplot(data = shape.ageb, aes(x = long, y = lat)) + 
#   geom_polygon(aes(group = group), fill = 'black') +
#   labs(title = "Zapopan", x = "", y = "") + coord_equal() + 
#   geom_polygon(data = mun.fort, aes(x = long, y = lat, group = group), alpha = .2, fill = 'black') +
#   #xlim(c(min(shape.ageb$long),max(shape.ageb$long))) +
#   #ylim(c(min(shape.ageb$lat),max(shape.ageb$lat))) +
#   theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
#   geom_point(data = subpruebas.points, aes(x = coords.x1, y = coords.x2), size = 3, alpha = .7, color = 'white')

# Conclusiones: Sitios arqueológicos no hay. Cuerpos de agua y rios muy limitado. 