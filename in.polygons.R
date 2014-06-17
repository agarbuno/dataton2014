munmap <- readShapePoly("/Users/alfredogarbuno/dataton/inegi/Municipios/jal_municipal.shp",
                        verbose = TRUE, proj4string = CRS("+proj=longlat"))

colmap <- readShapePoly("/Users/alfredogarbuno/dataton/colonias/Colonias.shp",
                        verbose = TRUE, proj4string = CRS("+proj=longlat"))

locmap <- readShapePoly("/Users/alfredogarbuno/dataton/inegi/Localidad/jal_loc_urb.shp",
                        verbose = TRUE, proj4string = CRS("+proj=longlat"))

agebmap <- readShapePoly("/Users/alfredogarbuno/dataton/inegi/Ageb/jal_ageb_urb.shp",
                        verbose = TRUE, proj4string = CRS("+proj=longlat"))

manmap <- readShapePoly("/Users/alfredogarbuno/dataton/inegi/Manzana/jal_manzanas.shp",
                         verbose = TRUE, proj4string = CRS("+proj=longlat"))

subagebmap <- subset(agebmap, substr(CVEGEO,1,5) %in% c(14120))
subcolmap <- subset(colmap, MUN_NAME == 'ZAPOPAN')

View(colmap@data)


shape.fort <- fortify(subagebmap) 
shape.fort <- shape.fort[order(shape.fort$order), ] 

View(subagebmap@data)

quartz()

ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), colour='black', fill='white') +
  labs(title = "Zapopan", x = "", y = "") 
  
geom_point(data = social, aes(x = lon, y = lat, color = social), size = .5)


submap <- subset(munmap, substr(CVEGEO,1,5) %in% c(14120, 14098, 14039, 14097))
points <- SpatialPoints(data.frame(social[,lon],social[,lat]),  proj4string = CRS("+proj=longlat"))
in.poly <- over(points, submap)

colonias <- read.csv('/Users/alfredogarbuno/dataton/zapopan/Colonias.csv', sep = ',')
eventos <- read.csv('/Users/alfredogarbuno/dataton/zapopan/Eventos.csv', sep = ',')
