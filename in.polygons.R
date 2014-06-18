library(data.table)
library(igraph)
library(reshape2)

# http://spatial.ly/2012/02/great-maps-ggplot2/

# munmap <- readShapePoly("/Users/alfredogarbuno/dataton/inegi/Municipios/jal_municipal.shp",
#                         verbose = TRUE, proj4string = CRS("+proj=longlat"))
# 
# colmap <- readShapePoly("/Users/alfredogarbuno/dataton/colonias/Colonias.shp",
#                         verbose = TRUE, proj4string = CRS("+proj=longlat"))
# 
# locmap <- readShapePoly("/Users/alfredogarbuno/dataton/inegi/Localidad/jal_loc_urb.shp",
#                         verbose = TRUE, proj4string = CRS("+proj=longlat"))

agebmap <- readShapePoly("/Users/alfredogarbuno/dataton/inegi/Ageb/jal_ageb_urb.shp",
                         verbose = TRUE, proj4string = CRS("+proj=longlat"))

# manmap <- readShapePoly("/Users/alfredogarbuno/dataton/inegi/Manzana/jal_manzanas.shp",
#                         verbose = TRUE, proj4string = CRS("+proj=longlat"))

# sublocmap <- subset(locmap, substr(CVEGEO,1,5) %in% c(14120))
# submanmap <- subset(manmap, substr(CVEGEO,1,5) %in% c(14120))
# subcolmap <- subset(colmap, MUN_NAME == 'ZAPOPAN')

subagebmap <- subset(agebmap, substr(CVEGEO,1,5) %in% c(14120, 14098, 14039, 14097))

shape.fort <- fortify(subagebmap) 
shape.fort <- shape.fort[order(shape.fort$order), ] 

ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), colour='black', fill='white') +
  labs(title = "Zapopan", x = "", y = "") 
#  geom_text(data=centroids, aes(x = long, y = lat, label=id), inherit.aes=FALSE, size = 2.7)
#geom_point(data = social, aes(x = lon, y = lat, color = social), size = .5)

#submap <- subset(munmap, substr(CVEGEO,1,5) %in% c(14120, 14098, 14039, 14097))

##### Esto es para ver los poligonos y los vecinos
shape.fort <- fortify(subagebmap) 
shape.fort <- shape.fort[order(shape.fort$order), ] 
# aux <- turistas
# turistas <- social 
# turistas <- aux

points <- SpatialPoints(data.frame(turistas[,lon],turistas[,lat]),  proj4string = CRS("+proj=longlat"))
in.poly <- over(points, subagebmap)
in.poly <- in.poly$CVEGEO

turistas$ageb <- in.poly
turistas <- data.frame(turistas)
tur <- data.table(turistas[!is.na(turistas$ageb),])

factors <- levels(factor(subagebmap@data$CVEGEO))
tur$ageb <- as.character(tur$ageb)
tur <- tur[order(user)]

#subtur <- tur[seq(1:1000),]
#ddply(subtur, .(user), summarise, len = length(levels(factor(ageb))))

res <- data.table(ddply(tur, .(user), function(data) { 
  expand.grid( levels(factor(data$ageb)), levels(factor(data$ageb)))
  }, .parallel = TRUE))
res <- subset(res, as.character(Var1) <= as.character(Var2))
res <- res[order(user)]
edges <- res[, list(count = .N), by = list(Var1,Var2)]

shape.fort <- data.table(shape.fort)
centroids <- shape.fort[, data.table(kmeans(cbind(long,lat),centers=1)$centers) , by=id]
centroids
agebs <- data.frame(ageb = subagebmap@data$CVEGEO, id = subagebmap@data$OID-1)
agebs <- plyr:::join(centroids, agebs, by= 'id')
pesos <- tur[, list(count = .N), by = ageb]
agebs <- plyr:::join(agebs, pesos, by= 'ageb', type = 'left')
agebs[is.na(agebs)] <- 0
agebs
setcolorder(agebs, c('ageb', 'long', 'lat', 'id', 'count'))

shape.fort <- plyr:::join(shape.fort, agebs, by= 'id', type = 'left')

theme <- theme(panel.grid.minor = element_blank(), axis.ticks = element_blank(), 
         axis.title.x = element_blank(), axis.title.y = element_blank(), 
         axis.text.x = element_blank(), axis.text.y = element_blank()
         )

ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group, fill = count)) +
  labs(title = "", x = "", y = "") + theme + coord_equal() +
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  scale_fill_continuous(low = 'black', high =  'tomato')

##### Agregando analisis de redes
graph <- graph.data.frame(edges, directed = FALSE, vertices = agebs)
pg.w <- page.rank(graph, weight = edges$count)
pg <- page.rank(graph)

bt <- betweenness(graph)
bt.w <- betweenness(graph, weight = edges$count)

eig <- alpha.centrality(graph)

pesos <- data.table(ageb = names(pg.w$vector), pg.w = pg.w$vector, pg = pg$vector, bt, bt.w, eig)
shape.fort <- plyr:::join(shape.fort, pesos, by= 'ageb', type = 'left')
shape.fort

#quartz()
ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group, fill = pg)) +
  labs(title = "Pagerank", x = "", y = "") + theme + coord_equal() +
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  scale_fill_continuous(low = 'black', high =  'tomato')
  #geom_point(data = social, aes(x = lon, y = lat), alpha = .05, size = .4, color = 'white')

#quartz()
ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group, fill = pg.w)) +
  labs(title = "Pagerank ponderado", x = "", y = "") + theme + coord_equal() +
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  scale_fill_continuous(low = 'black', high =  'tomato')

#quartz()
ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group, fill = bt)) +
  labs(title = "Betweenness", x = "", y = "") + theme + coord_equal() +
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  scale_fill_continuous(low = 'black', high =  'tomato')

#quartz()
ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group, fill = bt.w)) +
  labs(title = "Betweenness ponderado", x = "", y = "") + theme + coord_equal() +
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  scale_fill_continuous(low = 'black', high =  'tomato')

# write.graph(graph, file = '/Users/alfredogarbuno/Desktop/export_02.gml', format = 'gml')
setnames(edges, c('node1', 'node2', 'count'))
setnames(edges, c('node1'), c('ageb'))
edges <- plyr:::join(edges, agebs, by= 'ageb')
edges$id <- NULL
setnames(edges, c('ageb', 'node2', 'long', 'lat'), c('node1', 'ageb', 'long.1', 'lat.1'))
edges <- plyr:::join(edges, agebs, by= 'ageb')
setnames(edges, c('ageb', 'long', 'lat'), c('node2', 'long.2', 'lat.2'))
edges$id <- NULL
setnames(edges, c('node1', 'node2', 'count', 'long.1', 'lat.1','count.1', 'long.2', 'lat.2', 'count.2'))

theme <- theme(panel.grid.minor = element_blank(), axis.ticks = element_blank(), 
               axis.title.x = element_blank(), axis.title.y = element_blank(), 
               axis.text.x = element_blank(), axis.text.y = element_blank(),
               legend.position = "none")
#quartz()
ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "Zapopan", x = "", y = "") + theme + coord_equal() + 
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  geom_segment(data = edges, aes(x = long.1, y = lat.1, xend = long.2, yend = lat.2, color = node1), alpha = .1)

#quartz()
ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "Zapopan", x = "", y = "") + theme + coord_equal() + 
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  stat_density2d(data = tur, aes(x = lon, y = lat, fill = ..level..,
        alpha = ..level..), bins = 10,
    geom = "polygon") +
  scale_fill_gradient(low = "white", high= "green") 

#quartz()
ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "Zapopan", x = "", y = "") + theme + coord_equal() + 
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  stat_density2d(data = tur, aes(x = lon, y = lat, fill = ..level..,
                                 alpha = ..level..), bins = 10,
                 geom = "polygon") +
  scale_fill_gradient(low = "white", high= "green") +
  facet_wrap(~day, ncol = 4)
