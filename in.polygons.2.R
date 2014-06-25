#install.packages(c('igraph', 'reshape2'))
#install.packages('ggmap')

# Referencia de mapas con lineas de transporte:
# http://spatial.ly/2012/02/great-maps-ggplot2/

# munmap <- readShapePoly("~/dataton/inegi/Municipios/jal_municipal.shp",
#                         verbose = TRUE, proj4string = CRS("+proj=longlat"))
# 
# colmap <- readShapePoly("~/dataton/colonias/Colonias.shp",
#                         verbose = TRUE, proj4string = CRS("+proj=longlat"))
# 
# locmap <- readShapePoly("~/dataton/inegi/Localidad/jal_loc_urb.shp",
#                         verbose = TRUE, proj4string = CRS("+proj=longlat"))

agebmap <- readShapePoly("~/dataton/inegi/Ageb/jal_ageb_urb.shp",
                         verbose = TRUE, proj4string = CRS("+proj=longlat"))

# manmap <- readShapePoly("/Users/alfredogarbuno/dataton/inegi/Manzana/jal_manzanas.shp",
#                         verbose = TRUE, proj4string = CRS("+proj=longlat"))

# sublocmap <- subset(locmap, substr(CVEGEO,1,5) %in% c(14120))
# submunmap <- subset(munmap, substr(CVEGEO,1,5) %in% c(14120, 14098, 14039, 14097))
# subcolmap <- subset(colmap, MUN_NAME == 'ZAPOPAN')

subagebmap <- subset(agebmap, substr(CVEGEO,1,5) %in% c(14120, 14098, 14039, 14097))

shape.fort <- fortify(subagebmap) 
shape.fort <- shape.fort[order(shape.fort$order), ] 

subagebmap@data$id <- subagebmap@data$OID - 1
cat.ageb <- data.frame( id = subagebmap@data$id, ageb = subagebmap@data$CVEGEO)
shape.fort <- plyr:::join(shape.fort, cat.ageb, type = 'left')
shape.fort$mun <- factor(substr(shape.fort$ageb, 1, 5))
shape.fort$mun <- factor(shape.fort$mun, labels = c('Guadalajara', 'Tlajomulco de Zuñiga', 'Tlaquepaque', 'Zapopan'))

theme <- theme(panel.grid.minor = element_blank(), axis.ticks = element_blank(), 
               axis.title.x = element_blank(), axis.title.y = element_blank(), 
               axis.text.x = element_blank(), axis.text.y = element_blank(),
               legend.position = "none")

ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group, colour = mun), fill='black') +
  labs(title = "", x = "", y = "") + coord_equal() +
  theme + theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank())

ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "", x = "", y = "") + coord_equal() +
  theme + theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  #  geom_text(data=centroids, aes(x = long, y = lat, label=id), inherit.aes=FALSE, size = 2.7)
  geom_point(data = social, aes(x = lon, y = lat, color = social), size = .5)

#submap <- subset(munmap, substr(CVEGEO,1,5) %in% c(14120, 14098, 14039, 14097))

##### Esto es para ver los poligonos y los vecinos
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
tur$time.bin <- cut(tur$time, breaks = c(0, 4, 8, 12, 16, 20, 24), include.lowest = TRUE)

tur.2 <- tur
tur.2$id <- paste(tur[,date], tur[,hour], tur[,ageb], sep = '|')

#subtur <- tur[seq(1:1000),]
#ddply(subtur, .(user), summarise, len = length(levels(factor(ageb))))

res <- data.table(ddply(tur.2, .(user), function(data) {
  dt <- data.frame(expand.grid( data$id, data$id ))
  dt$date1 <- substr(dt$Var1,1,10)
  dt$date2 <- substr(dt$Var2,1,10)
  dt$hour1 <- as.numeric(substr(dt$Var1,12,13)) + as.numeric(substr(dt$Var1,15,16)) /60 + as.numeric(substr(dt$Var1,18,19)) / 3600
  dt$hour2 <- as.numeric(substr(dt$Var2,12,13)) + as.numeric(substr(dt$Var2,15,16)) /60 + as.numeric(substr(dt$Var2,18,19)) / 3600
  dt$Var1 <- substr(dt$Var1, 21, 34)
  dt$Var2 <- substr(dt$Var2, 21, 34)
  subset(dt, date1 == date2 & hour1 < hour2 & hour2 - hour1 < 4)
  }, 
  .parallel = TRUE))
res <- res[order(user)]

#edges <- res[, list(count = .N), by = list(Var1,Var2)]
edges <- res[, list(count = .N, time = (mean(hour1) + mean(hour2)) / 2 ) , by = list(Var1,Var2,date1,user)]

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

help(dcast)
test <- dcast.data.table(tur, ageb ~ social)
agebs <- plyr:::join(agebs, test, type = 'left')
agebs[is.na(agebs)] <- 0
rm(test)

shape.fort <- plyr:::join(shape.fort, agebs, by= 'id', type = 'left')

theme <- theme(panel.grid.minor = element_blank(), axis.ticks = element_blank(), 
               axis.title.x = element_blank(), axis.title.y = element_blank(), 
               axis.text.x = element_blank(), axis.text.y = element_blank()
)

ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group, fill = count) )+
  labs(title = "", x = "", y = "") + 
  theme + 
  coord_equal() +
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  scale_fill_continuous(low = 'black', high =  'wheat1')

##### Agregando analisis de redes
graph <- graph.data.frame(edges, directed = FALSE, vertices = agebs)
pg.w <- page.rank(graph, weight = edges$count)
pg <- page.rank(graph, weight = NA)

bt <- betweenness(graph, weight = NA)
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
  labs(title = "", x = "", y = "") + theme + coord_equal() +
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
  labs(title = "", x = "", y = "") + theme + coord_equal() +
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  scale_fill_continuous(low = 'black', high =  'tomato')

# write.graph(graph, file = '/Users/alfredogarbuno/Desktop/export_02.gml', format = 'gml')
edges
setnames(edges, c('Var1', 'Var2', 'count'), c('node1', 'node2', 'count'))
setnames(edges, c('node1'), c('ageb'))
edges <- plyr:::join(edges, agebs, by= 'ageb')
edges$id <- NULL
setnames(edges, c('ageb', 'node2', 'long', 'lat'), c('node1', 'ageb', 'long.1', 'lat.1'))
edges <- plyr:::join(edges, agebs, by= 'ageb')
setnames(edges, c('ageb', 'long', 'lat'), c('node2', 'long.2', 'lat.2'))
edges$id <- NULL
edges
setnames(edges, c('node1', 'node2', 'date', 'user', 'count',  'time', 'long.1', 'lat.1','count.1', 'foursq.1', 'twitter.1' ,'long.2', 'lat.2', 'count.2', 'foursq.2', 'twitter.2'))

theme <- theme(panel.grid.minor = element_blank(), axis.ticks = element_blank(), 
               axis.title.x = element_blank(), axis.title.y = element_blank(), 
               axis.text.x = element_blank(), axis.text.y = element_blank(),
               legend.position = "none")
#quartz()
ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "Zapopan", x = "", y = "") + theme + coord_equal() + 
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  geom_segment(data = subset(edges, count > 2), aes(x = long.1, y = lat.1, xend = long.2, yend = lat.2, color = node1), alpha = .15)

#quartz()
ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "", x = "", y = "") + theme + coord_equal() + 
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  stat_density2d(data = tur, aes(x = lon, y = lat, fill = ..level..,
                                 alpha = ..level..), bins = 10,
                 geom = "polygon") +
  scale_fill_gradient(low = "white", high= "lawngreen") 

#quartz()
ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "", x = "", y = "") + theme + coord_equal() + 
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  stat_density2d(data = tur, aes(x = lon, y = lat, fill = ..level..,
                                 alpha = ..level..), bins = 10,
                 geom = "polygon") +
  scale_fill_gradient(low = "white", high= "lawngreen") +
  facet_grid(~time.bin)

edges$dist <- ((edges[,lat.1]- edges[,lat.2])^2 + (edges[,long.1]- edges[,long.2])^2)^.5
edges <- edges[order(dist)]

routeQueryCheck()
distQueryCheck()

edges$from <- paste(deg_to_dms(edges$lat.1), deg_to_dms(edges$long.1), sep = ',')
edges$to <- paste(deg_to_dms(edges$lat.2), deg_to_dms(edges$long.2), sep = ',')
edges$id <- seq(1,nrow(edges))
subedges <- subset(edges, node1 != node2)

#routes <- data.frame()
#counter <- 1
# routeQueryCheck()
# for (i in 2522:3499){
#   data <- subedges[i,]
#   df <- route(from = data$from, to = data$to, alternatives = FALSE)
#   df <- df[, c('startLon', 'startLat', 'endLon', 'endLat', 'km', 'minutes')]
#   df$id <- data$id
#   routes <- rbind(routes, df)
#   cat('Paso: ', i)
#   if (counter == 9){
#     Sys.sleep(3)
#     counter <- 0
#   }
#   counter <- counter + 1
# }
routeQueryCheck()
routes <- fread('~/dataton/routes.csv', sep = ',')
routes.dt <- subedges[, list(id, date, time)]
routes.dt$date <- as.Date(routes.dt$date)
routes.dt$day  <- wday(routes.dt$date,  label = TRUE, abbr = FALSE)
routes.dt$time.bin <- cut(routes.dt$time, breaks = c(0, 4, 8, 12, 16, 20, 24), include.lowest = TRUE)
routes <- plyr:::join(routes, routes.dt, by = 'id')
 
# routes <- ddply(subsubedges, .(id),  function(data){ 
#   df <- route(from = data$from, to = data$to, alternatives = FALSE)
#   df <- df[, c('startLon', 'startLat', 'endLon', 'endLat', 'km', 'minutes')]
#   df
#   }, .progress = 'text')

#write.csv(routes, '~/Dropbox/Dataton/routes2.csv', row.names = FALSE)

#quartz()
ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "Zapopan", x = "", y = "") + theme + coord_equal() + 
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  geom_segment(
    aes(x = startLon, y = startLat, xend = endLon, yend = endLat, group = id),
    alpha = .1, data = routes, color = 'darkslategray1')

#quartz()
ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "Zapopan", x = "", y = "") + theme + coord_equal() + 
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  geom_segment(
    aes(x = startLon, y = startLat, xend = endLon, yend = endLat, group = id),
    alpha = .1, data = routes, color = 'darkslategray1') +
  facet_grid(~day)

#quartz()
ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "", x = "", y = "") + theme + coord_equal() + 
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  geom_segment(
    aes(x = startLon, y = startLat, xend = endLon, yend = endLat, group = id),
    alpha = .1, data = routes, color = 'darkslategray1') +
  facet_grid(~time.bin)

ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "Zapopan", x = "", y = "") + theme + coord_equal() + 
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  geom_segment(
    aes(x = startLon, y = startLat, xend = endLon, yend = endLat, group = id),
    alpha = .1, data = routes, color = 'darkslategray1') +
  facet_grid(time.bin~day)

map <- readShapeLines("~/dataton/inegi/Vialidades/jal_eje_vial.shp",
                      verbose = TRUE, proj4string = CRS("+proj=longlat"))
submap <- subset(map, substr(CVEGEO,1,5) %in% c(14120, 14098, 14039, 14097))
map.2 <- conv_sp_lines_to_seg(submap)
streets <- geom_segment2(data=map.2, 
                         size=.35, 
                         aes(x=slon,y=slat,xend=elon, yend=elat), 
                         color="white")

ggplot() +
  streets +
  labs(title = "", x = "", y = "") + theme + coord_equal() + 
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank())

ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "Zapopan", x = "", y = "") + theme + coord_equal() + 
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  streets +
  geom_segment(
    aes(x = startLon, y = startLat, xend = endLon, yend = endLat, group = id),
    alpha = .1, data = routes, color = 'tomato')
  
routes

rm(map, submap, map.2, streets)

crosses <- routes[, list( count= .N, km = mean(km), time = mean(minutes)), by = list(startLon, startLat, endLon, endLat)]
crosses.day <- routes[, list( count= .N, km = mean(km), time = mean(minutes)), by = list(startLon, startLat, endLon, endLat, day)]
crosses.time <- routes[, list( count= .N, km = mean(km), time = mean(minutes)), by = list(startLon, startLat, endLon, endLat, time.bin)]
crosses.daytime <- routes[, list( count= .N, km = mean(km), time = mean(minutes)), by = list(startLon, startLat, endLon, endLat, day, time.bin)]

#quartz()
ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "", x = "", y = "") + theme + coord_equal() + 
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  geom_segment(
    aes(x = startLon, y = startLat, xend = endLon, yend = endLat, group = id, color = factor(id)),
    alpha = .1, data = routes)

ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "Zapopan", x = "", y = "") + theme + coord_equal() + 
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  geom_segment(
    aes(x = startLon, y = startLat, xend = endLon, yend = endLat, group = id),
    alpha = .1, data = routes, color = 'tomato') +
  geom_point(data = crosses, aes(x = startLon, y = startLat, size = count), color = 'white', alpha = .4)

#quartz()
# ggplot(data = shape.fort, aes(x = long, y = lat)) + 
#   geom_polygon(aes(group = group), fill = 'black') +
#   labs(title = "Zapopan", x = "", y = "") + theme + coord_equal() + 
#   theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
#   geom_segment(
#     aes(x = startLon, y = startLat, xend = endLon, yend = endLat, group = id),
#     alpha = .1, data = routes, color = 'tomato') +
#   geom_point(data = crosses.time, aes(x = startLon, y = startLat, size = count), color = 'white', alpha = .4) +
#   facet_grid(~time.bin)

#quartz()
# ggplot(data = shape.fort, aes(x = long, y = lat)) + 
#   geom_polygon(aes(group = group), fill = 'black') +
#   labs(title = "Zapopan", x = "", y = "") + theme + coord_equal() + 
#   theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
#   geom_segment(
#     aes(x = startLon, y = startLat, xend = endLon, yend = endLat, group = id),
#     alpha = .1, data = routes, color = 'tomato') +
#   geom_point(data = crosses.daytime, aes(x = startLon, y = startLat, size = count), color = 'white', alpha = .4) +
#   facet_grid(time.bin~day)

crosses$from <- paste( formatC(crosses$startLon, flag = 0, digits = 8, format = 'f')
                      ,formatC(crosses$startLat, flag = 0, digits = 8, format = 'f')
                      , sep = '-')
crosses$to <- paste(   formatC(crosses$endLon, flag = 0, digits = 8, format = 'f')
                     , formatC(crosses$endLat, flag = 0, digits = 8, format = 'f')
                     , sep = '-')
setcolorder(crosses, c("from", "to","startLon", "startLat", "endLon", "endLat", "count", "km", "time"))

crosses
crosses.edges <- crosses
cx.1 <- data.table( node = crosses$from, lon = crosses$startLon, lat = crosses$startLat, count = crosses$count)
cx.2 <- data.table( node = crosses$to, lon = crosses$endLon, lat = crosses$endLat, count = crosses$count)
crosses <- rbind(cx.1, cx.2)
crosses <- crosses[, list( refs = sum(count), count = .N ), by = list(node, lon, lat)]
rm(cx.1, cx.2)

graph.inter <- graph.data.frame(crosses.edges, directed = TRUE, vertices = crosses)
crosses.edges <- crosses.edges[, list(from, to, count, km, time)]

crosses

#quartz()
ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "", x = "", y = "") + theme + coord_equal() + 
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  geom_segment(
    aes(x = startLon, y = startLat, xend = endLon, yend = endLat, group = id, color = factor(id)),
    alpha = .1, data = routes) +
  geom_point(data = crosses, aes(x = lon, y = lat, size = refs), color = 'white', alpha = .4)

pg.w <- page.rank(graph.inter, weight = crosses.edges$count, directed = TRUE)
pg <- page.rank(graph.inter, directed = TRUE)

bt <- betweenness(graph.inter, directed = TRUE)
bt.w <- betweenness(graph.inter, weight = crosses.edges$count, directed = TRUE)

crosses$pg.w <- pg.w$vector
crosses$pg <- pg$vector
crosses$bt <- bt
crosses$bt.w <- bt.w

rm(pg.w, pg, bt, bt.w)

ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "", x = "", y = "") + coord_equal() + theme + 
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  geom_segment(
    aes(x = startLon, y = startLat, xend = endLon, yend = endLat, group = id),
    alpha = .1, data = routes, color = 'darkred') +
  geom_point(data = crosses, aes(x = lon, y = lat, size = pg.w/max(pg.w) * 10, alpha = pg.w/max(pg.w) + .3
                                 , colour = pg.w/max(pg.w))) +
  scale_colour_gradient2(low = "salmon", high= "cyan2")
  
ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "Zapopan", x = "", y = "") + coord_equal() + theme + 
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  geom_segment(
    aes(x = startLon, y = startLat, xend = endLon, yend = endLat, group = id),
    alpha = .1, data = routes, color = 'tomato') +
  geom_point(data = crosses, aes(x = lon, y = lat, size = pg/max(pg) * 10, alpha = pg/max(pg)
                                 , colour = pg/max(pg))) +
  scale_colour_gradient2(low = "white", high= "darkslategray1")

ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "Zapopan", x = "", y = "") + coord_equal() + theme + 
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  geom_segment(
    aes(x = startLon, y = startLat, xend = endLon, yend = endLat, group = id),
    alpha = .1, data = routes, color = 'tomato') +
  geom_point(data = crosses, aes(x = lon, y = lat, size = bt/max(bt) * 10, alpha = bt/max(bt)
                                 , colour = bt/max(bt))) +
  scale_colour_gradient2(low = "white", high= "darkslategray1")

ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "", x = "", y = "") + coord_equal() + theme + 
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  geom_segment(
    aes(x = startLon, y = startLat, xend = endLon, yend = endLat, group = id),
    alpha = .1, data = routes, color = 'darkred') +
  geom_point(data = crosses, aes(x = lon, y = lat, size = bt.w/max(bt.w) * 10, alpha = bt.w/max(bt.w) + 0.3
                                 , colour = bt.w/max(bt.w))) +
  scale_colour_gradient2(low = "salmon", high= "cyan2")

#save.image('~/dataton/session1.Rdata')
#load('~/dataton/session1.Rdata')