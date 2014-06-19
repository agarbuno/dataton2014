library(data.table)
library(ggmap)
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
setnames(edges, c('node1', 'node2', 'date', 'user', 'count',  'time', 'long.1', 'lat.1','count.1', 'long.2', 'lat.2', 'count.2'))

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
  labs(title = "Zapopan", x = "", y = "") + theme + coord_equal() + 
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  stat_density2d(data = tur, aes(x = lon, y = lat, fill = ..level..,
                                 alpha = ..level..), bins = 10,
                 geom = "polygon") +
  scale_fill_gradient(low = "white", high= "red") 

#quartz()
ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "Zapopan", x = "", y = "") + theme + coord_equal() + 
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  stat_density2d(data = tur, aes(x = lon, y = lat, fill = ..level..,
                                 alpha = ..level..), bins = 10,
                 geom = "polygon") +
  scale_fill_gradient(low = "white", high= "red") +
  facet_wrap(~day, ncol = 4)

edges$dist <- ((edges[,lat.1]- edges[,lat.2])^2 + (edges[,long.1]- edges[,long.2])^2)^.5
edges <- edges[order(dist)]

routeQueryCheck()
distQueryCheck()

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

edges$from <- paste(deg_to_dms(edges$lat.1), deg_to_dms(edges$long.1), sep = ',')
edges$to <- paste(deg_to_dms(edges$lat.2), deg_to_dms(edges$long.2), sep = ',')
edges$from[7695]

df <- route(from = edges$from[7695], to = edges$to[7695], alternatives = FALSE)
df

edges$id <- seq(1,nrow(edges))
str(subsubedges)

subedges <- subset(edges, node1 != node2)
df <- route(from = subedges[11,]$from, to = subedges[11,]$to, alternatives = FALSE)
df

# routes <- data.frame()
# counter <- 1
# routeQueryCheck()
# for (i in 2396:2400){
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
# routeQueryCheck()
# routes
# 

# routes <- ddply(subsubedges, .(id),  function(data){ 
#   df <- route(from = data$from, to = data$to, alternatives = FALSE)
#   df <- df[, c('startLon', 'startLat', 'endLon', 'endLat', 'km', 'minutes')]
#   df
#   }, .progress = 'text')

routes <- data.table(routes)
routes

ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "Zapopan", x = "", y = "") + theme + coord_equal() + 
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  geom_segment(
    aes(x = startLon, y = startLat, xend = endLon, yend = endLat, group = id),
    alpha = .1, data = routes, color = 'darkslategray1')

map <- readShapeLines("/Users/alfredogarbuno/dataton/inegi/Vialidades/jal_eje_vial.shp",
                      verbose = TRUE, proj4string = CRS("+proj=longlat"))
submap <- subset(map, substr(CVEGEO,1,5) %in% c(14120, 14098, 14039))
map.2 <- conv_sp_lines_to_seg(submap)
streets <- geom_segment2(data=map.2, 
                         size=.25, 
                         aes(x=slon,y=slat,xend=elon, yend=elat), 
                         color="white")

ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "Zapopan", x = "", y = "") + theme + coord_equal() + 
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  streets +
  geom_segment(
    aes(x = startLon, y = startLat, xend = endLon, yend = endLat, group = id),
    alpha = .1, data = routes, color = 'tomato')
  
rm(map, submap, map.2, streets)

crosses <- routes[, list( count= .N, km = mean(km), time = mean(minutes)), by = list(startLon, startLat, endLon, endLat)]

ggplot(data = shape.fort, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'black') +
  labs(title = "Zapopan", x = "", y = "") + theme + coord_equal() + 
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  geom_segment(
    aes(x = startLon, y = startLat, xend = endLon, yend = endLat, group = id),
    alpha = .1, data = routes, color = 'tomato') +
  geom_point(data = crosses, aes(x = startLon, y = startLat, size = count), color = 'white', alpha = .4)

crosses$from <- paste(crosses$startLon, crosses$startLat, sep = '-')
crosses$to <- paste(crosses$endLon, crosses$endLat, sep = '-')
setcolorder(crosses, c("from", "to","startLon", "startLat", "endLon", "endLat", "count", "km", "time"))
crosses.edges <- crosses
cx.1 <- data.table( node = crosses$from, lon = crosses$startLon, lat = crosses$startLat)
cx.2 <- data.table( node = crosses$to, lon = crosses$endLon, lat = crosses$endLat)
crosses <- rbind(cx.1, cx.2)
crosses <- crosses[, list( count = .N), by = list(node, lon, lat)]
rm(cx.1, cx.2)

graph.inter <- graph.data.frame(crosses.edges, directed = TRUE, vertices = crosses)
crosses.edges <- crosses.edges[, list(from, to, count, km, time)]

