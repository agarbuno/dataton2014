# fuente: https://github.com/agarbuno/rMaps

# fuente: http://www.r-bloggers.com/rmaps-mexico-map/

# require(devtools)
# install_github('ramnathv/rCharts@dev')
# install_github('ramnathv/rMaps')
# install_github('rCharts', 'ramnathv')
library(rMaps)
library(rjson)
library(RJSONIO)
load('~/dataton/session2.Rdata')
crosses$point <- paste(crosses[,lat], crosses[,lon])
crosses$radius <- crosses$pg.w/max(crosses$pg.w) * 15
crosses$pg.w <- crosses$pg.w/max(crosses$pg.w) * 10
crosses$longitude <- as.character(crosses$lon)
crosses$latitude <- as.character(crosses$lat)
crosses$id <- seq(1, nrow(crosses))

subcrosses <- subset(crosses, pg.w > quantile(crosses$pg.w)[4])

# directions <- data.frame()
# counter <- 1
# 
# for (i in 1:1178){
#   data <- subcrosses[i,]
#   df <- geocode(data$point, messaging = FALSE, output = 'more')
#   df$id <- data$id
#   directions <- rbind(directions, df)
#   cat('Paso: ', i)
#   if (counter == 9){
#     Sys.sleep(3)
#     counter <- 0
#   }
#   counter <- counter + 1
# }

#write.table(directions, '~/Dropbox/Dataton/addresses.psv', row.names = FALSE, sep = '|')
directions <- read.table('~/Dropbox/Dataton/addresses.psv', sep = '|', header = TRUE)
subcrosses$address <- directions$address

subcrosses = transform(subcrosses,
                    popup = sprintf("<strong>Longitud:</strong> %s </br><strong>Latitud:</strong> %s </br><strong>Rank:</strong> %s </br><strong>Dirección:</strong> %s", 
                                    lon, lat, round(pg.w,2), address
                    ))

subcrosses <- data.frame(subcrosses)

# cross.list <- dlply(subcrosses, .(id), function(data){ return (data.frame(data))} )
# names(cross.list) <- ''
# head(rjson:::toJSON(cross.list))

##### Primer intento #####
d1 <- ichoropleth(pg ~ node, data = subcrosses, map = 'agebs_zapo', ncut = 5, pal = 'PuRd')

d1$params$bubbles <- d1$params$data
d1$params$data <- NULL
names(d1$params$bubbles) <- ''

d1$set(
  geographyConfig = list(
    dataUrl = "/Users/alfredogarbuno/dataton/shapefiles/agebs_zapo2.json"
  ),
  scope = 'agebs',
  width = 3200, 
  height = 1600,
  setProjection = '#! function( element, options ) {
  var projection, path;
  projection = d3.geo.mercator()
  .center([-103.4, 20.7]).scale(element.offsetWidth*100)
  .translate([element.offsetWidth / 2, element.offsetHeight / 2]);
  path = d3.geo.path().projection( projection );
  return {path: path, projection: projection};
  } !#'
)

d1$set(geographyConfig = list(
  popupTemplate = "#! function(geography, data){
  return '<div class=hoverinfo>' + data.popup + '</div>';
  } !#", 
  popupOnHover = 'true'
))

d1$set(bubbleConfig = list(
  popupTemplate = "#! function(geography, data){
  return '<div class=hoverinfo>' + data.popup + '</div>';
  } !#" ,
  fillOpacity = 0.85,
  highlightFillOpacity = 1, 
  borderWidth = 1
))


d1$save('rMaps3.html', cdn = TRUE)
#options(rcharts.cdn = TRUE)
#save.image('~/dataton/session3.Rdata')
