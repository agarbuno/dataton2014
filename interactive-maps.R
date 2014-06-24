# fuente: https://github.com/agarbuno/rMaps

# fuente: http://www.r-bloggers.com/rmaps-mexico-map/

# require(devtools)
# install_github('ramnathv/rCharts@dev')
# install_github('ramnathv/rMaps')
# install_github('rCharts', 'ramnathv')
library(rMaps)
load('~/dataton/session2.Rdata')
agebs <- plyr:::join(agebs, pesos, by = 'ageb', type = 'left')
agebs <- plyr:::join(agebs, ex, by = 'ageb', type = 'left')
agebs[is.na(agebs)] <- 0
setnames(agebs, c("Area Verde", "Deportivo o Recreativo"), c("AreasV", "Recreativos"))
agebs <- data.frame(agebs)
agebs$count2 <- agebs$count + runif(length(agebs$count))
agebs$ageb <- as.character(agebs$ageb)

##### Primer intento #####

agebs = transform(agebs,
                  popup = sprintf("<strong>Ageb:</strong> %s </br><strong>Tweets:</strong> %s </br><strong>Areas Verdes:</strong> %s </br><strong>Mercados:</strong> %s </br><strong>Plazas:</strong> %s </br><strong>Templos:</strong> %s </br><strong>Recreativos:</strong> %s", 
                                  ageb, count, AreasV, Mercado, Plaza, Templo, Recreativos
                  ))

subagebs <- subset(agebs, count > 0)

d1 <- ichoropleth(count2 ~ ageb, data = subagebs, map = 'agebs_zapo', ncut = 9, pal = 'Reds')

d1$set(
  geographyConfig = list(
    dataUrl = "/Users/alfredogarbuno/dataton/shapefiles/agebs_zapo.json"
  ),
  scope = 'agebs',
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
  } !#" 
))

d1$save('rMaps.html', cdn = TRUE)
options(rcharts.cdn = TRUE)

# ggplot(data = shape.fort, aes(x = long, y = lat)) + 
#     geom_polygon(aes(group = group), fill = 'black') +
#     labs(title = "Zapopan", x = "", y = "") + coord_equal() + 
#     theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank())

# fuente: http://ropensci.org/blog/2014/04/17/plotly/
# library("devtools")
# install_github("plotly", "ropensci")

# library("plotly")
# response <- signup("agarbuno", "alfredo.garbuno@gmail.com")
# py <- plotly("RgraphingAPI", "izwc728prc")
# 
# load('~/dataton/session2.Rdata')
# 
# beau.plot <- ggplot(data = shape.fort, aes(x = long, y = lat)) + 
#   geom_polygon(aes(group = group), fill = 'black') +
#   labs(title = "Zapopan", x = "", y = "") + theme + coord_equal() + 
#   theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
#   geom_segment(
#     aes(x = startLon, y = startLat, xend = endLon, yend = endLat, group = id),
#     alpha = .1, data = routes, color = 'darkslategray1')
# 
# py$ggplotly(beau.plot)