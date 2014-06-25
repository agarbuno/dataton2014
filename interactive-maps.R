# fuente: https://github.com/agarbuno/rMaps

# fuente: http://www.r-bloggers.com/rmaps-mexico-map/

# require(devtools)
# install_github('ramnathv/rCharts@dev')
# install_github('ramnathv/rMaps')
# install_github('rCharts', 'ramnathv')
library(rMaps)
load('~/dataton/session2.Rdata')
agebs <- plyr:::join(agebs, pesos, by = 'ageb', type = 'left')
ex <- data.table(dcast(interest, ageb~GEOGRAFICO))
agebs <- plyr:::join(agebs, ex, by = 'ageb', type = 'left')
agebs[is.na(agebs)] <- 0
setnames(agebs, c("Area Verde", "Deportivo o Recreativo", "Palacio Gobierno", "Centro médico"), c("AreasV", "Recreativos", "PalacioG", "CentroMed"))
agebs <- data.frame(agebs)
agebs$count2 <- agebs$count + runif(length(agebs$count))
agebs$ageb <- as.character(agebs$ageb)

##### Primer intento #####

agebs = transform(agebs,
                  popup = sprintf("<strong>Ageb:</strong> %s </br><strong>Tweets:</strong> %s </br><strong>Areas Verdes:</strong> %s </br><strong>Mercados:</strong> %s </br><strong>Plazas:</strong> %s </br><strong>Templos:</strong> %s </br><strong>Recreativos:</strong> %s </br><strong>Centros de Salud:</strong> %s </br><strong>Parques:</strong> %s </br><strong>Palacio de Gobierno:</strong> %s", 
                                  ageb, count, AreasV, Mercado, Plaza, Templo, Recreativos, CentroMed, Parque, PalacioG
                  ))

subagebs <- subset(agebs, count > 0)

subagebs$bt.w <- (subagebs$bt.w/max(subagebs$bt.w))
subagebs <- subset(subagebs, bt.w > 0.01)

d1 <- ichoropleth(bt.w ~ ageb, data = subagebs, map = 'agebs_zapo', ncut = 9, pal = 'Reds')

d1$set(
  geographyConfig = list(
    dataUrl = "/Users/alfredogarbuno/dataton/shapefiles/agebs_zapo2.json"
  ),
  scope = 'agebs',
  width = 1600, 
  height = 800,
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


d1$save('ageb_map3.html', cdn = TRUE)
#options(rcharts.cdn = TRUE)
#save.image('~/dataton/session3.Rdata')