#load('~/dataton/session2.Rdata')

which.max(agebs$count)
agebs[1082,]

agebs[order(count)]
subset(interest, ageb == '1412000013293')
subset(interest, ageb == '1403900014584')
subset(interest, ageb == '1403900011359')
subset(interest, ageb == '141200001066A')
subset(interest, ageb == '1403900011630')

manmap <- readShapePoly("/Users/alfredogarbuno/dataton/inegi/Manzana/jal_manzanas.shp",
                        verbose = TRUE, proj4string = CRS("+proj=longlat"))

ageb.cve <- 1412000013293
ageb.cve <- 1403900014584
ageb.cve <- 1403900011359

submanmap <- subset(manmap, substr(CVEGEO,1,13) %in% c(ageb.cve))
shape.man <- fortify(submanmap) 
shape.man <- shape.man[order(shape.man$order), ] 

sub.tur <- subset(tur, ageb == ageb.cve)
sub.interest <- subset(interest, ageb == ageb.cve)

ggplot(data = shape.man, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group)) +
  labs(title = "Zapopan", x = "", y = "")  + theme +
  theme(panel.background = element_rect(fill='gray50'), panel.grid.major = element_blank()) +
  geom_point(data = sub.tur, aes( x = lon, y = lat), colour = 'white', alpha = .7) +
  geom_point(data = sub.interest, aes( x = long, y = lat, colour = GEOGRAFICO), size = 2.5, alpha = .7) 

