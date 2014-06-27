# Con la libreria rgdal creamos los shapfiles filtrados
# topojson -o agebs_zapo.json -s 1e-7 -q 1e5 agebs.shp -p state_code=+CVEGEO,name=CVEGEO --id-property CVEGEO
writeOGR(subagebmap, dsn = '/Users/alfredogarbuno/dataton/shapefiles/', layer ='agebs', driver = 'ESRI Shapefile')