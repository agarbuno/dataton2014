brew install node.js
npm install -g topojson

curl -o estados.zip http://mapserver.inegi.org.mx/MGN/mge2010v5_0.zip
unzip estados.zip
ogr2ogr states.shp Entidades_2010_5.shp -t_srs "+proj=longlat +ellps=WGS84 +no_defs +towgs84=0,0,0"
topojson -o mx_states.json -s 1e-7 -q 1e5 states.shp -p state_code=+CVE_ENT,name=NOM_ENT --id-property NOM_ENT

topojson -o agebs_zapo.json -s 1e-7 -q 1e5 agebs.shp -p state_code=+CVEGEO,name=CVEGEO --id-property CVEGEO
topojson -o agebs_zapo.json -s 1e-18 -q 1e5 agebs.shp -p state_code=+CVEGEO,name=CVEGEO --id-property CVEGEO