library(dplyr)
library(sf)
library(osmdata)
library(tmap)

tmap_mode("view")


#### Read spatial data ####
# Take canberra for example
canberra=read_sf("https://raw.githubusercontent.com/tonywr71/GeoJson-Data/refs/heads/master/suburb-2-act.geojson")

# Make sure that the CRS should be EPSG:4326
# if CRS is not 4326, please use st_transform() to convert it
st_crs(canberra) # EPSG:4283
canberra=st_transform(canberra, crs=4326)

# Extract the boundary of the spatial data
canberra_box=st_bbox(canberra)

# Plot the map
tm_shape(canberra)+
  tm_polygons()+
  tm_shape(st_as_sfc(canberra_box))+
  tm_borders(lty="dashed", col="red")


#### Extract OSM features ####
# Basic Structure
# Use key and value tag listed in the following website:
# https://wiki.openstreetmap.org/wiki/Map_features
opq(BOUNDARY_BOX)%>%
  add_osm_feature(key="KEY", value="VALUE")%>%
  osmdata_sf()

# Take sidewalk in Canberra for example
sidewalk=opq(canberra_box)%>%
  add_osm_feature(key="footway", value="sidewalk")%>%
  osmdata_sf()

# Plot the result
tm_shape(canberra)+
  tm_polygons(alpha=0.2, border.alpha=0.2)+
  tm_shape(sidewalk$osm_lines)+
  tm_lines(col="blue")+
  tm_shape(st_as_sfc(canberra_box))+
  tm_borders(lty="dashed", col="red")

