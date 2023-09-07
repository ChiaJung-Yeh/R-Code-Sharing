library(dplyr)
library(sf)
library(tmap)
library(leaflet)
library(leaflet.providers)
library(leafpop)
library(TWspdata)
library(ggplot2)
library(TDX)
library(osmdata)

client_id="robert1328.mg10-5fef152e-ee4f-4cfb"
client_secret="591fe8aa-6fdc-4c3b-9663-428a62ec8863"
access_token=get_token(client_id, client_secret)


tmap_mode("view")

tamsui_itinerary=read.csv("./tamsui_itinerary/tamsui_itinerary.csv", fileEncoding="Big5")
tamsui_itinerary$lat=substr(tamsui_itinerary$Location, 1, regexpr(",", tamsui_itinerary$Location)-1)
tamsui_itinerary$lon=substr(tamsui_itinerary$Location, regexpr(",", tamsui_itinerary$Location)+2, 100)

temp=select(tamsui_itinerary, Site, Path, lon, lat, Path)
tamsui_itinerary_line=cbind(temp[1:(nrow(temp)-1),] %>% rename(Site_O=Site, lon_O=lon, lat_O=lat), select(temp[2:nrow(temp),], -Path) %>% rename(Site_D=Site, lon_D=lon, lat_D=lat))%>%
  mutate(geometry=st_as_sfc(paste0("LINESTRING(", lon_O, " ", lat_O, ",", lon_D, " ", lat_D, ")")))%>%
  st_sf(crs=4326)

tamsui_itinerary=mutate(tamsui_itinerary, geometry=st_as_sfc(paste0("POINT(", lon, " ", lat, ")")))%>%
  st_sf(crs=4326)%>%
  select(-Location, -lon, -lat, -Path)%>%
  filter(Site!="dummy")


leaflet()%>%
  addTiles()%>%
  addProviderTiles("CartoDB.Positron")%>%
  addMarkers(data=tamsui_itinerary, label=tamsui_itinerary$Site,
             labelOptions=labelOptions(noHide=T, direction="bottom", opacity=0.85,
                                       style = list(
                                         "color"="#003D79",
                                         "font-family"="serif",
                                         "font-style"="bold",
                                         "box-shadow" = "2px 3px rgba(0,0,0,0.25)",
                                         "font-size"="20px"
                                       )),
             popup=popupTable(rename(tamsui_itinerary, 景點=Site, 時間=Time, 備註=Note),
                              zcol=c("景點", "時間", "備註"),
                              feature.id=F,
                              row.numbers=F))%>%
  addPolylines(data=tamsui_itinerary_line, popup=tamsui_itinerary_line$Path, color="#02C874", weight=11, popupOptions=popupOptions(style=list("color"="red")))




riverpoly=read_sf("C:/Users/ASUS/Desktop/R Transportation/Map Plot/riverpoly/riverpoly.shp")%>%
  st_transform(crs=4326)
bike_shape=rbind(Bike_Shape(access_token, "Taipei", dtype="sf"), Bike_Shape(access_token, "NewTaipei", dtype="sf"))
bike_shape=bike_shape[st_is_valid(bike_shape),]%>%
  filter(!RouteName %in% c("濱海路(南側)","濱海路1、2段"))

taipei_mrt=rbind(Rail_Shape(access_token, "TRTC", dtype="sf"), Rail_Shape(access_token, "NTDLRT", dtype="sf"))


temp1=st_zm(read_sf("./tamsui_itinerary/history-2023-05-01.kml")[c(6,8,10),])
temp2=st_zm(read_sf("./tamsui_itinerary/history-2021-11-04.kml")[c(15),])
bike_ride=rbind(temp1, temp2)

taipei_road=opq(st_as_sfc(st_bbox(c(xmin=121.39359681318699, xmax=121.59011649307465, ymin=25.03231432630051, ymax=25.194953355679843))))%>%
  add_osm_feature(key="highway")%>%
  osmdata_sf()
taipei_road=taipei_road$osm_lines
taipei_road_plot=filter(taipei_road, highway %in% c("motorway","trunk","primary","secondary","tertiary","motorway_link","trunk_link","primary_link","secondary_link","tertiary_link"))%>%
  select(osm_id, name, highway)


png("./tamsui_itinerary/Map.png", width=837*3, height=763*3, res=200)
ggplot()+
  geom_sf(data=riverpoly, fill="#97CBFF", color=NA)+
  geom_sf(data=taipei_road_plot, color="#BEBEBE", linewidth=0.1)+
  geom_sf(data=bike_ride, color="#01814A", linewidth=2, alpha=0.9)+
  geom_sf(data=taipei_mrt, aes(color=LineName), linewidth=0.8, show.legend=F, alpha=0.7)+
  scale_color_manual(values=c("板南線"="#0070BD", "松山新店線"="#008659", "淡水信義線"="#E3002C", "中和新蘆線"="#F8B61C", "文湖線"="#C48B30","環狀線"="yellow","淡海輕軌"="#EB6D5C"))+
  coord_sf(xlim=c(121.39359681318699, 121.59011649307465),
           ylim=c(25.03231432630051, 25.194953355679843))+
  theme_void()
dev.off()







