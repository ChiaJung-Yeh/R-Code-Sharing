library(dplyr)
library(sf)
library(tmap)
library(leaflet)
library(leaflet.providers)
library(leafpop)

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
  select(-Location, -lon, -lat, -Path)


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



