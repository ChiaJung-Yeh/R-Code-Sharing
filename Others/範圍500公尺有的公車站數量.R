library(TWspdata)
library(sf)
library(dplyr)

# 擷取新竹市村里圖資
hsinchu=filter(taiwan_village, COUNTYNAME=="新竹市")%>%
  st_transform(crs=3826)

# 擷取新竹市各村里中心點
hsinchu_center=st_centroid(hsinchu)

# 將中心點取500公尺環域
vil_buf=st_buffer(hsinchu_center, 500)

# 將村里中心500公尺環域與新竹市公車站牌取交集
vil_stop=st_intersection(vil_buf, hs_bus_stop)

# 依據村里名稱，統計各村里的公車站牌數
vil_stop=group_by(st_drop_geometry(hsinchu_vil_stop), TOWNNAME, VILLNAME)%>%
  summarise(count=n())




#---根據指定經緯度的範圍500公尺收集---#
# 指定經緯度 (經度放錢，緯度放後)
lonlat=c(120.97153962847077, 24.8019562213021)

lonlat=st_sfc(st_point(lonlat))%>%
  st_sf(crs=4326)%>%
  st_transform(crs=3826)

# 將指定經緯度取500公尺環域
lonlat_buf=st_buffer(lonlat, 500)

# 將指定經緯度500公尺環域與新竹市公車站牌取交集
lonlat_stop=st_intersection(lonlat_buf, hs_bus_stop)

# 統計該指定經緯度500公尺範圍內的公車站牌數
nrow(lonlat_stop)


