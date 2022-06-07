library(ggplot2)
library(dplyr)
library(sf)
library(osmdata)
library(osmar)
library(xml2)
library(tmap)
library(TWspdata)
library(stringr)
library(reshape2)
library(data.table)

tmap_mode("view")

taipei=filter(taiwan_town, COUNTYNAME=="臺北市")

x=opq(bbox=st_bbox(taipei))%>%
  add_osm_feature(key="shop")%>%
  osmdata_xml()



#---Only POINT---#
geo_df=do.call(rbind.data.frame, xml_attrs(xml_find_all(x, "//node")))
colnames(geo_df)=c("id","lat","lon")


attr_df=do.call(rbind.data.frame, xml_attrs(xml_find_all(x, "//node//tag")))
colnames(attr_df)=c("k","v")

rep_time=xml_length(xml_find_all(x, "//node"))

res_point=cbind(geo_df[rep(c(1:nrow(geo_df)), times=rep_time),], attr_df)

res_point=reshape2::dcast(res_point, id ~ k, value.var="v")%>%
  left_join(geo_df)%>%
  select(id, name, shop, lat, lon)%>%
  mutate(geometry=st_as_sfc(paste0("POINT(", lon, " ", lat, ")")))%>%
  st_sf(crs=4326)%>%
  st_transform(crs=3826)



#---WAY---#
way_id=do.call(rbind.data.frame, xml_attrs(xml_find_all(x, "//way")))
way_nd=do.call(rbind.data.frame, xml_attrs(xml_find_all(x, "//nd")))
way_tag=do.call(rbind.data.frame, xml_attrs(xml_find_all(x, "//way//tag")))
colnames(way_tag)=c("k","v")

res_way=cbind(way_id[rep(c(1:nrow(way_id)), times=str_count(as.character(xml_find_all(x, "//way")), "<nd")),], way_nd)
colnames(res_way)=c("way_id", "way_nd")
res_way=left_join(res_way, geo_df, by=c("way_nd"="id"))%>%
  distinct()

res_way$way_id=factor(res_way$way_id, levels=unique(res_way$way_id))

res_way=group_by(res_way, way_id)%>%
  summarise(lat=mean(as.numeric(lat)), lon=mean(as.numeric(lon)))

rep_time=str_count(as.character(xml_find_all(x, "//way")), "<tag")

res_way=cbind(res_way[rep(c(1:nrow(res_way)), times=rep_time),], way_tag)

res_way=reshape2::dcast(res_way, way_id+lat+lon ~ k, value.var="v")%>%
  rename(id=way_id)%>%
  select(id, name, shop, lat, lon)%>%
  mutate(geometry=st_as_sfc(paste0("POINT(", lon, " ", lat, ")")))%>%
  st_sf(crs=4326)%>%
  st_transform(crs=3826)

# 部分way裡面沒有任何tag，但在relation內有，故先儲存之
res_way_noid=cbind(way_id[rep(c(1:nrow(way_id)), times=str_count(as.character(xml_find_all(x, "//way")), "<nd")),], way_nd)
colnames(res_way_noid)=c("way_id", "way_nd")
res_way_noid=left_join(res_way_noid, geo_df, by=c("way_nd"="id"))%>%
  distinct()%>%
  filter(!(way_id %in% res_way$id))%>%
  rename(id=way_id)%>%
  group_by(id)%>%
  summarise(lat=mean(as.numeric(lat)), lon=mean(as.numeric(lon)))%>%
  mutate(geometry=st_as_sfc(paste0("POINT(", lon, " ", lat, ")")))%>%
  st_sf(crs=4326)%>%
  st_transform(crs=3826)

res_way=bind_rows(res_way, res_way_noid)




#---RELATION---#
relation_id=do.call(rbind.data.frame, xml_attrs(xml_find_all(x, "//relation")))
rep_time=str_count(as.character(xml_find_all(x, "//relation")), "<member")
res_relation=cbind(relation_id[rep(c(1:nrow(relation_id)), times=rep_time),], do.call(rbind.data.frame, xml_attrs(xml_find_all(x, "//relation//member"))))
colnames(res_relation)=c("relation_id", "relation_way", "way_id", "role")

res_relation$relation_id=factor(res_relation$relation_id, levels=unique(res_relation$relation_id))

res_relation=left_join(res_relation, st_drop_geometry(res_way)[, c("id","lat","lon")], by=c("way_id"="id"))%>%
  group_by(relation_id)%>%
  summarise(lat=mean(lat, na.rm=T),
            lon=mean(lon, na.rm=T))

rep_time=str_count(as.character(xml_find_all(x, "//relation")), "<tag")
relation_tag=do.call(rbind.data.frame, xml_attrs(xml_find_all(x, "//relation//tag")))
colnames(relation_tag)=c("k","v")


res_relation=cbind(res_relation[rep(c(1:nrow(res_relation)), times=rep_time),], relation_tag)
res_relation=reshape2::dcast(res_relation, relation_id+lat+lon ~ k, value.var="v")%>%
  rename(id=relation_id)%>%
  select(id, name, shop, lat, lon)%>%
  filter(!(is.na(lat)))%>%
  mutate(geometry=st_as_sfc(paste0("POINT(", lon, " ", lat, ")")))%>%
  st_sf(crs=4326)%>%
  st_transform(crs=3826)



#---MAP---#
poi=rbind(res_point, res_way, res_relation)

poi=res_point

poi=filter(poi, !is.na(name))

tm_shape(poi)+
  tm_dots()



