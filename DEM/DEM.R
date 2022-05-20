# 下載處理DEM網格及繪製地圖之套件
install.packages(c("raster","ggplot2","tmap","sf","tmaptools"))

# 下載臺灣相關圖資
install.packages("devtools")
devtools::install_github("ChiaJung-Yeh/TWspdata")

# 載入套件
library(raster)
library(ggplot2)
library(tmap)
library(sf)
library(TWspdata)
library(tmaptools)

# 自以下連結下載20公尺網格數值地形模型資料(DEM)，並讀取該資料
# http://dtm.moi.gov.tw/不分幅_全台及澎湖.zip
dem=raster("./Raster Data/dem_20m.tif")

# 以內湖區為例
town=dplyr::filter(taiwan_town, TOWNNAME %in% c("內湖區"))%>%
  st_transform(crs=3826)

temp=raster::intersect(dem, extent(st_bbox(town)[c(1,3,2,4)]))

# 繪製地圖
tm_shape(temp)+
  tm_raster(style="cont", midpoint=NA, palette=get_brewer_pal("-RdYlGn", n=7, plot=FALSE))+
  tm_shape(town)+
  tm_borders()

# 擷取內湖區每20公尺網格中心點的經緯度與高度
temp_XY=coordinates(temp)
temp_XY=cbind(temp_XY, elev=extract(temp, temp_XY))

# 查看擷取成果
# 第一行為X；第二行為Y；第三行為高程p_XY
temp_XY

