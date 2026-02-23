# 安裝TDX套件
devtools::install_github("ChiaJung-Yeh/NYCU_TDX")

# 載入TDX套件
library(TDX)

# 前30分鐘免費：2024/02/28
# 以施行日期前後各半年為例
all_mon=unique(substr(as.character(as.Date(as.Date("2023-09-01"):as.Date("2024-08-31"))), 1, 7))

# 下載臺北市YouBike租借票證資料
?Bike_OD_His # 查看函式設定

scd=data.frame()
for(i in all_mon){
  temp=Bike_OD_His(bikesys=2, time=i)
  scd=rbind(scd, temp)
}


# 介接YouBike站點資料[以下為範例API]
access_token=get_token(client_id="robert1328.mg10-cee19598-bae6-418d",
                       client_secret="7cbc55fa-1ce8-443d-b23c-1ed175f72aa0")
bike_station=Bike_Station(access_token, "Taipei", dtype="sf")


# 修正票證資料站點名稱以對齊站點資料
for(i in c("RentStation","ReturnStation")){
  scd=mutate(scd, !!sym(i):=case_when(
    !!sym(i)=="南京林森路口(東南側)" ~ "南京林森路口",
    !!sym(i)=="糖?文化園區" ~ "糖廍文化園區",
    !!sym(i)=="水源路11-1號旁" ~ "水源路11_1號旁",
    !!sym(i)=="?公公園" ~ "瑠公公園",
    !!sym(i)=="永安藝文館-表演36房" ~ "永安藝文館_表演36房",
    !!sym(i)=="崇仰公園(公?路255巷)" ~ "崇仰公園(公舘路255巷)",
    !!sym(i)=="公?承德路口" ~ "公舘承德路口",
    TRUE ~ !!sym(i)
  ))
}



