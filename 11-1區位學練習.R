setwd("C:\\Users\\user\\Downloads\\ecology data\\Data-9-27-2021(edited)\\110年6月行政區人口統計_鄉鎮市區_SHP")

library(sf)
library(ggplot2)

Town <- st_read(dsn="標準鄉鎮市區.shp", options="ENCODING=UTF-8",
				stringsAsFactors=FALSE, crs=3826)


str(Town)

plot(st_geometry(Town), border="#C512D1")

library(tmap)
qtm(Town)

#dissolve--------------------------------------------------

library(dplyr)
County <- st_cast(st_union(x=summarise(group_by(.data=Town,
					COUNTY, COUNTY_ID)),by_feature=TRUE))
#group_by：依照COUNTY和COUNTY_ID兩個變項分類
#st_union：「聯集」合併 (把一個縣市內的鄉鎮市區合併成一個整體的縣市)

County
plot(County)

p2 <- ggplot()+
		geom_sf(data=Town, color="forestgreen", fill=NA, size=0.01)+
		geom_sf(data=County, aes(color=COUNTY), fill=NA, size=0.5)+
		coord_sf(ndiscr=0)+theme_void()
p2
#第一層：建立一層空白畫布
#第二層：鄉鎮市區，邊界顏色=forestgreen，內部不填滿顏色，邊界大小0.01
#第三層：剛剛合併出來的縣市，依照COUNTY變項給予不同的邊界顏色，
#內部不填滿顏色，邊界大小0.5
#第四層：取消地圖投影變化
#第五層：取消灰底佈景


#地理中心-----------------------------------------------
Taipei.Town <- filter(.data=Town, COUNTY=="臺北市")
Taipei.Center <- st_centroid(Taipei.Town)

Taipei.Center

p3 <- ggplot()+
		geom_sf(data=Taipei.Town, color="#2935D0", fill=NA)+
		geom_sf(data=Taipei.Center, color="#F93F19", size=5)+
		coord_sf(ndiscr=0)+theme_void()
p3


#剛剛是找臺北市各鄉鎮市區的中心，現在找臺北市本身的中心
Taipei.County <- st_cast(st_union(x=summarise(group_by(.data=
					Taipei.Town)),by_feature=TRUE))
#group_by若不寫任何變項條件，則代表直接把所有資料合併成一筆資料

Taipei.Center.One <- st_centroid(Taipei.County)

p4 <- ggplot()+
		geom_sf(data=Taipei.Town, color="#2935D0", fill=NA)+
		geom_sf(data=Taipei.Center.One, color="#F93F19", size=5)+
		coord_sf(ndiscr=0)+theme_void()
p4

#計算距離----------------------------------------------------
library(data.table)

Town.Center <- st_centroid(Town)

distances <- st_distance(x=Town.Center)
str(distances)
head(distances)

Town.Distance <- as.data.table(units::drop_units(st_distance(x=Town.Center)))
Town.Distance

IDs <- Town.Center$TOWN_ID

names(Town.Distance) <- IDs
Town.Distance <- Town.Distance[ , From:=IDs][]
head(Town.Distance)

#計算面積----------------------------------------------------
areas <- st_area(Town)
areas
Town$土地面積 <- units::drop_units(st_area(x=Town))

#將「全聯」資料變成sf--------------------------------------------------------
全聯 <- readxl::read_excel(path="全聯.xlsx", sheet="全聯")
全聯

st_as_sf(x=全聯, coords=c("經度","緯度"),crs=4326)

全聯 <- st_transform(x=st_as_sf(x=全聯, coords=c("經度","緯度"),crs=4326), crs=3826)
#先把原始資料變成sf資料(座標是經度緯度，所以crs是4326)，再用st_transform把crs變成3826
全聯

p5 <- ggplot()+
		geom_sf(data=Town, color="#2935D0", fill=NA)+
		geom_sf(data=全聯, color="#EEAA40", size=1)+
		coord_sf(ndiscr=0)+theme_void()
p5

#作業------------------------------------------------------------------
#1.算台北市各鄉鎮市區的地理中心之間的「平均距離」
#2.算各個縣市的全聯據點之間的「平均距離」
#---------------------------------------------------------------------
Town <- st_read(dsn="標準鄉鎮市區.shp", options="ENCODING=UTF-8",
				stringsAsFactors=FALSE, crs=3826)
全聯 <- readxl::read_excel(path="全聯.xlsx", sheet="全聯")
全聯

st_as_sf(x=全聯, coords=c("經度","緯度"),crs=4326)

全聯 <- st_transform(x=st_as_sf(x=全聯, coords=c("經度","緯度"),crs=4326), crs=3826)
#先把原始資料變成sf資料(座標是經度緯度，所以crs是4326)，再用st_transform把crs變成3826
全聯

p5 <- ggplot()+
		geom_sf(data=Town, color="#2935D0", fill=NA)+
		geom_sf(data=全聯, color="#EEAA40", size=1)+
		coord_sf(ndiscr=0)+theme_void()
p5


