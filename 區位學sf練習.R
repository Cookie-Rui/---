library(sf)

setwd("C:\\Users\\user\\Downloads\\Data-9-27-2021-20210927\\110年6月行政區人口統計_鄉鎮市區_SHP")

Town <- st_read(dsn="標準鄉鎮市區.shp", options="ENCODING=UTF-8",
				stringsAsFactors=FALSE, crs=3826)

str(Town)

Town

#如果不指定變項
plot(Town) #每個變項就都會跑出地圖，在這裡是5張圖

#指定變項
plot(st_geometry(Town), border="blue")
plot(Town$geometry, border="blue")
#兩者意思一樣

#快速畫法
library(tmap)
qtm(Town)

#複雜畫法
Maps <- tm_shape(shp=Town)+tm_borders(col="yellowgreen", lwd=0.05) +
		tm_layout(frame=FALSE, outer.margins=c(0,0,0,0))
Maps

library(dplyr)
雙北 <- filter(.data=Town, COUNTY %in% c("臺北市", "新北市"))
Maps <- tm_shape(shp=雙北)+tm_borders(col="yellowgreen", lwd=0.05) +
        tm_layout(frame=FALSE, outer.margins=c(0,0,0,0))
Maps

library(ggplot2)
p0 <- ggplot()+
		geom_sf(data=雙北)
p0

p1 <- ggplot()+
		geom_sf(data=雙北, color="forestgreen", fill=NA, size=0.01)+
		coord_sf(ndiscr=0)+theme_void() #coord_sf(ndiscr=0):不做投影處理
p1

