setwd("C:\\Users\\user\\Downloads\\ecology data\\Data-9-27-2021(edited)\\110年6月行政區人口統計_鄉鎮市區_SHP")

library(sf)
library(ggplot2)
library(data.table)
library(dplyr)

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

Taipei.Town <- dplyr::filter(.data=Town, COUNTY %in% c("臺北市","新北市"))

aa <- as.data.frame(st_intersects(x=全聯, y=Taipei.Town))
aa
Marts <- 全聯[aa$row.id, ]
Taipei.Town.names <- as.data.table(Taipei.Town)[ , geometry:=NULL]
Marts <- (as.data.table(Marts)
			[ , names(Taipei.Town.names):=Taipei.Town.names[check$col.id, ]][])

Marts <- st_as_sf(Marts)
p1 <- ggplot()+
		geom_sf(data=Marts, color="orange")+
		geom_sf(data=Taipei.Town, fill=NA, color="forestgreen")+
		coord_sf(ndiscr=0)+theme_void()
p1

#熱點圖-------------------------------------------------------------------------

#篩選出台北市，並把所有鄉鎮市區合併成一個大的雙北地圖(變成一個雙北的外框)
Taipei.Town <- filter(.data=Town, COUNTY %in% c("臺北市","新北市"))
Taipei <- st_cast(st_union(x=summarise(group_by(.data=Taipei.Town))))
plot(Taipei)

#畫格子(2000代表2000公尺)
grids <- st_make_grid(x=Taipei, cellsize=2000, what="polygons")
plot(grids)

#為格子加上編號
grids <- st_as_sf(data.table(ID=1:length(grids), geometry=grids))

#將格子圖與雙北圖合併起來
Taipei.grids <- st_intersection(x=grids, y=Taipei)
plot(Taipei.grids)

#計算雙北每個鄉鎮市區的全聯家數(具體註解可見「11-8區位學練習」-within區塊)
Taipei.grids$全聯家數 <- aggregate(x=全聯["店號"], by=Taipei.grids, FUN=length)$店號
Taipei.grids
Taipei.grids <- filter(.data=Taipei.grids, !is.na(全聯家數))

#上面是直接拿掉NA，也可以像下一行一樣把NA設成0，兩者差別在於畫圖時會不會被畫出來
#Taipei.grids$全聯家數 <- ifelse(is.na(Taipei.grids$全聯家數), 0, Taipei.grids$全聯家數)

#最後畫圖
#第一層：雙北鄉鎮市區 / 第二層：雙北方格圖(以各個格子的全聯家數做面量作圖) / 第三層：為各鄉鎮市區加上名字 / ...
#第二層的"alpha"參數代表「透明度」，1代表完全不透明，0代表完全透明
p0 <- ggplot()+
		geom_sf(data=Taipei.Town, fill=NA, color="forestgreen")+
		geom_sf(data=Taipei.grids, aes(fill=全聯家數), alpha=0.9)+
		geom_sf_text(data=Taipei.Town, aes(label=TOWN), size=2.5, color="blue")+
		scale_fill_viridis_c(direction=-1, option="A")+
		coord_sf(ndiscr=0)+theme_void()
p0

#隔離指數---------------------------------------------------------------------------
#公式：0.5 * sum{i}(P_A - P_B)，i代表各個區域

#讀入資料
setwd("C:\\Users\\user\\Downloads\\ecology data\\Data-11-1-2021-20211101")
ID <- readxl::read_excel(path="2020年V-ID.xlsx")
indie <- fread(input="2020年村里現住原住民人口按性別、身分、原住民族別分.csv", 
				encoding="UTF-8", colClasses="character", skip=1)
all <- fread(input="2020年村里戶數人口數單一年齡人口數.csv", 
				encoding="UTF-8", colClasses="character", skip=1)
str(indie)
str(all)

indie <- indie[ , 1:5]
all <- all[ , 1:6]

POP <- merge(x=indie, y=all, by=intersect(names(indie) , names(all)), all.x=TRUE)
names(POP)[5] <- "indie_pop"
POP <- (merge(x=POP, y=ID, by="區域別代碼", all.x=TRUE)
		[ , .(COUNTY, TOWN, VILLAGE, COUNTY_ID, TOWN_ID, V_ID, indie_pop, 人口數)][])
POP
#all.x的意思：兩個資料進行merge的時候，主要保留x資料，當x資料超出y資料的case數量時，x資料超出的部分並不會被刪掉
#相反地，若y資料超出x資料的case數量時，y資料超出的部分會被刪掉
#all.y即是反過來，而all就是兩個合起來



#計算隔離指數
Index <- (copy(POP)
			[ , ":="(人口數=as.numeric(人口數), indie_pop=as.numeric(indie_pop))]
			[ , ":="(非原住民=人口數 - indie_pop)]
			[ , ":="(pa=非原住民/sum(非原住民), 
						pb=indie_pop/sum(indie_pop)),
				by=.(COUNTY, TOWN, COUNTY_ID, TOWN_ID)]
			[ , .(Index=0.5 * sum(abs(pa - pb))),
				by=.(COUNTY, TOWN, COUNTY_ID, TOWN_ID)]
			[])
Index


#畫圖
County <- st_cast(st_union(x=summarise(group_by(.data=Town,
					COUNTY, COUNTY_ID)),by_feature=TRUE))

Town.Segregation.Index <- left_join(x=Town, y=Index) 

p1 <- ggplot()+
		geom_sf(data=Town.Segregation.Index, aes(fill=Index), color=NA, size=NA)+
		geom_sf(data=County, fill=NA, color="#448DAA", size=0.3)+
		scale_fill_viridis_c(direction=-1, option="C", na.value=NA, name="隔離指數")+
		coord_sf(ndiscr=0)+theme_void()+
		theme(legend.position=c(0.2, 0.25),
				legend.key.height=unit(0.8, "cm"),
				legend.spacing.x=unit(0.5, "cm"),
				plot.title=element_text(size=15))+
		labs(title="全台各鄉鎮市區隔離指數面量圖")
p1
