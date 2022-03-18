setwd("C:\\Users\\user\\Downloads\\ecology data\\Data-9-27-2021(edited)\\110年6月行政區人口統計_鄉鎮市區_SHP")

library(sf)
library(ggplot2)
library(data.table)

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

#within：檢查某個物件坐落在哪個區域裡面--------------------------------------------
check <- st_within(x=全聯, y=Town)
check
check <- as.data.frame(st_within(x=全聯, y=Town))
#用as.data.frame()變成dataframe，以利我們的分析
#row.id:全聯的case / col.id:Town的case

TownNames <- as.data.table(Town)[ , .(COUNTY, TOWN, COUNTY_ID, TOWN_ID)][]
TownNames

PxMart <- (as.data.table(全聯)[check$row.id, ]
			[ , names(TownNames):=TownNames[check$col.id, ]]
			[])
PxMart

#用dplyr的count()計算每個鄉鎮市區的全聯家數
dplyr::count(x=PxMart, COUNTY, TOWN, COUNTY_ID, TOWN_ID)

#以town作為分組依據，全聯作為計算目標，使用aggregate()進行統計計算
aa <- aggregate(x=全聯, by=Town, FUN=length) 
#agregate後的物件是向量，而計算元素的數量等同於計算向量的長度，因此可以使用length
aa

TownStore <- Town #建立一個新的shp變項，原本的就不動他

aa <- aggregate(x=全聯["店號"], by=TownStore, FUN=length) 
#在全聯檔案中，店號是"unique"的(每個case的店號都不一樣)，因此可以做為流水號的作用
aa

#因為他是以TownStore去做分組，因此aggregate後的case可以直接對應TownStore的case，在TownStore建立新的變項
TownStore$全聯家數 <- aggregate(x=全聯["店號"], by=TownStore, FUN=length)$店號
						#dplyr::mutate(全聯家數=ifelse(is.na(全聯家數), 0, 全聯家數))
TownStore
TownStore$全聯家數 <- ifelse(is.na(TownStore$全聯家數), 0, TownStore$全聯家數)

p0 <- ggplot()+
		geom_sf(data=全聯, color="orange")+
		geom_sf(data=Taipei.Town, fill=NA, color="forestgreen")
		coord_sf(ndiscr=0)+theme_void()
p0


#intersect----------------------------------------------------------------
Taipei.Town <- dplyr::filter(.data=Town, COUNTY %in% c("臺北市","新北市","基隆市"))

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

#Thematic Maps------------------------------------------------------------
setwd("C:\\Users\\user\\Downloads\\ecology data\\Data-10-18-2021-20211018")
library(readxl)
library(dplyr)
pop <- setDT(read_xlsx(path="109年12月行政區人口指標_鄉鎮市區.xlsx", sheet="109年12月行政區人口指標_鄉鎮市區"))

#以「"TOWN_ID"="鄉鎮市區代碼"」為對應，合併Town跟pop兩份資料
TownPOP <- left_join(x=Town, y=pop, by=c("TOWN_ID"="鄉鎮市區代碼"))
TownPOP

County <- st_cast(st_union(x=summarise(group_by(.data=Town,
					COUNTY, COUNTY_ID)),by_feature=TRUE))

p5 <- ggplot()+
		geom_sf(data=TownPOP, aes(fill=老化指數),color=NA, size=NA)+
		geom_sf(data=County, fill=NA, color="#CDEC5E", size=0.4)+
		scale_fill_viridis_c(option="A", direction=-1)+
		coord_sf(ndiscr=0)+theme_void()
p5

#畫圖囉~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(cowplot) #將多個圖合併起來的套件

County.island <- filter(.data=County, !(COUNTY_ID %in% c("09020", "09007", "10016")))
金門 <- filter(.data=TownPOP, COUNTY_ID %in% "09020" & !(TOWN_ID %in% c(09020060)))
連江 <- filter(.data=TownPOP, COUNTY_ID %in% "09007")
澎湖 <- filter(.data=TownPOP, COUNTY_ID %in% "10016")
台灣 <- filter(.data=TownPOP, !(COUNTY_ID %in% c("09020", "09007", "10016")) | (TOWN_ID %in% c(09020060)))

p0 <- ggplot()+
		geom_sf(data=台灣, aes(fill=老化指數),colour="transparent", size=0)+
		geom_sf(data=County.island, fill=NA, color="#83AFCD", size=0.1)+
		scale_fill_viridis_c(limits=c(36,649), option="A", na.value=NA, direction=-1)+
		coord_sf(ndiscr=0)+theme_void()+
		theme(legend.position=c(0, 0.2))

p1 <- ggplot()+
		geom_sf(data=連江, aes(fill=老化指數),colour="grey90", size=0.01)+
		scale_fill_viridis_c(limits=c(36,649), option="A", na.value=NA, direction=-1)+
		coord_sf(ndiscr=0)+theme_void()+
		theme(panel.border=element_rect(fill=NA, colour="tan"),legend.position="none")

p2 <- ggplot()+
		geom_sf(data=金門, aes(fill=老化指數),colour="grey90", size=0.01)+
		scale_fill_viridis_c(limits=c(36,649), option="A", na.value=NA, direction=-1)+
		coord_sf(ndiscr=0)+theme_void()+
		theme(panel.border=element_rect(fill=NA, colour="tan"),legend.position="none")

p3 <- ggplot()+
		geom_sf(data=澎湖, aes(fill=老化指數),colour="grey90", size=0.01)+
		scale_fill_viridis_c(limits=c(36,649), option="A", na.value=NA, direction=-1)+
		coord_sf(ndiscr=0)+theme_void()+
		theme(panel.border=element_rect(fill=NA, colour="tan"),legend.position="none")

#p1：連江 p2：金門 p3：澎湖
pa <- ggdraw()+
		draw_plot(plot=p0, x=0.5, y=0.5, width=1, height=1, hjust=0.5, vjust=0.5)+
		draw_plot(plot=p1, x=0.15, y=0.85, width=0.2, height=0.2, hjust=0.5, vjust=0.5)+
		draw_plot(plot=p2, x=0.15, y=0.65, width=0.2, height=0.2, hjust=0.5, vjust=0.5)+
		draw_plot(plot=p3, x=0.15, y=0.45, width=0.2, height=0.2, hjust=0.5, vjust=0.5)

library(svglite) #將圖輸出成svg檔
setwd("C:\\Users\\user\\Downloads")
svglite(filename="Taiwan_old.svg", width=9, height=7) #不能用中文名稱
print(pa)
dev.off() #離開svg輸出模式(記得做這步驟)