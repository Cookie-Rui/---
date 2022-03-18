library(patchwork)
library(dplyr)
library(readr)
library(sf)
library(ggplot2)
library(data.table)
library(readxl)
library(NbClust)

setwd("C:\\Users\\user\\Downloads\\ecology data\\Data-1-3-2022-20220103")
Town <- st_read(dsn="標準鄉鎮市區.shp", options="ENCODING=UTF-8",
				stringsAsFactors=FALSE, crs=3826)

Regions <- filter(.data=Town, COUNTY %in% c("臺北市", "新北市", "基隆市"))
County <- st_cast(st_union(x=summarise(group_by(.data=Regions,
					COUNTY, COUNTY_ID)),by_feature=TRUE))

p1 <- ggplot()+
		geom_sf(data=Regions, color="grey90", fill=NA, size=0.1)+
		geom_sf(data=County, color="orchid", fill=NA, size=0.8)+
		geom_sf_text(data=Regions, aes(label=TOWN), size=4, color="blue")+
		coord_sf(ndiscr=0)+theme_void()+
		theme(legend.position=c(0.8,0.2))


#綜合所得稅--------------------------------------------------------------------------------
#R接下來的更新會改變編碼方式，對於讀檔方面會有所不便
#因此之後建議使用應用範圍更廣的read_delim()(要加載readr套件)
income <- read_delim(file="108年綜合所得稅所得總額申報統計_鄉鎮市區.csv", skip=1,
					col_types=cols(.default=col_character()), locale=locale(encoding="BIG-5"))
names(income) <- enc2native(names(income))
names(income)[2:5] <- c("COUNTY_ID", "COUNTY", "TOWN_ID", "TOWN")
setDT(income)
income <- income[ , .(TOWN_ID, 所得=as.numeric(中位數))][]

Regions <- left_join(x=Regions, y=income) 
#合併具有sf屬性的資料時建議使用dplyr的left_join，不然用merge有時會錯誤去除資料的sf屬性

p2 <- ggplot()+
		geom_sf(data=Regions, color="grey90", aes(fill=所得), size=0.01)+
		geom_sf(data=County, color="orchid", fill=NA, size=0.8)+
		geom_sf_text(data=Regions, aes(label=TOWN), size=3, color="blue")+
		scale_fill_viridis_c(direction=-1)+
		coord_sf(ndiscr=0)+theme_void()+
		theme(legend.position=c(0.2,0.8))

#工商家數----------------------------------------------------------------------------------
事業單位 <- setDT(read_xlsx(path="109年12月行政區工商家數_鄉鎮市區.xlsx", col_types="text"))
#col_types參數其實就等於fread()的colClasses參數，這裡一樣設定變項名稱為文字類型

names(事業單位) <- enc2native(names(事業單位))
names(事業單位)[1:4] <- c("COUNTY_ID", "COUNTY", "TOWN_ID", "TOWN")
工 <- names(事業單位)[8:11]
商 <- names(事業單位)[12:17]
事業單位 <- (事業單位
			[ , (工):=lapply(.SD, as.numeric), .SDcols=工]
			[ , (商):=lapply(.SD, as.numeric), .SDcols=商]
			[ , 工業家數:=rowSums(.SD), .SDcols=工]
			[ , 商業家數:=rowSums(.SD), .SDcols=商]
			[ , .(TOWN_ID, 工業家數, 商業家數)][])

Regions <- left_join(x=Regions, y=事業單位) 

Regions <- mutate(.data=Regions,
					土地面積=units::drop_units(st_area(Regions))/(1000^2),
					工業=工業家數/土地面積,
					商業=商業家數/土地面積)

p3 <- ggplot()+
		geom_sf(data=Regions, color="grey90", aes(fill=工業), size=0.01)+
		geom_sf(data=County, color="orchid", fill=NA, size=0.8)+
		geom_sf_text(data=Regions, aes(label=TOWN), size=3, color="blue")+
		scale_fill_viridis_c(direction=-1)+
		coord_sf(ndiscr=0)+theme_void()+
		theme(legend.position=c(0.2,0.8))

p4 <- ggplot()+
		geom_sf(data=Regions, color="grey90", aes(fill=商業), size=0.01)+
		geom_sf(data=County, color="orchid", fill=NA, size=0.8)+
		geom_sf_text(data=Regions, aes(label=TOWN), size=3, color="blue")+
		scale_fill_viridis_c(direction=-1)+
		coord_sf(ndiscr=0)+theme_void()+
		theme(legend.position=c(0.2,0.8))

p3|p4

#2010 Census---------------------------------------------------------------------
AreaID <- setDT(read_xlsx(path="Census-2010-TOWN_ID.xlsx", sheet="鄉鎮市區"))
load(file="Township_Population.RData")
POP <- merge(x=Township_Population, y=AreaID, by.x="T400", by.y="CENSUS2010", all.x=TRUE)
pop <- (POP
		[ , .(閩南=sum((C082=="1")*EX3)/sum(EX3)*100,
				客家=sum((C083=="1")*EX3)/sum(EX3)*100,
				外來人口=sum((C100 %in% c("5","6"))*EX3)/sum(EX3)*100,
				老年人口=sum((C020=="07")*EX3)/sum(EX3)*100),
		by=.(TOWN_ID)]
		[])

Regions <- left_join(x=Regions, y=pop) 

p5 <- ggplot()+
		geom_sf(data=Regions, color="grey90", aes(fill=閩南), size=0.01)+
		geom_sf(data=County, color="orchid", fill=NA, size=0.8)+
		geom_sf_text(data=Regions, aes(label=TOWN), size=3, color="blue")+
		scale_fill_viridis_c(direction=-1)+
		coord_sf(ndiscr=0)+theme_void()+
		theme(legend.position=c(0.2,0.8))

p6 <- ggplot()+
		geom_sf(data=Regions, color="grey90", aes(fill=客家), size=0.01)+
		geom_sf(data=County, color="orchid", fill=NA, size=0.8)+
		geom_sf_text(data=Regions, aes(label=TOWN), size=3, color="blue")+
		scale_fill_viridis_c(direction=-1)+
		coord_sf(ndiscr=0)+theme_void()+
		theme(legend.position=c(0.2,0.8))

p7 <- ggplot()+
		geom_sf(data=Regions, color="grey90", aes(fill=外來人口), size=0.01)+
		geom_sf(data=County, color="orchid", fill=NA, size=0.8)+
		geom_sf_text(data=Regions, aes(label=TOWN), size=3, color="blue")+
		scale_fill_viridis_c(direction=-1)+
		coord_sf(ndiscr=0)+theme_void()+
		theme(legend.position=c(0.2,0.8))

p8 <- ggplot()+
		geom_sf(data=Regions, color="grey90", aes(fill=老年人口), size=0.01)+
		geom_sf(data=County, color="orchid", fill=NA, size=0.8)+
		geom_sf_text(data=Regions, aes(label=TOWN), size=3, color="blue")+
		scale_fill_viridis_c(direction=-1)+
		coord_sf(ndiscr=0)+theme_void()+
		theme(legend.position=c(0.2,0.8))

(p5|p6)/(p7|p8)

load(file="Township_Housings.RData")
Housings <- merge(x=Township_Housings, y=AreaID, by.x="T400", by.y="CENSUS2010", all.x=TRUE)
Housings <- (Housings
			[ , .(房屋構造=sum((A050=="2")*EX1)/sum(EX1)*100,
					平房=sum((A060=="1")*EX1)/sum(EX1)*100,
					高樓=sum((A060=="4")*EX1)/sum(EX1)*100),
			by=.(TOWN_ID)]
			[])
Regions <- left_join(x=Regions, y=Housings) 

summary(Housings)

p9 <- ggplot()+
		geom_sf(data=Regions, color="grey90", aes(fill=房屋構造), size=0.01)+
		geom_sf(data=County, color="orchid", fill=NA, size=0.8)+
		geom_sf_text(data=Regions, aes(label=TOWN), size=3, color="blue")+
		scale_fill_viridis_c(direction=-1)+
		coord_sf(ndiscr=0)+theme_void()+
		theme(legend.position=c(0.2,0.8))

p10 <- ggplot()+
		geom_sf(data=Regions, color="grey90", aes(fill=平房), size=0.01)+
		geom_sf(data=County, color="orchid", fill=NA, size=0.8)+
		geom_sf_text(data=Regions, aes(label=TOWN), size=3, color="blue")+
		scale_fill_viridis_c(direction=-1)+
		coord_sf(ndiscr=0)+theme_void()+
		theme(legend.position=c(0.2,0.8))

p11 <- ggplot()+
		geom_sf(data=Regions, color="grey90", aes(fill=高樓), size=0.01)+
		geom_sf(data=County, color="orchid", fill=NA, size=0.8)+
		geom_sf_text(data=Regions, aes(label=TOWN), size=3, color="blue")+
		scale_fill_viridis_c(direction=-1)+
		coord_sf(ndiscr=0)+theme_void()+
		theme(legend.position=c(0.2,0.8))

#K-means Cluster Analysis--------------------------------------------------------------
target <- as.data.table(Regions)
attributes <- scale(target[ , .(所得,工業,閩南,客家,外來人口,老年人口,房屋構造,高樓)])
nc <- NbClust(attributes, min.nc=2, max.nc=15, method="kmeans")
table(nc$Best.n[1, ])
barplot(table(nc$Best.n[1, ]), xlab="number of clusters", ylab="number of criteria")
fit.km <- kmeans(x=attributes, center=5, nstart=25)
target$Cluster <- fit.km$cluster
Type <- target[ , .(COUNTY, TOWN, Cluster)][order(Cluster), ]
print(Type, nrow(Type), justify="left")
par(mfrow=c(1,1))
target <- st_as_sf(target)

pc <- ggplot()+
		geom_sf(data=target, aes(fill=as.factor(Cluster)), size=0.01, color="grey90")+
		geom_sf(data=County, color="orchid", fill=NA, size=0.8)+
		geom_sf_text(data=target, aes(label=TOWN, color=as.factor(Cluster)), size=3)+
		scale_fill_viridis_d(option="D", direction=-1, alpha=0.3)+
		scale_color_viridis_d(option="A")+
		coord_sf(ndiscr=0)+theme_void()+
		theme(legend.position="none")


####彰化在70年前是全台最多人口的地區，我們可以把竹竹苗改成彰化縣來看看彰化的人口狀況(或任何你愛的縣市♪(^∇^*))####