library(sf)
library(dplyr)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(readxl)
library(stringr)
library(lubridate)

setwd("C:\\Users\\user\\Downloads\\Data-12-20-2021-20211220")

Stations <- st_read(dsn="臺北捷運車站.shp", options="ENCODING=BIG-5",
					stringsAsFactors=FALSE, crs=3826)

names(Stations) <- enc2native(names(Stations)) #養成改編碼的好習慣

Routes <- st_read(dsn="臺北捷運路線.shp", options="ENCODING=BIG-5",
					stringsAsFactors=FALSE, crs=3826)

p0 <- ggplot()+
	geom_sf(data=Routes, color="steelblue", size=0.8)+
	geom_sf(data=Stations, shape=22, color="darkgoldenrod", bg="limegreen", size=5, alpha=0.6)+
	geom_sf_text(data=Stations, aes(label=站名), color="tomato", size=3,
				position=position_nudge(x=-500, y=500))+
	theme_void() + coord_sf(ndiscr=0)+
	scale_x_continuous(expand=expansion(mult=c(0.01, 0.02)))
p0
#position_nudge：調整文字標籤的位置，(0,0)代表站名會坐落在剛好車站(點)的位置，(-500,500)即代表往左500單位，往上500單位
#expand=expansion(mult=c(0.01, 0.02))：調整圖表上下左右的留白比例，mult參數愈大留白愈多，反之與少


Town <- st_read(dsn="標準鄉鎮市區.shp", options="ENCODING=UTF-8",
				stringsAsFactors=FALSE, crs=3826)
Taipei.Town <- filter(.data=Town, COUNTY %in% c("臺北市","新北市"))
p1 <- ggplot()+
	geom_sf(data=Taipei.Town, color="black", fill=NA, size=1)+
	geom_sf(data=Routes, color="steelblue", size=0.8)+
	geom_sf(data=Stations, shape=22, color="darkgoldenrod", bg="limegreen", size=5, alpha=0.6)+
	geom_sf_text(data=Stations, aes(label=站名), color="tomato", size=3,
				position=position_nudge(x=-500, y=500))+
	theme_void() + coord_sf(ndiscr=0)+
	scale_x_continuous(expand=expansion(mult=c(0.01, 0.02)))
p1

Taipei.Town <- dplyr::filter(.data=Town, COUNTY %in% c("臺北市","新北市","基隆市"))

# aa <- as.data.frame(st_intersects(x=Routes, y=Taipei.Town))
# aa
#  <- Routes[aa$row.id, ]
# Taipei.Town.names <- as.data.table(Taipei.Town)[ , geometry:=NULL]
# Marts <- (as.data.table(Marts)
# 			[ , names(Taipei.Town.names):=Taipei.Town.names[check$col.id, ]][])

# Marts <- st_as_sf(Marts)
# p1 <- ggplot()+
# 		geom_sf(data=Marts, color="orange")+
# 		geom_sf(data=Taipei.Town, fill=NA, color="forestgreen")+
# 		coord_sf(ndiscr=0)+theme_void()
# p1


#捷運進出站資料與畫圖----------------------------------------------------------------
NB <- setDT(read_excel(path="站間起迄站數.xlsx", sheet="起迄站數"))
setnames(x=NB, old="起迄", new="進站")
OD <- fread(input="臺北捷運每日分時各站OD流量統計資料_202110.csv",
			encoding="UTF-8",colClasses="character")
OD

NB <- melt(data=NB, id.vars="進站", measure.vars=names(NB)[-1],
    variable.name = "出站", value.name = "站數", variable.factor = FALSE, value.factor = FALSE)
str(NB)
summary(NB)
str(OD)
str(Stops)
summary(Stops$站數)

OD <- (OD[ , 日期:=as.Date(fast_strptime(日期, "%Y-%m-%d"))]
		[ , ":="(進站=str_replace_all(string=進站, 
										pattern=c("大橋頭站"="大橋頭", "台"="臺")),
				出站=str_replace_all(string=出站, 
										pattern=c("大橋頭站"="大橋頭", "台"="臺")))]
		[])


#計算從某站進/出站的人平均會搭乘捷運經過幾站
Stops <- merge(x=OD, y=NB, by=intersect(names(OD), names(NB)), all.x=TRUE)
names(Stops) <- enc2native(names(Stops))
Stops$人次 <- as.numeric(Stops$人次)
IN <- (Stops[ , .(In.平均站數=sum(人次*站數)/sum(人次)), by=.(進站)][ , .(站名=進站,In.平均站數)])
OUT <- (Stops[ , .(Out.平均站數=sum(人次*站數)/sum(人次)), by=.(出站)][ , .(站名=出站,Out.平均站數)])

Result <- merge(x=IN, y=OUT, by=intersect(names(IN), names(OUT)))
new.stations <- merge(x=Stations, y=Result, by=intersect(names(Stations), names(Result)))

#結合前面的捷運路線圖，呈現各站的進出站情況
p2 <- ggplot()+
	geom_sf(data=Routes, color="steelblue", size=0.8)+
	geom_sf(data=new.stations, shape=22, color="darkgoldenrod", aes(bg=In.平均站數, size=In.平均站數), alpha=0.6)+
	geom_sf_text(data=Stations, aes(label=站名), color="tomato", size=3,
				position=position_nudge(x=-500, y=500))+
	theme_void() + coord_sf(ndiscr=0)+
	scale_size_area(max_size=6)+
	scale_fill_viridis_c(direction=-1, option="B", na.value=NA)+
	scale_x_continuous(expand=expansion(mult=c(0.01, 0.02)))+
	theme(legend.position="none")
p2

#scale_size_area(max_size=6)：改變整體尺寸的大小，以此圖為例，參數愈小點愈小，反之愈大
#theme(legend.position="none")：legend.position設定圖例的大小，若為"none"則代表刪除圖例


#捷運站點距離MDS分析------------------------------------------------------
XYstations <- filter(.data=Stations, !(編號1 %in% c("Y10", "Y17")))
站名 <- XYstations$站名
distance <- as.data.frame(st_distance(XYstations)) #計算站與站之間的距離

names(distance) <- 站名
rownames(distance) <- 站名

#MDS分析
Index <- distance
fit <- cmdscale(d=Index, eig=TRUE, k=2) #R最基本的MDS分析函數
#d：資料
#k：表示要呈現K向度(維度)的結果

mds.results <- as.data.frame(fit$points) #MDS分析結果中的"points"變項
mds.results$站名 <- row.names(mds.results)
#因為mds.results的站名變項(rownames)我們無法直接存取，所以我們自己再建立一個名稱變項

p3 <- ggplot()+
	geom_point(data=mds.results, aes(x=V1, y=V2), color="orange", shape=20)+
	geom_text(data=mds.results, aes(x=V1, y=V2, label=站名),color="blue", size=3, hjust=1.2)+
	scale_x_continuous(expand=expansion(mult=c(0.08, 0.02)))
p3

fit <- cmdscale(d=Index, eig=TRUE, k=3) #以三維方式呈現結果
mds.results <- as.data.frame(fit$points)
mds.results$站名 <- row.names(mds.results)
plotly::plot_ly(x=mds.results$V1, y=mds.results$V2, z=mds.results$V3, type="scatter3d")

#捷運站點進出關係MDS分析-----------------------------------------------------------------
Ins <- (copy(Stops)
		[ , .(人次=sum(人次)), by=.(進站, 出站)]
		[])
Ins <- dcast(data=Ins, formula=進站~出站, value.var="人次")


