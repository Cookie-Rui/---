setwd("C:\\Users\\user\\Downloads\\ecology data\\Data-11-1-2021-20211101")
library(data.table)
library(sf)
library(ggplot2)
library(dplyr)
library(stringr)
library(readxl)

POP <- fread(input="2020年村里戶數人口數單一年齡人口數.csv", skip=1, 
			encoding="UTF-8",colClasses="character")

ID <- setDT(read_xlsx(path="2020年V-ID.xlsx", sheet="2020-12"))

POP <- merge(x=POP, y=ID, all.x=TRUE)
names(POP)

POP <- melt(data=POP, id.vars=c("COUNTY","TOWN","VILLAGE","COUNTY_ID","TOWN_ID","V_ID"), 
			measure.vars=str_subset(string=names(POP), pattern="^[0-9]{1,3}歲"),
			variable.name="AgeGroup", value.name="人口數",
			variable.factor = TRUE, value.factor = FALSE)
POP

#melt：寬表轉長表
#參數意義：
#id.vars=我不要轉下來的 / measure.vars=我要轉下來的 (沒寫在這兩個裡面的變項會直接消失)
#variable.name=轉下來的「變項」本身形成的新變項的名稱 / value.name=轉下來的變項的「資料」形成的新變項的名稱
#variable.factor / value.factor ==> variable.name / value.name是否要變成factor型別

POP <- (POP
		[ , 性別:=str_extract(string=AgeGroup, pattern="[男女]$")]
		[ , 年齡:=str_extract(string=AgeGroup, pattern="(^[0-9]{1,3})")]
		[ , .(COUNTY,TOWN,VILLAGE,COUNTY_ID,TOWN_ID,V_ID, 性別,
				年齡=as.numeric(年齡), 人口數=as.numeric(人口數))]
		[])


#計算男女人口數-----------------------------------------------------
#1. data.table作法
POP[ , .(總人口數=sum(人口數)), by=.(性別)]

#2. dplyr作法
count(POP, 性別, wt=人口數) 
#注意不能直接寫count(POP, 性別)，會變成計算case數而不是人口數，要加上權重參數wt

#計算老年(大於等於65歲)男女人口數-----------------------------------------------------
POP[ , .(老年人口數=sum((年齡>=65)*人口數)), by=.(性別)]


############
#          
#   畫圖   
#    
############      
setwd("C:\\Users\\user\\Downloads\\ecology data\\Data-11-1-2021-20211101\\村里shapefile")
Tract <- st_read(dsn="村里.shp", options="ENCODING=UTF-8",
				stringsAsFactors=FALSE, crs=3826) #記得讀shp檔案時檔案夾裡要有其他類型的檔案
plot(st_geometry(Tract)) #全台村里地圖

#把資料提取出臺北市的部分，並依照村里計算人口數
taipeiPOP <- (POP[COUNTY=="臺北市", ]
				[ , .(Population=sum(人口數)), by=.(V_ID)]
				[])

summary(taipeiPOP$Population) #簡單的描述統計

#將全台村里地圖提取出臺北市的部分
Tract.taipei <- filter(.data=Tract, COUNTY=="臺北市") 

plot(st_geometry(Tract.taipei)) #臺北市村里地圖


#dissolve：行政區-------------------------------------------------------------------
#將臺北市村里地圖的村里合併，變成鄉鎮市區地圖
town.taipei <- st_cast(st_union(x=summarise(group_by(.data=Tract.taipei,
											TOWN, TOWN_ID)),by_feature=TRUE))

p0 <- ggplot()+
		geom_sf(data=Tract.taipei, fill=NA, color="LightSkyBlue", size=0.5)+
		geom_sf(data=town.taipei, fill=NA, color="orange", size=1)+
		coord_sf(ndiscr=0)+theme_void()


#面量圖：人口數量--------------------------------------------------------------------
Tract.taipei <- left_join(x=Tract.taipei, y=taipeiPOP,
							by=intersect(names(Tract.taipei), names(taipeiPOP)))

p1 <- ggplot()+
		geom_sf(data=Tract.taipei, aes(fill=Population), color=NA, size=NA)+
		geom_sf(data=town.taipei, fill=NA, color="orange", size=0.5)+
		scale_fill_viridis_c(direction=-1)+
		coord_sf(ndiscr=0)+theme_void()

#面量圖：人口密度--------------------------------------------------------------------
Tract.taipei$Area <- units::drop_units(st_area(Tract.taipei)) / (1000^2)

#新增Density變項 ==> 兩種方法
#1. 一般方法
Tract.taipei$Density <- Tract.taipei$Population / Tract.taipei$Area

#2. dplyr的mutate作法
Tract.taipei <- mutate(.data=Tract.taipei, Density=Population/Area)

p2 <- ggplot()+
		geom_sf(data=Tract.taipei, aes(fill=Density), color=NA, size=NA)+
		geom_sf(data=town.taipei, fill=NA, color="orange", size=0.5)+
		scale_fill_viridis_c(direction=-1)+
		coord_sf(ndiscr=0)+theme_void()

#改良1：原本的密度數據的分布太過傾斜，因此對變項取對數以獲得差距減小的數據
p3 <- ggplot()+
		geom_sf(data=Tract.taipei, aes(fill=log(Density)), color=NA, size=NA)+
		geom_sf(data=town.taipei, fill=NA, color="orange", size=0.5)+
		scale_fill_viridis_c(direction=-1)+
		coord_sf(ndiscr=0)+theme_void()


#改良2：數據經過對數轉換後變得難以直觀上理解，因此調整圖標的數字，使其變回原本的密度數據
p4 <- ggplot()+
		geom_sf(data=Tract.taipei, aes(fill=log(Density)), color=NA, size=NA)+
		geom_sf(data=town.taipei, fill=NA, color="orange", size=0.5)+
		scale_fill_viridis_c(direction=-1,
							name="人口密度",
							breaks=4:12,
							labels=round(exp(4:12)))+
		coord_sf(ndiscr=0)+theme_void()


#改良3：雖然圖標的數字單位變回來了，但現在數字的呈現變得沒有規律，所以我們自行設定breaks和labels
p5 <- ggplot()+
		geom_sf(data=Tract.taipei, aes(fill=log(Density)), color=NA, size=NA)+
		geom_sf(data=town.taipei, fill=NA, color="orange", size=0.5)+
		scale_fill_viridis_c(direction=-1,
							name="人口密度",
							breaks=log(c(100,500,1000,2000,10000,20000,50000)),
							labels=c(100,500,1000,2000,10000,20000,50000))+
		coord_sf(ndiscr=0)+theme_void()

library(patchwork)
(p2+p3)/(p4+p5)

#人口密度點圖------------------------------------------------------------------------

#st_sample：隨機選取特定規模的case數
persons <- st_sample(x=Tract.taipei, size=ceiling(Tract.taipei$Population/1000))
persons

p6 <- ggplot()+
		geom_sf(data=persons, shape=".", size=0.5, color="forestgreen")+
		geom_sf(data=Tract.taipei, fill=NA, color="grey", size=0.01)+
		geom_sf(data=town.taipei, fill=NA, color="orange", size=0.5)+
		coord_sf(ndiscr=0)+theme_void()

#人口重心----------------------------------------------------------------------------
#先找地理中心，然後把人口數作為加權計算人口中心
tractcenter <- st_centroid(Tract.taipei) #計算臺北市每個村里的地理中心
tractcenter

#用st_coordinates(tractcenter)取出tractcenter的座標變項，將x/y座標分開並以此新增兩個變項
tractcenter <- (as.data.table(tractcenter)
				[ , c("x","y"):=as.data.frame(st_coordinates(tractcenter))]
				[])

#依照加權平均公式算出人口中心的x/y座標
xybar <- (tractcenter[ , .(xbar=sum(Population*x)/sum(Population),
							ybar=sum(Population*y)/sum(Population))][])

#將xybar轉換成可以在圖上呈現的sf型態(即轉換成真正的座標格式)
POPcenter <- st_as_sf(x=xybar, coords=c("xbar","ybar"), crs=3826)

p7 <- ggplot()+
		geom_sf(data=Tract.taipei, fill=NA, color="LightSkyBlue", size=0.01)+
		geom_sf(data=town.taipei, fill=NA, color="orange", size=0.5)+
		geom_sf(data=POPcenter, shape=22, size=5, color="skyblue", fill="orchid")+
		coord_sf(ndiscr=0)+theme_void()


#Population Potential 人口潛力-------------------------------------------------------

