setwd("C:\\Users\\user\\Downloads\\ecology data\\Data-12-6-2021-20211206")
library(sf)
library(ggplot2)
library(dplyr)
library(data.table)
library(stringr)
library(cowplot)
library(spdep)


ID <- setDT(readxl::read_excel(path="2020年V-ID.xlsx", sheet="2020-12"))
pop <- fread(input="2020年戶數、人口數按戶別及性別分.csv", skip=1, encoding="UTF-8",colClasses="character")
names(pop) <- enc2native(names(pop)) #把變項名稱的編碼系統轉變為電腦內建的編碼系統，以防出現亂碼
pop <- merge(x=pop, y=ID, all.x=TRUE)


str(pop) #戶數/人數現在是"character"，我們要轉成"numeirc"
numeric.vars <- str_subset(string = names(pop), pattern = "[數男女]$") #把所有要轉成數字的變項名稱提取出來
pop <- (pop
		[ , (numeric.vars):=lapply(.SD, as.numeric), .SDcols=numeric.vars]
		[])

#...:=...：新增/修改「一個」變項，(...):=...或c(...):=...：新增/修改「多個」變項
#如果使用(...):=...，則要加上.SDcols參數來選取特定變項
#lapply：等同for迴圈，並且是針對list來做，也就是對多個list進行同個動作，lapply(動作對象, 要執行動作)
#.SD：意思等同「(範圍內)全部的變項」，「範圍」會用例如.SDcols等方式做選取，不選的話就是「全部」
#整段的意義：針對numeric.vars選到的所有變項，執行as.numeric，轉換成numeric型態


pop <- (pop
		[ , (numeric.vars):=lapply(.SD, as.numeric), .SDcols=numeric.vars]
		[ , (numeric.vars):=lapply(.SD, function(x){ifelse(is.na(x),0,x)}), #將NA值變成0
			.SDcols=numeric.vars]
		[])

town.pop <- (copy(pop)
			[ , lapply(.SD, sum),
				by=.(COUNTY, TOWN, COUNTY_ID, TOWN_ID), .SDcols=numeric.vars]
			[ , 單人家戶比重:=
				單獨生活戶_戶數 / (共同生活戶_戶數 + 共同事業戶_戶數 + 單獨生活戶_戶數)*100]
			[])

writexl::write_xlsx(x=town.pop,
					path="C:\\Users\\user\\Downloads\\ecology data\\Data-12-6-2021-20211206\\townPOP.xlsx")
town.pop

#----------------------------------------------
for(i in seq(from=0, to=1, by=0.01)){
	print(quantile(town.pop$共同生活戶_戶數, i))
}
quantile(town.pop$共同生活戶_戶數, 0.45)
#----------------------------------------------


#global spatial autocorrelation
#local indicator of spatial association(LISA)
#Moran's I 空間相關分析
town.pop <- town.pop[!(COUNTY_ID %in% c("09020", "09007", "10016"))&
						!(TOWN_ID %in% c("10014160","10014110","10013220")), ]

Town <- st_read(dsn="標準鄉鎮市區.shp", options="ENCODING=UTF-8", stringsAsFactors=FALSE, crs=3826)
Town <- left_join(x=Town, y=town.pop) |> filter(!is.na(單人家戶比重))
County <- st_cast(st_union(x=summarise(group_by(.data=Town, COUNTY, COUNTY_ID),.groups="drop"),by_feature=TRUE))

library(spdep)
Town.sp <- as(Town, "Spatial") #變成sp格式
Town.sp 
Town.lw <- nb2listw(poly2nb(Town.sp)) #計算每個鄉鎮市區和他周圍的鄉鎮市區的距離
str(Town.lw)


#進行moran整體空間相關檢定
#計算出來的Moran I 統計值可以當作相關係數
moran.test(x=Town.sp$單人家戶比重, listw=Town.lw) 

#進行「局部」空間相關檢定
a <- localmoran(x=Town.sp$單人家戶比重, listw=Town.lw) 


#----------------------------------------------------------#
#  Univariate & Bivariate Moran's I: Global & Local Index  #
#----------------------------------------------------------#
HotSpot <- function(x, y=NULL, Map, ALPHA=0.05, nsims=1000) {
# Adjacency Matrix (Queen) using spdep #
# nb <- poly2nb(Map)
# lw <- nb2listw(nb, style="B", zero.policy=TRUE)
# W  <- as.matrix(as(lw, "symmetricMatrix"))
# W  <- as.matrix(W/rowSums(W))
#  queen neighbours  #
	st_queen <- function(a, b=a) {st_relate(a, b, pattern = "F***T****", sparse=FALSE)}
	W <- st_queen(Map)
	W <- as.matrix(W/rowSums(W))
	W[which(is.na(W))] <- 0

	if(is.null(y)) {y=x}
	xp <- scale(x)[ , 1]
	yp <- scale(y)[ , 1]

# Bivariate Moran's I
	n <- nrow(W)
	global <- (xp%*%W%*%yp)/(n-1)
	local  <- (xp*W%*%yp)
	moran_I <- list(global=global, local=as.numeric(local))

# Permutations for the Bivariate Moran's I
	n   = nrow(W)
	IDs = 1:n
	global_sims = NULL
	local_sims  = matrix(NA, nrow=n, ncol=nsims)
	y_s = matrix(data=y[sample(x=IDs, size=n*nsims, replace=TRUE)], nrow=n, ncol=nsims)
	y_s <- (y_s - apply(y_s, 1, mean))/apply(y_s, 1, sd)
	global_sims  <- as.numeric( (xp%*%W%*%y_s)/(n - 1) )
	local_sims  <- (xp*W%*%y_s)
	simula_moran <- list(global_sims = global_sims, local_sims  = local_sims)

#======================================================
# Calculating the index and its simulated distribution for global and local values
# Global Moral
	global_moran <- moran_I[[1]][1]
# Local values
	m_i <- moran_I[[2]]
	Map$LocalMoranI <- m_i
# local simulations
	local_sims <- simula_moran$local_sims
# global pseudo p-value
  # get all simulated global moran
	global_sims <- simula_moran$global_sims
  # Proportion of simulated global values taht are higher (in absolute terms) than the actual index
	moran_pvalue <- sum(abs(global_sims) > abs( global_moran )) / length(global_sims)
  #> 0
# Identifying the significant values
	alpha <- ALPHA  # for a 95% confidence interval
	probs <- c(alpha/2, 1-alpha/2)
	intervals <- t( apply(local_sims, 1, function(x) quantile(x, probs=probs)))
	sig       <- ( m_i < intervals[ , 1] )  | ( m_i > intervals[ , 2] )
	Map$sig <- sig
# Identifying the LISA clusters
	xp <- scale(x)[ , 1]
	yp <- scale(y)[ , 1]
	patterns <- as.character( interaction(xp > 0, W%*%yp > 0) )
	patterns <- str_replace_all(string=patterns, patter=c("TRUE"="High", "FALSE"="Low"))
	patterns[Map$sig==0] <- "Not Significant"
	Map$patterns <- patterns
# Rename LISA clusters
	Map$patterns2 <- factor(x=Map$patterns,
	                      levels=c("Low.Low", "Low.High", "High.Low", "High.High",
	                               "Not Significant"))
	return(Map)
}
Island <- HotSpot(x=Town$單人家戶比重, Map=Town, ALPHA=0.05)
#high.high：熱區 ==> 該單人家戶比例很高 且 周遭的鄉鎮市區的單人家戶比例也很高(自變相關>0)
#low.low：冷區 ==> 該單人家戶比例很低 且 周遭的鄉鎮市區的單人家戶比例也很低(自變相關>0)
#high.low：孤島 ==> 該單人家戶比例很高 且 周遭的鄉鎮市區的單人家戶比例很低(自變相關<0)
#low.high：孤島 ==> 該單人家戶比例很低 且 周遭的鄉鎮市區的單人家戶比例很高(自變相關<0)

Island <- mutate(.data=Island, Area=ifelse((patterns=="Not Significant"), NA, patterns),
				Area=factor(x=Area,
				levels=c("Low.Low","Low.High","High.Low","High.High"),
				labels=c("Low-Low","Low-High","High-Low","High-High")))

pp <- ggplot()+
	geom_sf(data=filter(.data=Island, !is.na(Area)),
			aes(fill=Area), size=0.01, color="tan")+
	geom_sf(data=County, fill=NA, color="forestgreen", size=0.2)+
	scale_fill_manual(values=c("dodgerblue", rgb(0.8,1,0.96, alpha=0.7),
								rgb(1,0.84,0.96, alpha=0.7),"orangered"),
					na.value=NA, name="單人家戶比重", drop=FALSE, limits=levels(Island$Area))+
	coord_sf(ndiscr=0)+theme_void()+
	theme(legend.position=c(0.9, 0.25),
				legend.key.height=unit(0.6, "cm"),
				legend.spacing.x=unit(0.4, "cm"),
				legend.title=element_text(size=13))
pp

