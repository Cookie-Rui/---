https://emap.pcsc.com.tw/EMapSDK.aspx
commandid: SearchStore
city: 台北市
town: 文山區
roadname: 
ID: 
StoreName: 
SpecialStore_Kind: 
leftMenuChecked: 
address: 

------------------------------------------------------
library(data.table)
library(stringr)
library(httr)
library(readxl)
library(writexl)
url <- "https://emap.pcsc.com.tw/EMapSDK.aspx"
KeyWords <- list(commandid="SearchStore",
                 city="台北市", town="文山區",
                 roadname="", ID="", StoreName="", SpecialStore_Kind="",
                 leftMenuChecked="", address="")
raw <- GET(url, query=KeyWords)
raw
text <- content(raw, as="text")
text
text <- as.data.table(str_split(text, "<POIID>"))

result <- (text
           [ , .(店號=str_extract(string = V1, pattern = "(^[0-9]{6})"),
                   店名=str_extract(string = V1, pattern = "(?<=POIName>)\\w{2,}"),
                   X=str_extract(string = V1, pattern = "((?<=<X>)[0-9.]{2,})"),
                   Y=str_extract(string = V1, pattern = "((?<=<Y>)[0-9.]{2,})"),
                   地址=str_extract(string = V1, pattern = "(?<=Address>)\\w{2,}"))]
           [!is.na(店號),]
           [])
result
