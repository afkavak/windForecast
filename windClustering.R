library("RODBC")
library("xlsx")
library("httr")
library("rjson")
library("zoo")
library("readxl")
library(dtw)

library(reshape)
library(reshape2)

options(stringsAsFactors = F)

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

con<-odbcConnect(dsn="QuantDatabase",pwd="@m0k@chi3091!",uid="sa")

dateIntervals=sqlQuery(con,"SELECT min(measDateTime) dataStartDate,max(measDateTime) dataEndDate,eic EIC
  FROM [QuantDatabase].[dbo].[REAL_TIME_WIND] GROUP BY eic")

commonStart=max(dateIntervals$dataStartDate)
commonEnd=min(dateIntervals$dataEndDate)

long=dateIntervals[!dateIntervals$dataStartDate>=commonStart,]
newS=format(max(long$dataStartDate),"'%Y-%m-%d %H:%M'")
newE=format(min(long$dataEndDate),"'%Y-%m-%d %H:%M'")

# inputQuery=paste0("select eic
# , cast(measDateTime AS DATE) date
# , DATEPART(HOUR,measDateTime) hour 
# ,value  from REAL_TIME_WIND where eic not in 
# (",paste0("'",short$eic,"'",collapse = ","),") 
# and measDateTime>=",newS," and measDateTime<=",newE)

inputQuery=paste0("select eic
, convert(varchar, measDateTime, 120) date
,value  from REAL_TIME_WIND where eic not in 
(",paste0("'",short$eic,"'",collapse = ","),") 
and measDateTime>=",newS," and measDateTime<=",newE)

a=Sys.time()

rawData=sqlQuery(con,inputQuery)

b=Sys.time()

b-a

# T1 Operation
melt1<-melt(rawData,id=c(1,2))
pivot1<-data.frame(cast(melt1,eic~date~variable,NULL))
colnames(pivot1)<-seq(max(long$dataStartDate),min(long$dataEndDate),by="hour")

coreGroup=pivot1[sapply(1:nrow(pivot1), function(i) rownames(pivot1)[i] %in% EMDB$EIC),]

distProfiles <- data.frame(as.matrix(dist(coreGroup, method="Euclidean")))

which(is.nan(rowSums(normCoreGroup))==T)

coreGroup[which(is.na(rowSums(normCoreGroup))==T),]

eicLoc=merge(EMDB,dateIntervals,by="EIC")[,c(1,11,12)]

## mapping
gadm2 <- readRDS("C:/Users/admin/Desktop/busClustering/TUR_adm2.rds")
gadm1 <- readRDS("C:/Users/admin/Desktop/busClustering/TUR_adm1.rds")

mDist=0

matches=c()

for (region in 1:nrow(eicLoc)) {
  
  print(paste0("finding match for ",eicLoc[region,2]))
  
  count=NULL
  
  while(length(count)!=1) {
    
    count=agrep(eicLoc[region,2],toupper(gadm1$NAME_1),max = mDist)
    
    if(length(count)>1) {
      mDist=mDist-.05
    } else {
      if(length(count)==0) {
        mDist=mDist+.05
      }
    }
    print(paste0("iterating at distance ",mDist, "/// progress: ", percent(region/nrow(eicLoc))))
  }
    matches=c(matches,count)
}

# duplicated(matches)

eicLoc$m=gadm1$NAME_1[matches]

mDist=0

subRegion=c()

for (region in 1:nrow(eicLoc)) {
  
  print(paste0("finding match for ",eicLoc[region,3]))
  
  if(eicLoc[region,3]=="" | eicLoc[region,3]=="MERKEZ") {
    if("Merkez" %in% gadm2$NAME_2[gadm2$ID_1==gadm1$ID_1[matches[region]]]) {
      count=which(gadm2$ID_1==gadm1$ID_1[matches[region]] & gadm2$NAME_2=="Merkez")
      print(paste0("assuming city center is the exact location"))
    } else {
      count=sample(which(gadm2$ID_1==gadm1$ID_1[matches[region]]),1)
      print(paste0("setting random location around ", eicLoc[region,2]))
    }
  } else {
    
    count=NULL
    
    while(length(count)!=1) {

      subS=gadm2$NAME_1==eicLoc$m[region]
      
      target=agrep(eicLoc[region,3],toupper(gadm2$NAME_2[subS]),max = mDist)
      
      count=which(subS)[target]
      
      if(length(count)>1) {
        mDist=mDist-.01
      } else {
        if(length(count)==0) {
          mDist=mDist+.01
        }
      }
      print(paste0("iterating at distance ",mDist, "/// progress: ", percent(region/nrow(eicLoc))))
    }
  }
  subRegion=c(subRegion,count)

}

profileIndex=sapply(1:nrow(coreGroup), function(i) rownames(coreGroup)[i] %in% coordInputs$eic)

locIndex=sapply(1:nrow(coordInputs), function(i) coordInputs$eic[i] %in% rownames(coreGroup))


coordInputs=data.frame(eic=eicLoc[,1],province=gadm1$NAME_1[matches],subProvince=gadm2$NAME_2[subRegion],lat=coordinates(gadm2)[subRegion,1],long=coordinates(gadm2)[subRegion,2])

orderCoord=sapply(1:nrow(coordInputs), function(i) if(coordInputs[i,1] %in% rownames(coreGroup)) {which(rownames(coreGroup)==coordInputs[i,1])} else {99})


# generating distance matrix
normCoords=apply(coordInputs[order(orderCoord)[1:nrow(coreGroup)],4:5], 2, function(x) (x-min(x))/(max(x)-min(x)))
euclideanM <- dist(normCoords, method="Euclidean")
distMatrix<-data.frame(as.matrix(euclideanM))

colnames(distMatrix)=coordInputs$eic[order(orderCoord)[1:nrow(coreGroup)]]
rownames(distMatrix)=coordInputs$eic[order(orderCoord)[1:nrow(coreGroup)]]


dm_overall=as.dist(distMatrix/max(distMatrix)+distProfiles/max(distProfiles))



# matching meters via distance
hc1 <- hclust(dm_overall, method="ward.D2")
plot(hc1, labels=substr(rownames(distMatrix),9,16), main="Dendogram of Wind Facilities")


# plot(hc1, labels=substr(rownames(coreGroup),9,16), main="Dendogram of Hourly Meters")
clusterNum=8
rect.hclust(hc1, k=clusterNum)
clusterIndex1<-cutree(hc1, k=clusterNum)

write.table(clusterIndex1,file = "plantsClustered.csv",row.names = T,sep = ";")
