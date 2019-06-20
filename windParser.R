###start

library("RODBC")
library("xlsx")
library("httr")
library("rjson")
library("zoo")
library("readxl")

options(stringsAsFactors = F)

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

con<-odbcConnect(dsn="QuantDatabase",pwd="@m0k@chi3091!",uid="sa")

# eic
# Santral EIC bilgisi
# id
# Santral ID bilgisi
# name
# Santral ad bilgisi
# shortName
# Santral kısa ad bilgisi

"http://gateway.santraltakip.com/seffaflik/"
MClist=read.xlsx("C:/Users/admin/Downloads/AFKMarginalCosts.xlsx",sheetName = "Sheet2", encoding = "UTF-8")[,1:5]

diff<-31
sdate<-format((as.Date(format(Sys.time(), "%Y-%m-%d"))-diff), "%Y-%m-%d")
edate<-format((as.Date(format(Sys.time(), "%Y-%m-%d"))), "%Y-%m-%d")

# PP Info
url="http://gateway.santraltakip.com/seffaflik/transparency/production/power-plant"

status="success"
count=1
while((count==1 | status=="fail") & count<=10) {
  res <- try(assign("response",GET(url = paste0(url,"?period=",Sys.Date()), add_headers(.headers=c("x-ibm-client-id"= "1f9e3bd7-de7c-4eb0-baa1-74257e22df93",Accept="application/json")),timeout(5))),silent = TRUE)
  if(class(res) == "try-error") {
    status="fail"
  } else {
    status="success"
    file=response
  }
  print(paste0("trial ",count,", status: ",status))
  count=count+1
}

file=GET(url = paste0(url,"?period=",Sys.Date()), add_headers(.headers=c("x-ibm-client-id"= "1f9e3bd7-de7c-4eb0-baa1-74257e22df93",Accept="application/json")))

if(file$status_code==200)
{
  ff=fromJSON(content(file,as="text", encoding = "UTF-8"))
  powerPlantList=ff$body$powerPlantList
}

# eic
# UEVÇB EIC bilgisi
# id
# UEVÇB ID bilgisi
# name
# UEVÇB ad bilgisi

# row=510
# UEVCB

for (row in 1:nrow(powerPlantList)) {
  
  url="http://gateway.santraltakip.com/seffaflik/transparency/production/uevcb"
  
  id=powerPlantList$id[row]
  
  # file=GET(url = paste0(url,"?period=",Sys.Date(),"&powerPlantId=",id), add_headers(.headers=c("x-ibm-client-id"= "1f9e3bd7-de7c-4eb0-baa1-74257e22df93",Accept="application/json")))
  
  status="success"
  count=1
  while((count==1 | status=="fail") & count<=10) {
    res <- try(assign("response",GET(url = paste0(url,"?period=",Sys.Date(),"&powerPlantId=",id), add_headers(.headers=c("x-ibm-client-id"= "1f9e3bd7-de7c-4eb0-baa1-74257e22df93",Accept="application/json")),timeout(5))),silent = TRUE)
    if(class(res) == "try-error") {
      status="fail"
    } else {
      status="success"
      file=response
    }
    print(paste0("trial ",count,", status: ",status))
    count=count+1
  }
  
  print(paste0("adding ",powerPlantList$name[row]," /// progress: ",percent(row/nrow(powerPlantList))))
  
  if(file$status_code==200)
  {
    ff=fromJSON(content(file,as="text", encoding = "UTF-8"))
    if(is.null(ff$body$uevcbList) | length(ff$body$uevcbList)==0) {
      print(paste0("No record found for ",powerPlantList$name[row]))
    } else {
      if(row==1) {
        respDF=ff$body$uevcbList
        colnames(respDF)=paste0("UECVB_",colnames(respDF))
        df=data.frame(id,respDF)
        UECVBdetail=df
      } else {
        respDF=ff$body$uevcbList
        colnames(respDF)=paste0("UECVB_",colnames(respDF))
        df=data.frame(id,respDF)
        UECVBdetail=rbind(UECVBdetail,df)
      } 
    }
  }  
}

duplicateRows=UECVBdetail[duplicated(UECVBdetail[,c(3)]),]
duplicateRows

UECVBdetail=UECVBdetail[!duplicated(UECVBdetail[,c(2,3,4)]),]

#ETSOCode
url <- "http://gateway.santraltakip.com/seffaflik/transparency/production/dpp-organization"
file=GET(url = url, add_headers(.headers=c("x-ibm-client-id"= "65358798-99b8-47db-8784-d0b1bcbf4644",Accept="application/json")))
ff=fromJSON(content(file,as="text", encoding = "UTF-8"))

# uu<-data.frame(ff[[3]])
# names(uu)<-c("Id","Name","Status","ETSO","ShortName")

organizations=ff$body$organizations

# row2=2
#UEVCB by Org.
url <- "http://gateway.santraltakip.com/seffaflik/transparency/production/dpp-injection-unit-name"
for (row2 in 1:nrow(organizations)) {
  
  status="success"
  count=1
  while((count==1 | status=="fail") & count<=10) {
    res <- try(assign("response",GET(url = paste0(url,"?organizationEIC=",organizations$organizationETSOCode[row2]), add_headers(.headers=c("x-ibm-client-id"= "1f9e3bd7-de7c-4eb0-baa1-74257e22df93",Accept="application/json")),timeout(5))),silent = TRUE)
    if(class(res) == "try-error") {
      status="fail"
    } else {
      status="success"
      file=response
    }
    print(paste0("trial ",count,", status: ",status))
    count=count+1
  }
  
  print(paste0("adding ",organizations$organizationName[row2],"'s pp details /// progress: ",percent(row2/nrow(organizations))))
  
  if(file$status_code==200)
  {
    ff=fromJSON(content(file,as="text", encoding = "UTF-8"))
    if(is.null(ff$body$injectionUnitNames) | length(ff$body$injectionUnitNames)==0) {
      print(paste0("No record found for ",powerPlantList$name[row]))
    } else {
      if(row2==1) {
        respDF=ff$body$injectionUnitNames
        colnames(respDF)=paste0("injectionUnit_",colnames(respDF))
        df=data.frame(orgEic=organizations$organizationETSOCode[row2],respDF)
        injectionUnitDetail=df
      } else {
        respDF=ff$body$injectionUnitNames
        colnames(respDF)=paste0("injectionUnit_",colnames(respDF))
        df=data.frame(orgEic=organizations$organizationETSOCode[row2],respDF)
        injectionUnitDetail=rbind(injectionUnitDetail,df)
      } 
    }
  } 
}

write.table(injectionUnitDetail,file = "injectionUnitDetail.csv",sep = ";",dec = ",",row.names = F)
write.table(UECVBdetail,file = "UECVBdetail.csv",sep = ";",dec = ",",row.names = F)

### excelParse

path="C:/Users/admin/Downloads/gerçek zamanlı üretim/New folder"

filesToParse=list.files(path, pattern = "GercekZamanliUretim")

eicS=sapply(1:length(filesToParse), function(i) strsplit(filesToParse[i],"-")[[1]][length(strsplit(filesToParse[i],"-")[[1]])-2])

eicSomitted=eicS[!duplicated(eicS)]

filesToParse=list.files(path, pattern = "GercekZamanliUretim")[!duplicated(eicS)]

j=1

for (j in 2:length(filesToParse)) {
  
  windData=read_excel(paste(path,filesToParse[j],sep = "/"))
  
  # plot(as.numeric(gsub("[:,:]",".",windData[,3])))
  
  df=data.frame(eic=eicSomitted[j],measDateTime=paste0(format(as.Date(windData[,1],format = "%d.%m.%Y"),"%Y-%m-%d "),windData[,2]),value=as.numeric(gsub("[:,:]",".",windData[,3])))
  
  sqlSave(channel = con,dat = df,tablename = "REAL_TIME_WIND_TEMP",rownames = F)
  
  query="insert into REAL_TIME_WIND
  select * from REAL_TIME_WIND_TEMP"
  
  sqlQuery(con,query)
  
  sqlQuery(con,"DROP TABLE REAL_TIME_WIND_TEMP")
  
  
  print(percent(j/length(filesToParse)))
}

