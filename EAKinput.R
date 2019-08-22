library(forecast)
library("graphql")
library(jsonlite)
library("httr")
library(xlsx)

# PP Info
url="http://gateway.santraltakip.com/seffaflik/transparency/production/real-time-generation-power-plant-list"
file=GET(url, add_headers(.headers=c("x-ibm-client-id"= "1f9e3bd7-de7c-4eb0-baa1-74257e22df93",Accept="application/json")))

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

if(file$status_code==200)
{
  ff=fromJSON(content(file,as="text", encoding = "UTF-8"))
}

ppTable=na.omit(ff$body$powerPlantList)

url <- "http://gateway.santraltakip.com/seffaflik/transparency/production/dpp-organization"
file=GET(url = url, add_headers(.headers=c("x-ibm-client-id"= "65358798-99b8-47db-8784-d0b1bcbf4644",Accept="application/json")))
ff=fromJSON(content(file,as="text", encoding = "UTF-8"))

organizations=ff$body$organizations

load("C:/Users/admin/Documents/gitHub/private/VPP/PP_details.rda")

row2=1

for (row2 in 16:nrow(organizations)) {
  #UEVCB by Org.
  url <- "http://gateway.santraltakip.com/seffaflik/transparency/production/dpp-injection-unit-name"
  file=GET(url = paste0(url,"?organizationEIC=",organizations$organizationETSOCode[row2]), add_headers(.headers=c("x-ibm-client-id"= "1f9e3bd7-de7c-4eb0-baa1-74257e22df93",Accept="application/json")))
  print(paste0("adding plants from ", organizations$organizationETSOCode[row2]," /// progress: ",percent(row2/nrow(organizations))))
  if(file$status_code==200)
  {
    ff=fromJSON(content(file,as="text", encoding = "UTF-8"))
    if(length(ff$body$injectionUnitNames)>0) {
      if(row2==1) {
        orgPP=ff$body$injectionUnitNames
        orgPP$orgEIC=organizations$organizationETSOCode[row2]
      } else {
        df=ff$body$injectionUnitNames
        df$orgEIC=organizations$organizationETSOCode[row2]
        orgPP=rbind(orgPP,df)
      }
    }
  }
}

save(orgPP,file = "org&eic.rda")

plantList=read.xlsx("C:/Users/admin/Documents/windProductionClient/windProductionClient/windProductionClient/2019-08-09clusterSim_AB_V1.0.xlsx",sheetName = "planList",encoding = "UTF-8")
plantDetail=read.csv("injectionUnitDetail.csv",sep = ";",dec = ",")

# PP Info
url="http://gateway.santraltakip.com/seffaflik/transparency/production/power-plant"
file=GET(url = paste0(url,"?period=",Sys.Date()), add_headers(.headers=c("x-ibm-client-id"= "1f9e3bd7-de7c-4eb0-baa1-74257e22df93",Accept="application/json")))

if(file$status_code==200)
{
  ff=fromJSON(content(file,as="text", encoding = "UTF-8"))
  powerPlantList=ff$body$powerPlantList
}

clientInput=merge(plantList,powerPlantList,by.x="EIC",by.y = "eic")

clientInput$uevcb=NA

for (i in 1:nrow(clientInput)) {
  
  url="http://gateway.santraltakip.com/seffaflik/transparency/production/uevcb"
  
  id=clientInput$id[i]
  
  file=GET(url = paste0(url,"?period=",Sys.Date(),"&powerPlantId=",id), add_headers(.headers=c("x-ibm-client-id"= "1f9e3bd7-de7c-4eb0-baa1-74257e22df93",Accept="application/json")))
  print(paste0("adding uevcb from ", clientInput$EIC[i]," /// progress: ",percent(i/nrow(clientInput))))
  if(file$status_code==200)
  {
    ff=fromJSON(content(file,as="text", encoding = "UTF-8"))
    if(length(ff$body$uevcbList)>0) {
      if(i==1) {
        uevcbList=ff$body$uevcbList
        uevcbList$ppEIC=clientInput$EIC[i]
      } else {
        df=ff$body$uevcbList
        df$ppEIC=clientInput$EIC[i]
        uevcbList=rbind(uevcbList,df)
      }
    }
  }
  
 
}


# clientInput$EIC[1]

# ppTable=data.frame(t(sapply(1:length(ff$body$powerPlantList), function(r) sapply(1:length(ff$body$powerPlantList[[1]]), function(c) if(is.null(ff$body$powerPlantList[[r]][[c]])) {"NULL"} else {unlist(ff$body$powerPlantList[[r]][[c]])}))),stringsAsFactors = F)
# 
# colnames(ppTable)=names(ff$body$powerPlantList[[1]])

# windEMDB=read.xlsx(file = "2019-08-08clusterSim.xlsx",sheetName = "planList",encoding = "UTF-8")
# 
# mergedList=merge(windEMDB,ppList,by.x="EIC",by.y="eic",all.x = T)
# 
# which(is.na(mergedList$id))

etsoS=c('40W000000000144J','40W0000000001677','40W000000000143L','40W0000000105219')

for (i in 1:length(etsoS)) {
  targetETSO='40W000000000142N'
  
  quantileGroup=data.frame(matrix(c(.1,.05,.341,.25),nrow=2))
  
  quantileGroup[1,c]
  
  ppID=ppTable[ppTable$eic==targetETSO,1]
  
  url="http://gateway.santraltakip.com/seffaflik/transparency/production/real-time-generation_with_powerplant"
  file=GET(paste0(url,"?startDate=",b_date,"&endDate=",e_date,"&powerPlantId=",ppID), add_headers(.headers=c("x-ibm-client-id"= "1f9e3bd7-de7c-4eb0-baa1-74257e22df93",Accept="application/json")))
  
  ff=fromJSON(content(file,as="text", encoding = "UTF-8"))
  
  df=ff$body$hourlyGenerations
  
  # df=data.frame(t(sapply(1:length(ff$body$hourlyGenerations), function(r) sapply(1:length(ff$body$hourlyGenerations[[1]]), function(c) if(is.null(ff$body$hourlyGenerations[[r]][[c]])) {"NULL"} else {unlist(ff$body$hourlyGenerations[[r]][[c]])}))),stringsAsFactors = F)
  
  production=data.frame(measDateTime=format(as.POSIXct(df[,1],format="%Y-%m-%dT%H:%M:00.000+0300"),"%Y-%m-%d %H:%M:%S"),eic=targetETSO,value=as.numeric(df[,ncol(df)]))
  
  
  if(file$status_code==200)
  {
    ff=fromJSON(content(file,as="text", encoding = "UTF-8"))

    df=ff$body$hourlyGenerations

    df=data.frame(t(sapply(1:length(ff$body$hourlyGenerations), function(r) sapply(1:length(ff$body$hourlyGenerations[[1]]), function(c) if(is.null(ff$body$hourlyGenerations[[r]][[c]])) {"NULL"} else {unlist(ff$body$hourlyGenerations[[r]][[c]])}))),stringsAsFactors = F)

    production=data.frame(measDateTime=format(as.POSIXct(df[,1],format="%Y-%m-%dT%H:%M:00.000+0300"),"%Y-%m-%d %H:%M:%S"),eic=targetETSO,value=as.numeric(df[,ncol(df)]))

    print(paste0("inserting production values of ",mergedList$name[i]," ... progress: ",percent(i/nrow(mergedList))))

    a=Sys.time()

    sqlSave(channel = con,dat = production[,1:3],tablename = "SEFFAFLIK_HIDRO_URETIM2",rownames = F)

    b=Sys.time()

    b-a

    query="insert into SEFFAFLIK_HIDRO_URETIM
  select eic, measDateTime, value from SEFFAFLIK_HIDRO_URETIM2"

    sqlQuery(con,query)

    sqlQuery(con,"DROP TABLE SEFFAFLIK_HIDRO_URETIM2")
  }
}