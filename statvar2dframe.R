AnnualMax<-function(model,sdcNum){
  print (model)
  #vars<-read.table(paste(models[1],"/default1.statvar",sep=""),header=T,row.names=NULL,colClasses=c("character","numeric"),sep=" ",nrows=26)
  vars<-as.data.frame(fread(paste(model,"/default",sdcNum,".statvar",sep=""),sep=" ",skip=1,nrows=26))
  PRMSvars<-vars$V1
  #vals2<-read.table(paste(models[1],"/default1.statvar",sep=""),header=F,row.names=NULL,sep=" ",skip=27)
  vals<-as.data.frame(fread(paste(models[1],"/default",sdcNum,".statvar",sep=""),sep=" ",header=F,skip=27))
  colnames(vals)<-c("timestep","year","month","day","hour","minute","second",PRMSvars)

  vals$date<-as.Date(paste(vals$year,"-",vals$month,"-",vals$day,sep=""))
  vals$wyears<-as.numeric(levels(waterYear(vals$date))[waterYear(vals$date)])

  vals<-vals[,!(names(vals) %in% c("timestep","month","day","hour","minute","second",NA,"basin_intcp_stor","basin_lake_stor",
                                 "basin_gwsink","basin_recharge"))]
  #vals<-vals[,c("timestep","month","day","hour","minute","second",NA,"basin_intcp_stor","basin_lake_stor",
  #              "basin_gwsink","basin_recharge"):=NULL]

  meltVals<-melt(vals,id=c("date","wyears"),stringsasFactors=F)

  # Get Max Values
  yearMax<-meltVals %>% arrange(desc(value)) %>%group_by(variable,wyears) %>%filter(row_number() <= 1L)
  yearMax2<-yearMax[order(yearMax$variable,yearMax$wyears),]
  yearMax3<-yearMax2 %>% filter(variable %in% c("basin_sroff","basin_pweqv","basin_snowmelt","basin_snowcov"))
  yearMax3$Max<-yearMax3$value

  # Get SWE time series
  yearMin<-meltVals %>% mutate(value=replace(value,value<=0,NA))
  sweDates<-yearMin$date[which(yearMin$variable=="basin_pweqv"&yearMin$value>0)]
  yearMin2<-yearMin[which(yearMin$date %in% sweDates),]
  yearMin3<-yearMin2 %>% filter(variable %in% c("basin_sroff","basin_pweqv","basin_snowmelt","basin_snowcov"))
  newMatLong2 <- left_join(yearMax3,yearMin3,by=c("variable","wyears"))
  newMatLong2 <- newMatLong2[,!(names(newMatLong2) =="value.y")]

  #write.csv(newMatLong2,paste(model,"/default",sdcNum,".csv",sep=""),row.names=F,quote=F)
  return(newMatLong2)
}

library(reshape2)
library(dplyr)
library(data.table)
library(smwrBase)
args<-commandArgs(TRUE)

Num<-as.numeric(c(args[1]))

setwd("d:/abock/SDC")
models<-list.dirs("Models",full.names=TRUE,recursive=FALSE)

#1 - Extract model output to dataframe
modelData1<-lapply(models[1],AnnualMax,1)
#modelData2<-lapply(models[1:10],AnnualMax,2)
#modelData3<-lapply(models[1:10],AnnualMax,3)
#modelData4<-lapply(models[1:10],AnnualMax,4)
#modelData5<-lapply(models[1:10],AnnualMax,5)
#modelData6<-lapply(models[1:10],AnnualMax,6)
#modelData7<-lapply(models[1:10],AnnualMax,7)

#2 combine dataframes of same model

#3 calculate statistics over POR for each model
# mean length of snowmelt period - finished
# difference in days between PeakSWE and peaksnowmelt, peakrunoff
# total volume of snowmelt - finished
# total volume of snowmelt lost to ET - recomputing models

print (model)
#vars<-read.table(paste(models[1],"/default1.statvar",sep=""),header=T,row.names=NULL,colClasses=c("character","numeric"),sep=" ",nrows=26)

###Read in Data from STATVAR file
vars<-as.data.frame(fread(paste(model,"/default",sdcNum,".statvar",sep=""),sep=" ",skip=1,nrows=26))
PRMSvars<-vars$V1
vals<-as.data.frame(fread(paste(models[1],"/default",sdcNum,".statvar",sep=""),sep=" ",header=F,skip=27))
colnames(vals)<-c("timestep","year","month","day","hour","minute","second",PRMSvars)

vals$date<-as.Date(paste(vals$year,"-",vals$month,"-",vals$day,sep=""))
vals$wyears<-as.numeric(levels(waterYear(vals$date))[waterYear(vals$date)])

vals<-vals[,!(names(vals) %in% c("timestep","month","day","hour","minute","second",NA,"basin_intcp_stor","basin_lake_stor",
                                 "basin_gwsink","basin_recharge"))]

meltVals<-melt(vals,id=c("date","wyears"),stringsasFactors=F)

# Get Max Values
yearMax<-meltVals %>% arrange(desc(value)) %>%group_by(variable,wyears) %>%filter(row_number() <= 1L)
yearMax2<-yearMax[order(yearMax$variable,yearMax$wyears),]
yearMax3<-yearMax2 %>% filter(variable %in% c("basin_sroff","basin_pweqv","basin_snowmelt","basin_snowcov"))
yearMax3$Max<-yearMax3$value

# Get SWE time series
yearMin<-meltVals %>% mutate(value=replace(value,value<=0,NA))
sweDates<-yearMin$date[which(yearMin$variable=="basin_pweqv"&yearMin$value>0)]
yearMin2<-yearMin[which(yearMin$date %in% sweDates),]
yearMin3<-yearMin2 %>% filter(variable %in% c("basin_sroff","basin_pweqv","basin_snowmelt","basin_snowcov"))
newMatLong2 <- left_join(yearMin3,yearMax3,by=c("variable","wyears"))
newMatLong2 <- newMatLong2[,!(names(newMatLong2) =="value.y")]

peakSWE<- newMatLong2 %>% filter(variable == "basin_pweqv")
peakSWEdate<- peakSWE %>% arrange(desc(date.y)) %>%group_by(wyears) %>%filter(row_number() <= 1L) 
peakSWEdate<-rename(peakSWEdate,maxSWE=date.y)
peakSWEdate<-peakSWEdate[,!(names(peakSWEdate) %in% c("value.x","Max","value.y"))]

peakRO<- newMatLong2 %>% filter(variable == "basin_sroff")
peakROdate<- peakRO %>% arrange(desc(date.y)) %>%group_by(wyears) %>%filter(row_number() <= 1L) 
peakROdate<-rename(peakROdate,maxRO=date.y)
peakROdate<-peakROdate[,!(names(peakROdate) %in% c("value.x","Max","value.y"))]

peakMelt<- newMatLong2 %>% filter(variable == "basin_snowmelt")
peakMeltdate<- peakMelt %>% arrange(desc(date.y)) %>%group_by(wyears) %>%filter(row_number() <= 1L) 
peakMeltdate<-rename(peakMeltdate,maxMelt=date.y)
peakMeltdate<-peakMeltdate[,!(names(peakMeltdate) %in% c("value.x","Max","value.y"))]

newMatLong3<-left_join(newMatLong2,peakSWEdate,by=c("wyears"))
newMatLong3<-left_join(newMatLong3,peakROdate,by=c("wyears"))
newMatLong3<-left_join(newMatLong3,peakMeltdate,by=c("wyears"))
newMatLong3<-newMatLong3[,(names(newMatLong3) %in% c("date.x.x","wyears","variable.x","value.x","maxSWE","maxRO","maxMelt"))]

newMatLong3 <- newMatLong3[,!(names(newMatLong3) %in% c("value.y.x","variable.y","value.y","date.x.y"))]
MeltPer<-ifelse(newMatLong3$date.x.x >= newMatLong3$maxDate,1,0)
newMatLong3$MeltPer<-MeltPer
newMatLong4<-newMatLong3 %>% filter(MeltPer==1)

NumDays_meltPeriod_total<-newMatLong4 %>% group_by(variable.x)%>% summarise(count=sum(MeltPer))
MeltPeriod_byYR<-newMatLong4 %>% group_by(variable.x,wyears) %>% summarise (sum=sum(MeltPer))
VolRO_total<-newMatLong4 %>% group_by(variable.x)%>% summarise(count=sum(value.x,na.rm=TRUE))





#****************DEVELOPMENTAL CODE*************************************
#yohoho<-meltVals %>% filter(variable,rank(value,ties.method="last")==1)
#meltVals<-melt(vals,id=c("date","year"))
#maxVals<-meltVals %>% group_by(variable,year) %>% top_n(1,value)
#yearMax<-meltVals %>% arrange(desc(value)) %>%group_by(variable,year) %>%filter(row_number() <= 1L)
#
# AnnualMax<-function(model,sdcNum){
#   #print(model)
#   #print(sdcNum)
#   vars<-read.table(paste(model,"/default",sdcNum,".statvar",sep=""),header=T,row.names=NULL,colClasses=c("character","numeric"),sep=" ",nrows=26)
#   PRMSvars<-vars[,1]
#   vals<-read.table(paste(model,"/default",sdcNum,".statvar",sep=""),header=F,row.names=NULL,sep=" ",skip=27)
#   
#   colnames(vals)<-c("timestep","year","month","day","hour","minute","second",PRMSvars)
#   
#   vals$date<-as.Date(paste(vals$year,"-",vals$month,"-",vals$day,sep=""))
#   
#   vals<-vals[,!(names(vals) %in% c("timestep","month","day","hour","minute","second",NA,"basin_intcp_stor","basin_lake_stor",
#                                    "basin_gwsink","basin_recharge"))]
#   #print(head(vals))
#   
#   meltVals<-melt(vals,id=c("date","year"))
#   yearMax<-meltVals %>% arrange(desc(value)) %>%group_by(variable,year) %>%filter(row_number() <= 1L)
#   
#   yearMax2<-yearMax[order(yearMax$variable,yearMax$year),]
#   #print(head(yearMax2))
#   return(yearMax2)
# }