# Function to determine maxSWE and maxSWEdate
maxSWEday <- function(x)
{
  i <<- i + 1
  if(all(is.na(x)))
  {
    return("NA")
  } else {
    maxSWE <- which(x==max(x,na.rm=TRUE))
    return(maxSWE[length(maxSWE)])
  }
}

# function to determine average SWE duration
SWEduration <- function(x)
{
  if(all(is.na(x)))
  {
    return(NA)
  #}
  #if (median(x,na.rm=TRUE)==0){
  #  return(0) 
  }else {
    maxSWE <- which(x==max(x,na.rm=TRUE))
    rleRes<-rle(x)
    AvDur<-mean(rleRes$lengths[which(rleRes$values==1)],na.rm=TRUE)
    #AvDur[w]<-NA
    SdDur<-sd(rleRes$lengths[which(rleRes$values==1)],na.rm=TRUE)
    #SdDur[w]<-NA
    MaxDur<-max(rleRes$lengths[which(rleRes$values==1)],na.rm=TRUE)
    #MaxDur[w]<-NA
    return(AvDur)
  }
}

#! /bin/sh
library(RCurl)
library(zoo)
library(dplyr)
library(accelerometry,lib.loc="/home/abock/R/x86_64-redhat-linux-gnu-library/3.2")
library(smwrBase)
library(reshape2)
options(digits=6)
args<-commandArgs(TRUE)

# input variables
yid1<-as.numeric(c(args[1]))
yid2<-as.numeric(c(args[2]))
xid1<-as.numeric(c(args[3]))
xid2<-as.numeric(c(args[4]))

print(yid1)
print(yid2)
print(xid1)
print(xid2)

yrs<-c("2004","2005","2006","2007","2008","2009","2010","2011","2012","2013")

# base URL for SNODAS
#url<-"http://cida-eros-mows1.er.usgs.gov:8080/thredds/dodsC/snodas/SNODAS_SWE_2003-09-30_2014-06-13.nc.ascii?SWE[0:1:1][0:1:1000][0:1:1000]"

#*******************************************
# get the time dimension
URL<-"http://cida-eros-mows1.er.usgs.gov:8080/thredds/dodsC/snodas/SNODAS_SWE_2003-09-30_2014-06-13.nc.ascii?"
Var<-"SWE"
queary<-paste(URL,"time", sep="")
timeDat = getURL(queary)

# convert unix POSIX to calendar dates
con = textConnection(timeDat)
all.lines = readLines(con)
close(con)
timeString<-all.lines[6]
timeArray<-unlist(strsplit(timeString, ", "))
zoo_dates1<- as.Date(as.POSIXct(as.numeric(timeArray)*86400, origin = '1870-1-1T00:00:00Z'))

# read infile for one year to get lats and lons
infile<-read.table(paste("output/",yid1,"_",yid2,"_",xid1,"_",xid2,"_2004",sep=""))
lats<-infile[dim(infile)[1],]
lons<-infile[(dim(infile)[1])-1,]
yrVec<-c()
finalDat<-cbind(c(lats),c(lons))
tempDat<-c()
cNames<-c()

for (i in (1:length(yrs))){
  print (yrs[i])
  yrVec<-append(yrVec,yrs[i])
  # read infile for specific year
  infile<-read.table(paste("output/",yid1,"_",yid2,"_",xid1,"_",xid2,"_",yrs[i],sep=""))
  # convert timeseries to water year index, uses USGS library
  wyears<-as.numeric(levels(waterYear(zoo_dates1))[waterYear(zoo_dates1)])
  wyEnd<-max(which(wyears==yrs[i]))
  wyStart<-min(which(wyears==yrs[i]))
  infile2<-infile[1:(dim(infile)[1]-2),]
  zooSub<-zoo_dates1[wyStart:wyEnd]
  
  newMat<-cbind(infile2,zooSub)
  ###This is a function to use in summarize to get day of max SWE
  newMatLong <- melt(newMat,id.vars=c("zooSub"))
  
  newMatLong<-group_by(newMatLong,variable)
  # get maxSWE date
  maxSWE<-summarize(newMatLong,
                    maxSWEday = zooSub[maxSWEday(value)]
  )
  newMatLong2 <- left_join(newMatLong,maxSWE[c("variable","maxSWEday")],by="variable")
  newMatLong3<-newMatLong2
  
  # determine the melt period
  # creates a vector that gives a 1 to date if after maxSWEdate until September 1
  meltPeriod<-ifelse(newMatLong3$maxSWEday>newMatLong3$zooSub,0,1)
  newMatLong3$meltPeriod<-meltPeriod
  
  # Create vector where date ==1 if SWE>0, -1 if not
  binTS<-ifelse(newMatLong3$value>0,1,-1)
  newMatLong3$binTS<-binTS
  
  # this uses the run-length encoding function to determine the length of each duration of SWE
  Num01<-summarize(newMatLong3,check=sum((rle2(sign(as.integer(binTS)))[,1]>0),na.rm=TRUE))
  
  # create vector where date=1 if SWE>0, 0 if not
  binTS2<-ifelse(newMatLong3$value>0,1,0)
  newMatLong3$binTS2<-binTS2
  # sum of number of days with 1
  NumDays1<-summarize(newMatLong3,check=sum(binTS2,na.rm=TRUE))

  aveSCADur<-summarize(newMatLong3,aveDur=SWEduration(binTS2))  
  aveSCADur$aveDur[aveSCADur$aveDur<0]<-NA
  
  tempDat<-cbind(tempDat,Num01$check,NumDays1$check,aveSCADur$aveDur)
  cNames<-append(cNames,c(paste("NumDays01_",yrs[i],sep=""),paste("NumDays1_",yrs[i],sep=""),paste("AvDur_",yrs[i],sep="")))
}

finalDat<-cbind(finalDat,tempDat)
colnames(finalDat)<-c("lat","lon",cNames)
write.table(finalDat,file=paste("SWE_metrics2/",yid1,"_",yid2,"_",xid1,"_",xid2,sep=""),row.names=FALSE)  

