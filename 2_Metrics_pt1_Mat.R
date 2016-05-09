# function to determine max SWE
maxSWEday <- function(x)
{
  if(all(is.na(x)))
  {
    return("NA")
  } else {
    maxSWE <- which(x==max(x,na.rm=TRUE))
    return(maxSWE[length(maxSWE)])
  }
}


#! /bin/sh
library(RCurl,lib.loc="/home/abock/R/x86_64-redhat-linux-gnu-library/3.2")
library(zoo)
library(dplyr)
library(reshape2,lib.loc="/home/abock/R/x86_64-redhat-linux-gnu-library/3.2")
library(smwrBase)
options(digits=6)
args<-commandArgs(TRUE)

# input variables
yid1<-as.numeric(c(args[1]))
yid2<-as.numeric(c(args[2]))
xid1<-as.numeric(c(args[3]))
xid2<-as.numeric(c(args[4]))
yrs<-c("2004","2005","2006","2007","2008","2009","2010","2011","2012","2013")

# base URL for SNODAS
#url<-"http://cida-eros-mows1.er.usgs.gov:8080/thredds/dodsC/snodas/SNODAS_SWE_2003-09-30_2014-06-13.nc.ascii?SWE[0:1:1][0:1:1000][0:1:1000]"

#*******************************************
# get the time dimension from the netCDF
URL<-"http://cida-eros-mows1.er.usgs.gov:8080/thredds/dodsC/snodas/SNODAS_SWE_2003-09-30_2014-06-13.nc.ascii?"
Var<-"SWE"
queary<-paste(URL,"time", sep="")
timeDat = getURL(queary)

# convert POSIX unix time to dates
con = textConnection(timeDat)
all.lines = readLines(con)
close(con)
timeString<-all.lines[6]
timeArray<-unlist(strsplit(timeString, ", "))
zoo_dates1<- as.Date(as.POSIXct(as.numeric(timeArray)*86400, origin = '1870-1-1T00:00:00Z'))

# read file from one year to get lats and lons(output fiel from 1_Read_Data)
infile<-read.table(paste("output/",yid1,"_",yid2,"_",xid1,"_",xid2,"_2004",sep=""))
lats<-infile[dim(infile)[1],]
lons<-infile[(dim(infile)[1])-1,]

df = data.frame(matrix(vector(), dim(infile)[2], 2,
                       dimnames=list(c(), c("lat", "lon"))),
                stringsAsFactors=F)
df$lat<-unlist(lats)
df$lon<-unlist(lons)


for (j in (1:length(yrs))){
  print (yrs[j])
  # open infile for specific year
  infile<-read.table(paste("output/",yid1,"_",yid2,"_",xid1,"_",xid2,"_",yrs[j],sep=""))
  # convert timeseries to water year index, uses USGS library
  wyears<-as.numeric(levels(waterYear(zoo_dates1))[waterYear(zoo_dates1)])
  #wyEnd<-max(which(wyears=="2004"))
  wyEnd<-max(which(wyears==yrs[j]))
  #wyStart<-min(which(wyears=="2004"))
  wyStart<-min(which(wyears==yrs[j]))
  infile2<-infile[1:(dim(infile)[1]-2),]
  zooSub<-zoo_dates1[wyStart:wyEnd]
  # 1 - 365 mark of water year day
  wyMark<-as.vector(1:length(zooSub))

  # bind data for year with wyDay
  newMat<-cbind(infile2,wyMark)
  ###This is a function to use in summarize to get day of max SWE
  # Melt the data frame
  newMatLong <- melt(newMat,id.vars=c("wyMark"))

  # Utilize these functions to get MaxSWE and MaxSWEdate
  newMatLong<-group_by(newMatLong,variable)
  maxSWEvalue<-summarize(newMatLong,max=max(value,na.rm=TRUE))
  maxSWE<-summarize(newMatLong,
                    maxSWEday = wyMark[maxSWEday(value)]
  )

  # Where MaxSWE is 0, maxSWEday should be NA, maybe this should be 0??
  maxSWE$maxSWEday[maxSWEvalue$max==0]<-NA
  # add column names for specific year
  df[[paste("MAxSWE_",yrs[j],sep="")]]<-maxSWEvalue$max
  df[[paste("MaxSWEdate_",yrs[j],sep="")]]<-maxSWE$maxSWEday
}
  
# write output file
write.table(df,file=paste("SWE_metrics/",yid1,"_",yid2,"_",xid1,"_",xid2,sep=""),row.names=FALSE)
