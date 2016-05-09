#! /bin/sh
library(RCurl,lib.loc="/home/abock/R/x86_64-redhat-linux-gnu-library/3.2")
library(zoo,lib.loc="/home/abock/R/x86_64-redhat-linux-gnu-library/3.2")
library(plyr,lib.loc="/home/abock/R/x86_64-redhat-linux-gnu-library/3.2")
library(smwrBase,lib.loc="/home/abock/R/x86_64-redhat-linux-gnu-library/3.2")
options(digits=8)
args<-commandArgs(TRUE)

# input variables
yid1<-as.numeric(c(args[1]))
yid2<-as.numeric(c(args[2]))
xid1<-as.numeric(c(args[3]))
xid2<-as.numeric(c(args[4]))
 
print(xid1)
print(yid1)
 
#*******************************************
# URL for netCDF on the thredsserver
URL<-"http://cida-eros-mows1.er.usgs.gov:8080/thredds/dodsC/snodas/SNODAS_SWE_2003-09-30_2014-06-13.nc.ascii?"
Var<-"SWE"
queary<-paste(URL,"time", sep="")
timeDat = getURL(queary)
 
# get the time series information from the netCDF
con = textConnection(timeDat)
all.lines = readLines(con)
close(con)
timeString<-all.lines[6]
timeArray<-unlist(strsplit(timeString, ", "))
zoo_dates1<- as.Date(as.POSIXct(as.numeric(timeArray)*86400, origin = '1870-1-1T00:00:00Z'))
# convert timeseries to water year index, uses USGS library
wyears<-as.numeric(levels(waterYear(zoo_dates1))[waterYear(zoo_dates1)])

#*******************************************
# This is the sequential indices for each calendar year in the SWE variable
Timesteps=c("2:364","365:726","727:1087","1088:1449","1450:1812","1813:2176","2177:2541","2542:2906","2907:3272","3273:3636")

# number of days within each calendar year, note some years have a day or two of missing data.
noDays<-c(363,362,361,362,363,364,365,365,366,364)
yrs<-c("2004","2005","2006","2007","2008","2009","2010","2011","2012","2013")

#yID=c(0:149)
yID=c(yid1:yid2)
lenyID=length(yID)
#xID=c(0:149)
xID=c(xid1:xid2)
lenxID=length(xID)
 
# extract the time series for each year for each HRU
# Because of Thredds server rules for data size, can only extract
# approximately a 150 x 150 km area.
for (i in (1:length(yrs))){
  print (yrs[i])
  # open connection to extract data for that year
  VarDat = getURL(paste("http://cida-eros-mows1.er.usgs.gov:8080/thredds/dodsC/snodas/SNODAS_SWE_2003-09-30_2014-06-13.nc.ascii?SWE[",Timesteps[i],"][",yID[1],":",yID[length(yID)],"][",xID[1],":",xID[length(xID)],"]",sep=""))
  con = textConnection(VarDat)  
  VarStrings<-scan(con,skip=12,what=character(),sep=",",strip.white=TRUE)
  close(con)
  
  # cuts the query for SWE here
  TimeStart<-which(match(VarStrings,grep("SWE.time",VarStrings,value=TRUE))==1)
  VarSubset<-VarStrings[1:TimeStart-1]
  VarSubset2<-as.numeric(VarSubset[-seq(1,length(VarSubset),lenxID+1)])*1000
  IDs<-VarSubset[seq(1,length(VarSubset),lenxID+1)]
  # set non-logical values to NA
  VarSubset2[VarSubset2>=100000]<-NA
  df<-matrix(VarSubset2,ncol=lenxID,byrow=TRUE)  	  
  
  # get latitudes and longitudes for each grid cell
  latStart<-which(match(VarStrings,grep("SWE.lat",VarStrings,value=TRUE))==1)
  lonStart<-which(match(VarStrings,grep("SWE.lon",VarStrings,value=TRUE))==1)
  lats<-VarStrings[(latStart+1):(lonStart-1)]
  lons<-VarStrings[(lonStart+1):length(VarStrings)]
  
  # create output data frame and bind lats and lons to the beginning of it
  df<-cbind(df,rep(as.numeric(lats),noDays[i]))
  colnames(df)<-c(lons,"lat")
  rownames(df)<-IDs

  noGrids<-expand.grid(lats,lons)
  SWEmat<-mat.or.vec(noDays[i],length(noGrids[,1]))
  count=1
  latlist<-c()
  lonlist<-c()
  for (j in (1:length(lats))){
    # grep by rowname (latitude)
    df_row<-df[grep(paste("\\[",j-1,"]\\b",sep=""),rownames(df)),]
    print(dim(df_row))
    # gerp by colname (longitude)
    for (k in (1:length(lons))){
      df_col<-df_row[,grep(lons[k],colnames(df_row))]
      SWEmat[,count]<-df_col
      count=count+1
      latlist<-append(latlist,lats[j])
      lonlist<-append(lonlist,lons[k]) 
      }
    } 
  SWEmat<-rbind(SWEmat,latlist)
  SWEmat<-rbind(SWEmat,lonlist)
  # write out matrix, lats and lons were added as a row on the bottom
  write.table(SWEmat,paste("output/",yID[1],"_",max(yID),"_",xID[1],"_",max(xID),"_",yrs[i],sep=""),col.names=FALSE,row.names=FALSE,quote=FALSE)
}
