library(sp)
library(rgdal,lib.loc="/home/abock/R/x86_64-redhat-linux-gnu-library/3.2")
library(raster,lib.loc="/home/abock/R/x86_64-redhat-linux-gnu-library/3.2")
options(digits=8)
args<-commandArgs(TRUE)

# input lat/lons to read each file
yid1<-as.numeric(c(args[1]))
#yid1<-1050
yid2<-as.numeric(c(args[2]))
#yid2<-1199
xid1<-as.numeric(c(args[3]))
#xid1<-1050
xid2<-as.numeric(c(args[4]))
#xid2<-1199
gridNum<-as.numeric(c(args[5]))

setwd(".")
# read in data from SWEmetrics1 - MaxSWE and mmaxSWEdate
pts <-read.table(paste("SWE_metrics/",yid1,"_",yid2,"_",xid1,"_",xid2,sep=""),header=TRUE)
print (dim(pts))
# get the two sets of data for each grid cell
maxSWE<-pts[c(3,5,7,9,11,13,15,17,19,21)]   
maxSWEday<-pts[c(4,6,8,10,12,14,16,18,20,22)]

# get the average maxSWE date for each row
maxSWE2<-apply(maxSWE,1,function(x) mean(x,na.rm=TRUE))
maxSWEday2<-apply(maxSWEday,1,function(x) mean(x,na.rm=TRUE))
maxSWEday2[which(maxSWE2==0)]<-0
maxSWEday2[which(is.na(maxSWE2))]<-NA

# put points in dataframe and convert to spatial object
pts2<-cbind(pts[,1:2],maxSWE2,maxSWEday2)
colnames(pts2)<-c("x","y","maxSWE","maxSWEday")
coordinates(pts2)<-pts2[,1:2]
# assign WGS coordinate sytem
proj4string(pts2)<-CRS("+init=epsg:4326")
#pts3 = spTransform(pts2,CRS("+init=epsg:5070"))

# convert points to grid and write out grids
gridded(pts2)=TRUE
rmaxSWE=raster(pts2,layer=3)
rmaxSWEday=raster(pts2,layer=4)
writeRaster(rmaxSWE,paste("grids/maxSWE",gridNum,".tif",sep=""),overwrite=TRUE)
writeRaster(rmaxSWEday,paste("grids/maxSWEday",gridNum,".tif",sep=""),overwrite=TRUE)

#******Second set of Metrics
# read the file
pts2 <-read.table(paste("SWE_metrics2/",yid1,"_",yid2,"_",xid1,"_",xid2,sep=""),header=TRUE,sep="")
# get the average SWE for each row
NumDays01<-pts2[c(3,6,9,12,15,18,21,24,27,30)]   
NumDays1<-pts2[c(4,7,10,13,16,19,22,25,28,31)]           
AvDur<-pts2[c(5,8,11,14,17,20,23,26,29,32)]         

# get the average maxSWE date for each row
NumDays01_mean<-apply(NumDays01,1,function(x) mean(x,na.rm=TRUE))
NumDays1_mean<-apply(NumDays1,1,function(x) mean(x,na.rm=TRUE))
AvDur_mean<-apply(AvDur,1,function(x) mean(x,na.rm=TRUE))

# assign 0's to indices where average maxSWE is 0
NumDays01_mean[which(maxSWE2==0)]<-0
NumDays01_mean[which(is.na(maxSWE2))]<-NA
NumDays1_mean[which(maxSWE2==0)]<-0
NumDays1_mean[which(is.na(maxSWE2))]<-NA
AvDur_mean[which(maxSWE2==0)]<-0
AvDur_mean[which(is.na(maxSWE2))]<-NA

pts2<-cbind(pts[,1:2],NumDays01_mean,NumDays1_mean,AvDur_mean)
colnames(pts2)<-c("x","y","NumDays01","NumDays1","AvDur")
coordinates(pts2)<-pts2[,1:2]
proj4string(pts2)<-CRS("+init=epsg:4326")
#pts3 = spTransform(pts2,CRS("+init=epsg:5070"))

gridded(pts2)=TRUE
days01=raster(pts2,layer=3)
days1=raster(pts2,layer=4)
Av=raster(pts2,layer=5)

writeRaster(days01,paste("grids2/days01",gridNum,".tif",sep=""))
writeRaster(days1,paste("grids2/days1",gridNum,".tif",sep=""))
writeRaster(Av,paste("grids2/AvDur",gridNum,".tif",sep=""))

# write out data to xyz format
pts3<-cbind(pts,pts2[,3:5])
write.csv(pts3,paste("points/",yid1,"_",yid2,"_",xid1,"_",xid2,sep=""),col.names=TRUE,row.names=FALSE)
#***********************************************************************************************************************************
# references
#http://gis.stackexchange.com/questions/20018/how-can-i-convert-data-in-the-form-of-lat-lon-value-into-a-raster-file-using-r
#yeppers2<-apply(maxSWEday,2,function(x) as.numeric(unlist(as.POSIXlt(as.POSIXct(x, origin="1970-01-01"),format="%d%b%y"))[4]))
#yeppers3<-apply(maxSWEday,1,function(x) as.numeric(unlist(as.POSIXlt(as.POSIXct(x, origin="1970-01-01"),format="%d%b%y"))[4]))

