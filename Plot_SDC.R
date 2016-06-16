plotSDC<-function(model){
  
  modelname<-unlist(strsplit(model,"/"))[4]
  outfile<-read.csv(paste("../runs/syn_sdc/Routput/",modelname,".csv",sep=""),row.names=1)
  outMelt<-melt(as.matrix(outfile))
  Names<-outMelt$Var1

  #totalDays<-as.numeric(outfile[1,])
  MeanAnnMelt<-as.numeric(outfile[2,])
  MeanSDMelt<-as.numeric(outfile[3,])
  TotalRo<-as.numeric(outfile[4,])
  Totalmelt<-as.numeric(outfile[5,])
  totalCfS<-as.numeric(outfile[6,])
  MeanSWEMeltPeak<-as.numeric(outfile[7,])
  SDSweMeltPeak<-as.numeric(outfile[8,])
  MeanSWEROPeak<-as.numeric(outfile[9,])
  SDSWEROPeak<-as.numeric(outfile[10,])
  MeanSWECFSpeak<-as.numeric(outfile[11,])
  SDSWECFSpeak<-as.numeric(outfile[12,])

  #png("d:/abock/temp/testerino.png",width=11, height=8.5,units="in",res=150)
  png(paste("../runs/syn_sdc/Routput/",modelname,"1.png",sep=""),width=11,height=8.5,units="in",res=150)
  par(mfrow=c(2,3))
  barplot(MeanAnnMelt,space=c(0.2,0,0,0,0,0,0),xlab="curve",ylab="days",main="Mean Annual Melt Period Length")
  barplot(MeanSDMelt,space=c(0.2,0,0,0,0,0,0),xlab="curve",ylab="days",main="SD Annual Melt Period Length")
  barplot(TotalRo,space=c(0.2,0,0,0,0,0,0),xlab="curve",ylab="inches",main="Total RO (in.)")
  barplot(Totalmelt,space=c(0.2,0,0,0,0,0,0),xlab="curve",ylab="inches",main="Total Melt (in.)")
  barplot(totalCfS,space=c(0.2,0,0,0,0,0,0),xlab="curve",ylab="CFS", main = "Total CFS (CFS)")
  dev.off()

  #png("d:/abock/temp/testerino2.png",width=11,height=8.5,units="in",res=150)
  png(paste("../runs/syn_sdc/Routput/",modelname,"2.png",sep=""),width=11,height=8.5,units="in",res=150)
  par(mfrow=c(2,3))
  barplot(abs(MeanSWEMeltPeak),space=c(0.2,0,0,0,0,0,0),xlab="curve",ylab="days",main="Annual Mean lag between SWE and Melt Peak (days)")
  barplot(abs(MeanSWEROPeak),space=c(0.2,0,0,0,0,0,0),xlab="curve",ylab="days",main = "Annual Mean lag between SWE and Melt Peak (days)")
  barplot(abs(MeanSWECFSpeak),space=c(0.2,0,0,0,0,0,0),xlab="curve",ylab="days",main = "Annual Mean lag between SWE and Melt Peak (days)")
  barplot(SDSweMeltPeak,space=c(0.2,0,0,0,0,0,0),xlab="curve",ylab="days",main = "Annual SD lag between SWE and Melt Peak (days)")
  barplot(SDSWEROPeak,space=c(0.2,0,0,0,0,0,0),xlab="curve",ylab="days",main = "Annual SD lag between SWE and Melt Peak (days)")
  barplot(SDSWECFSpeak,space=c(0.2,0,0,0,0,0,0),xlab="curve",ylab="days",main = "Annual SD lag between SWE and Melt Peak (days)")
  dev.off()
}

# 
# library(reshape2)
# 
# #setwd("d:/abock/SDC")
# setwd(".")
# models<-list.dirs("Models",full.names=TRUE,recursive=FALSE)
# #models<-models[!(models %in% c("runs/syn_sdc/params","runs/syn_sdc/Routput"))]
# 
# outfile<-read.csv(paste("Models/",model,"/output.csv",sep=""),row.names=1)
# outMelt<-melt(as.matrix(outfile))
# Names<-outMelt$Var1
# 
# ggplot(data=outMelt, aes(Var1,Var2)) +
#   geom_bar(stat="identity")
# 
# #totalDays<-as.numeric(outfile[1,])
# MeanAnnMelt<-as.numeric(outfile[2,])
# MeanSDMelt<-as.numeric(outfile[3,])
# TotalRo<-as.numeric(outfile[4,])
# Totalmelt<-as.numeric(outfile[5,])
# totalCfS<-as.numeric(outfile[6,])
# MeanSWEMeltPeak<-as.numeric(outfile[7,])
# SDSweMeltPeak<-as.numeric(outfile[8,])
# MeanSWEROPeak<-as.numeric(outfile[9,])
# SDSWEROPeak<-as.numeric(outfile[10,])
# MeanSWECFSpeak<-as.numeric(outfile[11,])
# SDSWECFSpeak<-as.numeric(outfile[12,])
# 
# #png("d:/abock/temp/testerino.png",width=11, height=8.5,units="in",res=150)
# png(paste("runs/syn_sdc/Routput/",modelname,"1.png",sep=""),width=11,height=8.5,units="in",res=150)
# par(mfrow=c(2,3))
# barplot(MeanAnnMelt,space=c(0.2,0,0,0,0,0,0),xlab="curve",ylab="days",main="Mean Annual Melt Period Length")
# barplot(MeanSDMelt,space=c(0.2,0,0,0,0,0,0),xlab="curve",ylab="days",main="SD Annual Melt Period Length")
# barplot(TotalRo,space=c(0.2,0,0,0,0,0,0),xlab="curve",ylab="inches",main="Total RO (in.)")
# barplot(Totalmelt,space=c(0.2,0,0,0,0,0,0),xlab="curve",ylab="inches",main="Total Melt (in.)")
# barplot(totalCfS,space=c(0.2,0,0,0,0,0,0),xlab="curve",ylab="CFS", main = "Total CFS (CFS)")
# dev.off()
# 
# #png("d:/abock/temp/testerino2.png",width=11,height=8.5,units="in",res=150)
# png(paste("runs/syn_sdc/Routput/",modelname,"2.png",sep=""),width=11,height=8.5,units="in",res=150)
# par(mfrow=c(2,3))
# barplot(abs(MeanSWEMeltPeak),space=c(0.2,0,0,0,0,0,0),xlab="curve",ylab="days",main="Annual Mean lag between SWE and Melt Peak (days)")
# barplot(abs(MeanSWEROPeak),space=c(0.2,0,0,0,0,0,0),xlab="curve",ylab="days",main = "Annual Mean lag between SWE and Melt Peak (days)")
# barplot(abs(MeanSWECFSpeak),space=c(0.2,0,0,0,0,0,0),xlab="curve",ylab="days",main = "Annual Mean lag between SWE and Melt Peak (days)")
# barplot(SDSweMeltPeak,space=c(0.2,0,0,0,0,0,0),xlab="curve",ylab="days",main = "Annual SD lag between SWE and Melt Peak (days)")
# barplot(SDSWEROPeak,space=c(0.2,0,0,0,0,0,0),xlab="curve",ylab="days",main = "Annual SD lag between SWE and Melt Peak (days)")
# barplot(SDSWECFSpeak,space=c(0.2,0,0,0,0,0,0),xlab="curve",ylab="days",main = "Annual SD lag between SWE and Melt Peak (days)")
# dev.off()