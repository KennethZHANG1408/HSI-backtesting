#Please set the working folder to your own one.
rm(list = ls())
setwd("E:/Desktop/QuantInvestment")
Data = read.csv("data2.csv",stringsAsFactors = F)

library(data.table)
colnames(Data)=list('DT','ID','WGT','PX','DIV','LEV','PMOM','RTNVOL','MCAP','EY','RTNM','GICS')
Data['PMOM']=lapply(Data['PMOM'],as.numeric)
Data['ID']=lapply(Data['ID'],as.character)
Data['RTNM']=lapply(Data['RTNM'],as.numeric)
Data['WGT']=Data['WGT']/100

#Descriptional Statistics
#correlation matrix:highest->DIV and EY,which makes sense
cormat = cor(na.omit(Data[,5:11]))



#comp
Data = as.data.table(Data)
Data = melt(Data,id.vars = c('DT','ID','WGT','RTNM','GICS'),measure.vars = c('DIV','LEV','PMOM','RTNVOL','MCAP','EY'))

#Data
Data[,':='(S1=(value-mean(value,na.rm = T))/sd(value,na.rm = T),S2=(value-median(value,na.rm = T))/median(abs(value-median(value,na.rm = T)),na.rm = T),S3 = frankv(value,ties.method='average',na.last='keep',order=1)),by=.(DT,variable)]
Data[,S3:=S3/max(S3,na.rm = T),by=.(DT,variable)]

#Data

dat = dcast(Data,DT+ID+WGT+RTNM+GICS~variable,value.var='S3')
#dat
dat[,':=' (ComP_Value = (ifelse(is.na(EY),0,-EY)+ifelse(is.na(MCAP),0,MCAP))
           /ifelse(is.na(EY)&is.na(MCAP),NA,(ifelse(is.na(EY),0,1)+ifelse(is.na(MCAP),0,1))),
           Comp_Earn=(ifelse(is.na(PMOM),0,PMOM)+ifelse(is.na(RTNVOL),0,RTNVOL))
           /ifelse(is.na(PMOM)&is.na(RTNVOL),NA,(ifelse(is.na(PMOM),0,1)+ifelse(is.na(RTNVOL),0,1))),
           Comp_Qua=(ifelse(is.na(DIV),0,-DIV)+ifelse(is.na(-LEV),0,LEV))
           /ifelse(is.na(DIV)&is.na(LEV),NA,(ifelse(is.na(DIV),0,1)+ifelse(is.na(LEV),0,1)))) ]

dat[,ComP:=ComP_Value + Comp_Earn + Comp_Qua]


#-------------------------------------------------------------
Data = dat
Data = as.data.table(Data)
fractile <- function(x,n) 
  {
  if (sum(!is.na(x))<n){return(rep(1L*NA,length(x)))}
  rnk = rank(x,ties.method = 'first',na.last = 'keep')
  qnt = quantile(rnk, probs = seq(0,1,length.out = n+1),na.rm = T,names = F)
  cut(rnk,breaks = qnt, include.lowest = T, labels = F, right = F)
}

NoBas = 3
Data[,Basket:=fractile(ComP,3),by=DT]
Data

Perf = Data[,sum(WGT*RTNM,na.rm = T)/sum(ifelse(is.na(RTNM),0,WGT)),by = .(DT,Basket)]
Perf
BMPerf = Data[,sum(WGT*RTNM,na.rm = T)/sum(ifelse(is.na(RTNM),0,WGT)),by = DT]
setnames(BMPerf,c('DT','BM'))
BMPerf
setkey(BMPerf,DT)
setkey(Perf,DT)
Perf = BMPerf[Perf]
Perf
Perf[, RelRtn :=V1-BM]
Perf
Perf = dcast(Perf,DT~Basket,value.var = 'RelRtn')
Perf
setnames(Perf,c('DT','Low','Mid','High','NoData'))
Perf[,LS:=High-Low]
Perf
Perf = melt(Perf,id.vars = 'DT')
Perf
ExRtn = Perf[,12*mean(value,na.rm = T),by = variable]
RskAdjRtn = Perf[,sqrt(12)*mean(value,na.rm=T)/sd(value,na.rm = T),by = variable]

setorder(Perf,DT)
CumP = Perf[,.(DT = DT, CumPerf = cumsum(value)),by = variable]
CumP = CumP[!is.na(CumPerf),]
setnames(CumP,c('Basket','Date','Cum.Perf'))
CumP

library(ggplot2)
p = ggplot(data = CumP[Basket %in% c('Low','Mid','High')],aes(x=Date, y=Cum.Perf))
p = p+geom_line(aes(group=Basket,color= Basket),size=1)
p = p+theme_bw(base_family = 'Times')
print(p)

p = ggplot(data = CumP[Basket=='LS'],aes(x = Date,y = Cum.Perf))
p = p + geom_line(size = 1)
p = p + theme_bw(base_family = 'Times')
print(p)


IC = Data[,cor(RTNM,ComP,method = 'spearman',use = 'pairwise.complete.obs'),by=DT]
IC[,mean(V1,na.rm = T)]
setnames(IC,c('Date','Info.Coef'))

p = ggplot(data = IC, aes(x=Date,y=Info.Coef))+geom_bar(stat = 'identity')
p = p + theme_bw(base_family = 'Times')
print(p)

setkey(Data,DT)
setkey(BMPerf,DT)
Data = BMPerf[Data]
Data
HR = Data[, sum(RTNM>BM,na.rm = T)/sum(!is.na(RTNM)),by=.(DT,Basket)]
HR_avg = HR[,mean(V1 , na.rm = T), by=Basket]
setorder(HR_avg,Basket)
HR_avg
HR[,HitRate:=V1-0.5]
p = ggplot(data = HR[!is.na(Basket)],aes(x=DT,y=HitRate))+geom_bar(stat = 'identity')
p = p +facet_wrap(~Basket,nrow=1)+theme_bw(base_family = 'Times')
print(p)

Turnover = function(Data)
{
  setorder(Data,DT)
  Mth = Data[,unique(DT)]
  sapply(2:length(Mth), function(m){
    Basket1 = Data[DT==Mth[m-1],unique(ID)]
    Basket2 = Data[DT==Mth[m],unique(ID)]
    length(setdiff(Basket1,Basket2))/length(Basket1)
    
  })
}
TO_Long = Turnover(Data[Basket==3,])
TO_Short = Turnover(Data[Basket==1,])
TO_Long
TO_Short
mean(TO_Short)
mean(TO_Long)
#--------------

Data[, GICS1 :=substr(GICS,1,2)]
Data[substr(GICS,1,4)=='4040',GICS1:='60']
load('GICS.rda')
GICS_Des
setkey(GICS_Des,GICS1)
setkey(Data,GICS1)
Data=GICS_Des[Data]

SctWgt_BM=Data[, .(BM=sum(WGT,na.rm = T)), by=.(DT,Sector)]
SctWgt_L=Data[Basket==3, .(Long=sum(WGT,na.rm = T)), by=.(DT,Sector)]
SctWgt_S=Data[Basket==1, .(Short=sum(WGT,na.rm = T)), by=.(DT,Sector)]
SctWgt_L[, Long :=Long/sum(Long),by=DT]
SctWgt_S[, Long :=Short/sum(Short),by=DT]

setkey(SctWgt_BM,DT,Sector)
setkey(SctWgt_L,DT,Sector)
SctWgt=SctWgt_L[SctWgt_BM]

setkey(SctWgt,DT,Sector)
setkey(SctWgt_S,DT,Sector)
SctWgt=SctWgt_S[SctWgt]
SctWgt=SctWgt[!is.na(Sector), ]
SctWgt[, ':='(Long_rel=Long-BM,Short_rel=Short-BM)]

SctWgt
SctWgt1=melt(SctWgt,id.vars=c('DT','Sector'),measure.vars=c('Long_rel', 'Short_rel'))
setnames(SctWgt1,old = c('variable', 'value'), new=c('Basket', 'Relative.Wgt'))
p=ggplot(data=SctWgt1,aes(x=DT, y= ))

p=ggplot(data=SctWgt1, aes(x=DT, y=Relative.Wgt))
p=p+geom_bar(aes(fill=Sector), stat='identity',position='stack',size=1)+facet_wrap(~Basket, nrow=2)
p=p+theme_bw(base_family='Times')
print(p)

Data[,Basket1:=fractile(ComP,NoBas), by=.(Sector,DT)]
Perf_s=Data[, .(Perf=sum(WGT*RTNM,na.rm=T)/sum(ifelse(is.na(RTNM),0,WGT))), by=.(DT,Basket,Sector)]

setkey(Perf_s,DT,Sector)
setkey(SctWgt_BM,DT,Sector)
Perf_s=SctWgt_BM[Perf_s]
Perf_s=Perf_s[!is.na(Sector), ]

Perf2=Perf_s[, sum(Perf*BM, na.rm = T)/sum(ifelse(is.na(Perf),0,BM)), by=.(DT,Basket)]

setkey(BMPerf,DT)
setkey(Perf2,DT)
Perf2=BMPerf[Perf2]
Perf2[,RelRtn :=V1-BM]
Perf2=dcast(Perf2, DT~Basket, value.var='RelRtn')
setnames(Perf2,c('DT','Low','Mid','High','NoData'))
Perf2[, LS:=High-Low]
Perf2=melt(Perf2,id.vars='DT')

ExRtn2 = Perf2[,12*mean(value,na.rm = T),by = variable]
RskAdjRtn2 = Perf2[,sqrt(12)*mean(value,na.rm = T)/sd(value,na.rm = T),by = variable]
data.frame(ExRtn2,RskAdjRtn2)

setorder(Perf2,DT)
CumP2 = Perf2[,.(DT = DT,CumPerf = cumsum(value)),by=variable]
CumP2 = CumP2[!is.na(CumPerf),]
setnames(CumP2,c('Basket','Date','Cum.Perf'))
CumP[,Strategy:='Non Sector Neutral']
CumP2[,Strategy:='Sector Neutral']
CumP = rbind(CumP, CumP2)
CumP2
p = ggplot(data=CumP[Basket=='LS',],aes(x=Date,y=Cum.Perf))
p = p + geom_line(aes(group=Strategy,color=Strategy),size=1)
p = p +theme_bw(base_family = 'Times')
print(p)