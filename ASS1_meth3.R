getwd()
setwd("~/data/assignment1")
fileName <- dir()
N=length(fileName)
a <- vector("list", 81)
library(FastCSVSample)
for(i in 1:81)
  a[[i]]=csvSample(fileName[i],100)
result3<-function(a)
{
  b=vector("integer",2700)
for(i in 1:21)
for(j in 1:100)
b[100*(i-1)+j]=strsplit(a[[i]][j], ",")[[1]][15]
for(i in 22:81)
  for(j in 1:10)
    b[10*(i-22)+j]=strsplit(a[[i]][j], ",")[[1]][15]
b=as.integer(b)
c(mean(b,na.rm=TRUE),sd(b,na.rm=TRUE),median(b,na.rm=TRUE))
}
time=system.time(stew<-result3(a))
summary=list(runtime=time, results = c(mean = stew[1], sd = stew[2],median=stew[3]),
             system = Sys.info(),  session = sessionInfo() )
summary
save(summary,file="result3.rda")
