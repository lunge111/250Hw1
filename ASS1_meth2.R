#method2:use shell to extract only arrdely colum and use frequency table to computer the result
#add 2 functions to the FrequencyTable package to get sd and median
load<-function(){ #use shell command to read arrdelay colum of 1987-2007 and 2008-2012
  data1 = system("grep -v ArrDelay ~/data/assignment1/19*.csv ~/data/assignment1/200[0-7].csv| cut -f 15 -d ,", intern = TRUE)
  data2 = system("grep -v ARR_DELAY ~/data/assignment1/20[08-12]*.csv| cut -f 45 -d ,", intern = TRUE)
  data=c(data1, data2)
  data
}
time1=system.time(data<-load())

result2<- function(data){
  t=table(data)
  library(FrequencyTable)
  standv<-function(x ,..., na.rm = FALSE)
  {
    
    ids = as.numeric(names(x))
    if(na.rm) {
      i = is.na(ids)
      x = x[!i]
      ids = ids[!i]
    } 
    total = sum(x)
    ns = x/total
    sqrt(sum((ids-sum(ids * ns))^2 * ns))
  }

  t=IntegerFrequencyTable(t)
  c(mean(t, na.rm = TRUE),standv(t,na.rm=TRUE),median(t,na.rm=TRUE))
}
time2=system.time(stew<-result2(data))
summary=list(loadfiletime = time1,runtime=time2, results = c(mean = stew[1], sd = stew[2],median=stew[3]),
             system = Sys.info(),  session = sessionInfo() )
save(summary,file="result2.rda")

