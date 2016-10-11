ToMillisecond<-function(date,date_format,origin){
  #This function is to transfer date to milliseconds.
  if(is.null(date)){
    return(NULL)
  }else{
    as.numeric(as.Date(date, origin, format=date_format))* 86400000  
  }
}

library(data.table)
#"marked.data" function use "data.table" package to process the data.
marked.data<-function(data,
                      start.dates,
                      end.dates=NULL,
                      date_format="%Y-%m-%d",
                      origin="1970-01-01"){
  #This function is to process the raw data from searchWB{RWildbook} function for process.data{marked} function.
  #data, the raw dataset from searchWB{RWildbook} function.
  #start.dates, a character vector of dates which are the start dates of the capture occasions.
  #end.dates, a character vector of dates which are the end dates of the capture occasions.
  #end.dates=NULL(default) means the end date of one capture occasion is the start point of the next capture occasion.
  #date_format sets the format of start.dates and end.dates.
  #origin sets the origin when transfering dates to milliseconds.
  
  #step 1. pull the needed variable from the raw data set.
  data <- data.frame(data$individualID,data$dateInMilliseconds)
  data <- data[order(data[,1]),]
  ##abstract the encounter sighted date and individual ID.
  
  #Step 2. Transfer begin.date, end.date milliseconds.
  start.dates <- ToMillisecond(date=start.dates, date_format=date_format, origin=origin)
  end.dates <- ToMillisecond(date=end.dates, date_format=date_format, origin=origin)
  if(is.null(end.dates)) end.dates <- c(start.dates[-1],ToMillisecond(date=format(Sys.Date(),date_format),date_format=date_format,origin=origin))
  
  #Step 3. Check the validity of capture time interval.
  if(!length(start.dates)==length(end.dates)){
    warning("start.dates and end.dates should have the same length")
    break()
  }
  if(any((end.dates-start.dates)<0)){
    warning("end.dates should be dates after start.dates")
    break()
  }
  
  #Step 4. generate the capture history of each individual from the encounter information.
  index1 <- outer(data[,2],start.dates,FUN=">")
  index2 <- outer(data[,2],end.dates,FUN="<")
  e.ch <- index1*index2 #generate "ch" for each encounter
  
  #Step 5. turn encounter capture history to individual capture history
  ch <- data.table(e.ch, individualID=data[,1])
  myfun<-function(x) as.numeric(any(x==1))
  mark.data <- ch[,lapply(.SD,myfun), by=individualID] #done!
  return(mark.data[,.(individualID,ch=apply(mark.data[,varname,with=FALSE],1,paste0,collapse=""))])
  
}

#example:
data1 <- searchWB(searchURL = "http://xinxin:changeme@whaleshark.org/api/jdoql?SELECT FROM org.ecocean.Encounter WHERE individualID == 'A-001'")
data2 <- searchWB(username="xinxin",
                  password="changeme",
                  baseURL ="whaleshark.org",
                  object="Encounter",
                  individualID=c("A-001","A-002","A-003"))

start.dates1 <- paste0(1998:2016,"-01-01")
end.dates1 <- paste0(1998:2016,"-04-01")
marked.data1.1 <- marked.data(data = data1,
                              start.dates = start.dates1,
                              end.dates=NULL,
                              date_format="%Y-%m-%d",
                              origin="1970-01-01")
marked.data1.2 <- marked.data(data = data1,
                              start.dates = start.dates1,
                              end.dates  = end.dates1,
                              date_format = "%Y-%m-%d",
                              origin = "1970-01-01")
marked.data2.1 <- marked.data(data = data2,
                              start.dates = start.dates1,
                              end.dates=NULL,
                              date_format="%Y-%m-%d",
                              origin="1970-01-01")
marked.data2.2 <- marked.data(data = data2,
                              start.dates = start.dates1,
                              end.dates  = end.dates1,
                              date_format = "%Y-%m-%d",
                              origin = "1970-01-01")
