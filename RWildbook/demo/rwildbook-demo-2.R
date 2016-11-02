## Load packages
library(RWildbook)
 library(marked)

 ## Extract data for individual A-001 through A-099
 data1 <- searchWB(username="xinxin",
                   password="changeme",
                   baseURL ="whaleshark.org",
                   object="Encounter",
                   individualID=paste0("A-0",rep(0:9,rep(10,10)),rep(0:9,10))[-1])

 ## Define start and end dates of capture occasions
 start.dates1 <- paste0(1998:2016,"-01-01") #Define the start.date value
 end.dates1 <- paste0(1998:2016,"-04-01") #Define the end.date value

 ## Format data for use in marked
 markedData1.1 <- markedData(data = data1,
                              varname_of_capturetime = "dateInMilliseconds",
                              varlist = c("individualID"),
                              start.dates = start.dates1,
                              end.dates = NULL,
                              date_format = "%Y-%m-%d",
                              origin = "1970-01-01")

 ## Fit simple CJS model in marked
 markedData1.proc=process.data(markedData1.1,model="cjs",begin.time=1)
 markedData1.ddl=make.design.data(markedData1.proc)
 markedData1.cjs=crm(markedData1.proc,markedData1.ddl,model.parameters=list(Phi=list(formula=~time),p=list(formula=~time)))
