## ---- eval=FALSE---------------------------------------------------------
#  data1 <- searchWB(username="xinxin",
#                    password="changeme",
#                    baseURL ="whaleshark.org",
#                    object="Encounter",
#                    individualID=c("A-001"))

## ---- eval=FALSE---------------------------------------------------------
#  data2<- searchWB(username="xinxin",
#                   password="changeme",
#                   baseURL ="whaleshark.org",
#                   jdoql="SELECT FROM org.ecocean.Encounter WHERE individualID == 'A-001'")

## ---- eval=FALSE---------------------------------------------------------
#  data3 <- searchWB(searchURL = "http://xinxin:changeme@whaleshark.org/api/jdoql?SELECT FROM org.ecocean.Encounter WHERE individualID == 'A-001'")

