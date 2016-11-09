## ------------------------------------------------------------------------
## Load packages
library(RWildbook)
library(marked)

## ------------------------------------------------------------------------
## Extract data for individual A-001 through A-099
data1 <- searchWB(username="xinxin",
                   password="changeme",
                   baseURL ="whaleshark.org",
                   object="Encounter",
                   individualID=paste0("A-0",rep(0:9,rep(10,10)),rep(0:9,10))[-1])

