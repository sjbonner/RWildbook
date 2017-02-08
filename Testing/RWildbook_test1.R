## Prompt for password
cat("Enter your password:")
psswd <- readline()

library(RWildbook)

# ---- eval=FALSE---------------------------------------------------------
 data1 <- searchWB(username="username",
                   password=psswd,
                   baseURL ="whaleshark.org",
                   object="Encounter",
                   individualID=c("A-001"))

# ---- eval=FALSE---------------------------------------------------------
 data2<- searchWB(username="username",
                  password=psswd,
                  baseURL ="whaleshark.org",
                  jdoql="SELECT FROM org.ecocean.Encounter WHERE individualID == 'A-001'")

# ---- eval=FALSE---------------------------------------------------------
 data3 <- searchWB(searchURL = paste0("https://sbonner:",psswd,"@whaleshark.org/rest/jdoql?SELECT FROM org.ecocean.Encounter WHERE individualID == 'A-001'"))

# ----eval=FALSE----------------------------------------------------------
 data3 <-
   searchWB(username = "username",
            password = psswd,
            searchURL = "http://www.whaleshark.org/rest/jdoql?SELECT FROM org.ecocean.Encounter WHERE individualID == 'A-001'")

## ------------------------------------------------------------------------
data("vignette_1_data",package="RWildbook")
head(vignette_1_data)[,c("location","year","month","day")]

