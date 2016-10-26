#demo for pulling data from the Wildbook interface with RWildbook package
#Pulling data of all the encounters of individual "A-001" in whaleshark.org
#All the datasets below are searching for the same set of data which is all the encounter of individual "A-001" in "whaleshark.org".

#Case 1. You can filter variables directly in the function "searchWB".
data1 <- searchWB(username="xinxin",
                  password="changeme",
                  baseURL ="whaleshark.org",
                  object="Encounter",
                  individualID=c("A-001"))

#Case 2. If you have the JDOQL query for the search, You can ignore the filter arguments.
data2 <- searchWB(username="xinxin",
                  password="changeme",
                  baseURL ="whaleshark.org",
                  jdoql="SELECT FROM org.ecocean.Encounter WHERE individualID == 'A-001'")

#Case 3. If you have a searchURL on hand, you can directly open the URL with function "searchWB".
data3 <- searchWB(searchURL = "http://xinxin:changeme@whaleshark.org/api/jdoql?SELECT FROM org.ecocean.Encounter WHERE individualID == 'A-001'")







