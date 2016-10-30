#' Generate the search URL given the JDOQL query.
#'
#' This function helps users to generate the URL for data searching in the Wildbook framework with the account information of Wildbook, the URl of the desire wildbook and the JDOQL query.
#'
#' @param username The username in the Wildbook framework.
#'
#' @param password The password in the Wildbook framework.
#'
#' @param baseURL The URL represent the desire wildbook data base.
#'
#' @param jdoql The JDOQL string for data searching.
#'
#'
#' @examples
#' searchURL1 <- WBsearchURL(username="xinxin",
#'                           password="changeme",
#'                           baseURL="whaleshark.org",
#'                           jdoql="SELECT FROM org.ecocean.MarkedIndividual WHERE individualID == 'A-001'")

WBsearchURL <-
  function(username,
           password,
           baseURL,
           jdoql,
           method = NULL) {
    #This function is to call data from a wildbook site via JDO API
    #This function is for users who know JDOQL query language
    #The JDOQL query can be directly written by users
    #An example of "baseURL" is "whaleshark.org" (No "http://www.").
    
    if (method == "readLines") {
      searchURL <- paste0("http://",
                          username,
                          ":",
                          password,
                          "@",
                          baseURL,
                          "/api/jdoql?",
                          jdoql)
    }
    else if (method == "curl") {
      searchURL <- paste0("http://www.", baseURL,
                          "/api/jdoql?", jdoql)
    }
    else{
      stop(
        "You must supply the method of retrieving the data. Currently supported methods include readLines and curl.\n"
      )
    }
    return(as.character(searchURL))
  }
