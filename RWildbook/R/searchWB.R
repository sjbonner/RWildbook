#' Pull data from the Wildbook framework.
#'
#' This function allows users to pull data from the Wildbook framework in R.
#'
#' The \code{searchWB} function provides the main interface to the Wilbook
#' framework and can be used in one of three ways. First, users may supply
#' filters based on the variables within the database. These filters are
#' combined in a single JDOQL statement which is then combined with the base
#' URL, username, and password to create the URL search string for data
#' extraction. Second, users may supply the JDOQL string, username and password,
#' and base URL as separate arguments. Finally, users may supply the search URL
#' directly.
#'
#' We envisage that most users will supply filters to create the search URL. The
#' other options allow users to easily repeat or modify previous searches and
#' enable advanced users familiar with the JDOQL API and internals of the
#' Wildbook framework to conduct more complex searches.
#'
#' \strong{Filtering Locations}
#'
#' Locations may be filtered with either location names or location ids.
#' Multiple location names can be given to the \code{location} argument.
#' Multiple location ids can be given to the \code{locationID} argument.
#' In this case the search will return all objects (encounters or individuals)
#' matching at least one of the locations.
#'
#' \strong{Filtering Dates}
#'
#' The \code{sighting_date} filter may be specified as a character vector of
#' either one or two elements representing dates. If one date is provided then
#' results will be filtered from 00:00:00 to 24:00:00 on that day. If two dates
#' are provided then results will be filtered from 00:00:00 on the first date to
#' 24:00:00 on the second date. By default, dates must be entered using the
#' "YYYY-MM-DD" format. Other formats may be used by specifying the value of
#' \code{date_format}. More details about the date format can be found in the help
#' page of \code{as.Date} The same rule can apply to the \code{encounter_submission_date},
#' \code{Date_of_birth} and \code{Date_of_death} filters.
#'
#' \strong{Defalut NULL value for filter arguments}
#'
#' The default value for some filter arguments are NULL. NULL value for a filter argument
#' returns data not filtering that argument.
#'
#' @param searchURL A character object of the URL for data searching in the Wildbook framework.
#'
#' @param username A character object of the username in the Wildbook framework.
#'
#' @param password A character object of the password in the Wildbook framework.
#'
#' @param baseURL A character object of the base URL represent the Wildbook data base.
#'
#' @param jdoql A character object of the JDOQL string for data searching.
#'
#' @param object A character object for defining the the search type. The value can be
#'   either "encounter" for the encounter search or "individual" for the individual search.
#'   The default value is "encounter" for encounter search.
#'
#' @param location A vector of character strings for searching encounters in locations
#'   containing the character strings.
#'
#' @param locationID A character vector for searching encounters in locations with
#'   specified locationID. Note that the location ID is case sensitive.
#'
#' @param sighting_date A character vector for filtering encounters which are
#'   sighted during a period of time. More information of the date argument can
#'   be found in the Detail section.
#'
#' @param encounter_submission_date A character vector for filtering encounters
#'   which are submitted during a period of time.
#'
#' @param date_format The format for all the arguments of date value.
#'
#' @param sex A character vector of maximum size of three representing the values
#'   for the sex filter. The value can be any combination of "male", "female"
#'   and "unknown". The default value is "sex = c("male", "female", "unknown")".
#'
#' @param status A character vector of maximum size of two representing the values
#'   for the encounter status. The value can be any combination of "alive" and
#'   "dead".
#'
#' @param measurement A numeric object sets the minimum individual measurement
#'   when searching in the Wildbook framework.
#'
#' @param individualID A character vector of individual ID for searching data
#'   of specified individual IDs. Note that the individual ID is case sensitive.
#'
#' @param encounterID A character vector for searching data of specific
#'   encounter ID. Note that the encounter ID is case sensitive.
#'
#' @param encounter_type A character vector of maximum size of three for
#'   searching data with specific encounter type. It can be any combination
#'   of â€œunapprovedâ€?, â€œapproved â€? and â€œunidentifiableâ€?.
#'
#' @param Date_of_birth A character vector for searching data of individual
#'   which is born during a period of time.
#'
#' @param Date_of_death A character vector for searching data of individual
#'   which died during a period of time.
#'
#' @param showURL logical. If TRUE(default) the function returns the search
#'   URL, otherwise the function will not return the search URL.
#'
#' @param showJDOQL logical. If FALSE(default) the function will not return
#'  the search JDOQL, otherwise the function returns the search JDOQL.
#'
#' @importFrom jsonlite fromJSON
#'
#' @export
#'
#' @examples
#' ## The following examples conduct the same search using the complete searchURL, JDOQL, or variable filters.
#'
#' data1 <- searchWB(searchURL = "http://xinxin:changeme@whaleshark.org/api/jdoql?SELECT FROM org.ecocean.Encounter WHERE individualID == 'A-001'")
#'
#' data2 <- searchWB(username="xinxin",password="changeme",baseURL ="whaleshark.org",jdoql="SELECT FROM org.ecocean.Encounter WHERE catalogNumber != null &&  (( dateInMilliseconds >= -189388800000 )&&( dateInMilliseconds <= 1483228740000 ))&&(( dwcDateAddedLong >= 1041379200000 )&&( dwcDateAddedLong <= 1483228740000 ))&&(( individualID == 'a-001' )) ")
#'
#' data3 <- searchWB(username="xinxin",password="changeme",baseURL ="whaleshark.org",object="Encounter",individualID=c("A-001"))
#'

searchWB <-
  function(searchURL = NULL,
           username = NULL,password = NULL,baseURL, jdoql = NULL,
           object = "encounter",
           location = NULL,locationID = NULL,
           sighting_date = c("1964-01-01","2016-12-31"),encounter_submission_date =
             c("2003-01-01","2016-12-31"), date_format = "%Y-%m-%d",
           sex = c("male","female","unknown"),status = c("alive","dead"),
           measurement = NULL,individualID = NULL,
           encounterID = NULL,encounter_type = NULL,
           Date_of_birth = NULL,Date_of_death = NULL,
           showJDOQL = FALSE,
           showURL = TRUE) {
    #This function is to get data from the Wildbook framework via the JDO API
    #For user of different level
    #Step 1. Define(generate) the JDOQL query
    if (is.null(searchURL) && is.null(jdoql)) {
      jdoql <- WBjdoql(
        object,
        location,locationID,
        sighting_date,encounter_submission_date, date_format,
        sex,status, measurement,individualID,
        encounterID,encounter_type
      )
    }
    #Step 2.Define(generate) the search URL
    if (is.null(searchURL)) {
      if (is.null(username) || is.null(password)) {
        warning("Lack of username and password")
        break
      }
      searchURL <- WBsearchURL(username,password,baseURL,jdoql)
    }
    #Step 3. Get data from Wildbook framework with the search URL

    if (.Platform$OS.type == "windows") {
      tmpdata <- readLines(searchURL,warn = FALSE)
      data <- fromJSON(tmpdata)
    }
    else{
      tmpfile <- tempfile()
      download.file(
        "http://xinxin:changeme@whaleshark.org/api/jdoql?SELECT FROM org.ecocean.Encounter WHERE individualID == 'A-001'",method =
          "wget",destfile = tmpfile
      )
      data <- fromJSON(readLines(tmpfile,warn = FALSE))
    }
    return(data)
    #Step 4. (Options)Show the search URL/JDOQL
    if(showURL==TRUE) cat(searchURL)
    if(showJDOQL==TRUE) cat(jdoql)
    
  }
