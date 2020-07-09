importODSAQ <-
  function(pollutant = c("all", "nox", "no2", "no", "pm10", "pm2.5", "o3"),
           siteid = c("203", "215", "463", "270", "500", "501"),
           dateFrom = "2018-01-01",
           dateTo = "2018-01-02",
           includeGeo = FALSE) {

    #---------------------------------VARIABLES-----------------------------
    wants <- c("tidyverse", "httr", "jsonlite")
    has   <- wants %in% rownames(installed.packages())
    if (any(!has))
      install.packages(wants[!has])
    lapply(wants, library, character.only = T)
    
    # dateFrom <- "2020-01-01"
    # dateTo <- "2020-01-02"
    # pollutant <- c("nox", "no2)
    # siteid = "all"
    # includeGeo = F
    
#--------------------------------ASSERT VALID DATA------------------------
dates <- c(dateFrom, dateTo)
#pollutant names
pollutant <- tolower(str_replace(pollutant, "[.]", "")) #make nice for the ODS
    
#any(pollutant == "no2")
 pollnames <- #vector of valid pollutant names
    c(
      "nox",
      "no2",
      "no",
      "co",
      "so2",
      "o3",
      "pm10",
      "pm25",
      "vpm10",
      "nvpm10",
      "nvpm25",
      "vpm25",
      "temp",
      "rh",
      "press"
    )
    if (any(pollutant != "all")) { #pollutant specified
     
      if (!any(pollutant %in% pollnames)) {
        stop(
          c(
            "You have entered one or more invalid pollutant names. Possible values are: ",
            paste0(pollnames, collapse = " "), ". Please also review the schema: https://opendata.bristol.gov.uk/explore/dataset/air-quality-data-continuous/information/?disjunctive.location"
          )
        )
      } #if any pollutant arguments don't match field names from the air-quality-data-continuous dataset, stop the function
    }
    
    # DATES
    #function to check whether dates are valid
  is.convertible.to.date <- function(x) all(!is.na(as.Date(x, tz = 'UTC', format = '%Y-%m-%d')))
    
    if(!is.convertible.to.date(dates)){
      stop("You have supplied a non - compliant date string: Should be in the format 2020-02-28")
    }
    
    if (any(as.Date(dates) > Sys.Date())) {
      stop("dateFrom and dateTo cannot be in the future")
    }# are they in the past?
    
    if (!is_logical(includeGeo)) {
      stop("includeGeo must be TRUE or FALSE") #CHECK LOGICAL VARIABLES ARE
    }
    
    if (as.Date(dateFrom) - as.Date(dateTo) >= 0) {
      stop("The end date precedes or is equal to the start date.")
    }
    # CREATE DATE QUERY PORTION OF ODS SQL STRING----------------
    date_query_string <-
      paste0("date_time IN ['",
             dateFrom,
             "T00:00:00' TO '",
             dateTo,
             "T23:59:00']")

#--------function to get query string for all the sites measuring a vector of pollutants-------
    #this uses the refine=key:value syntax in a list to specify the facets on which to filter
    #this works with this dataset (air-quality-monitoring-sites)
    #because the pollutants are in one field, but split by a processor in the #configuration of the dataset
    site_polls <- function(pollutant) {
      #if (any(siteid == "all")) {
        sel <- "siteid, pollutants"
        rows <- 100
        url <-
          "https://opendata.bristol.gov.uk/api/v2/catalog/datasets/air-quality-monitoring-sites/records"
        #just select continuous otherwise DT's are returned for NO2
        listvec <-
          c(paste0("pollutants:", toupper(pollutant)),
            "instrumenttype:Continuous (Reference)")
        lst <- as.list(listvec) #make a list for the refine section
        names(lst) <- rep("refine", length(listvec)) #of the query list
        qry_list_a <-
          list(select = sel, rows = rows) # the base qry list
        qry_add <- c(qry_list_a, lst) #append lists
        raq <- GET(url = url, query = qry_add) #get response object
        sites <- content(raq, as = "text") %>%
          jsonlite::fromJSON() %>%
          `[[`("records") %>%
          `[[`("record") %>%
          `[[`("fields") %>%
          pull() #extract DF from JSON \ lists and pull vector of sites that measure the pollutant
        (site_where_qry <-
            paste0("siteid = ", sites, collapse = " OR ")) #return the sites as a ODS SQL query string
      #}
    }
    
 #---------------------------------------------------------------------
 basefields <- "date_time as date, siteid, location"
    
    if(!includeGeo){
      (allpolls <- paste0(pollnames, collapse = ", "))
      (polls <-  paste0(pollutant, collapse = ", "))
        } else {
          (allpolls <- paste(paste0(pollnames, collapse = ", "), "geo_point_2d", sep = ", "))
          (polls <-  paste(paste0(pollutant, collapse = ", "), "geo_point_2d", sep = ", "))
      end_col_types <- paste0(paste0(rep("n", length(pollutant)), collapse = ""), "c", collapse = "")
        } 
    
    
    if (any(pollutant == "all")) {
      (select_str <-
        paste(basefields, allpolls, sep = ", "))
      end_col_types = "nnnnnnnnnnnnnnn"
      #-------------------------------------------
      if (any(siteid == "all")) {
        #all siteids all pollutants
        where_str <- date_query_string
      } else {
        #siteids specified.  all pollutants
        where_str <-
          paste0(date_query_string,
                 " AND (",
                 paste0("siteid=", siteid, collapse = " OR "),
                 ")")
      }
      #--------------------------------------------
    } else {
      #pollutants specified
      
      (select_str <- paste(basefields, polls, sep = ", "))
      end_col_types <- paste0(rep("n", length(pollutant)), collapse = "")
      #---------------------------------------
      if (any(siteid == "all")) {
        #pollutants specified     all sites
        where_str <-
          paste0(date_query_string, " AND (", site_polls(pollutant), ")")
      } else {
        #pollutants and siteids specified
        where_str <-
          paste0(date_query_string,
                 " AND (",
                 paste0("siteid=", siteid, collapse = " OR "),
                 ")")
      }
#------------------------------------------
    }
    #--------------------------DEFINE BASE URL AND QUERY STRING FOR API CALL-------------
    base_url <-
      "https://opendata.bristol.gov.uk/api/v2/catalog/datasets/air-quality-data-continuous/exports/csv"
    qry_list <-
      list(select = select_str,
           where = where_str,
           sort = "-date_time")
    # GET the response object
    r <- GET(url = base_url, query = qry_list)
    
    #retrieve the csv data from the response object and change names to be openair compliant
    if (!http_error(r)) {
      aqdata <- content(r, as = "text") %>%
        read_delim(
          na = "",
          delim = ";",
          col_types = paste0("Tic", end_col_types, collapse = "")
          # col_types = list(siteid = col_integer(),
          #                  vpm10 = col_number(),
          #                  nvpm10 = col_number(),
          #                  nvpm25 = col_number(),
          #                  vpm25 = col_number(),
          #                  nvpm25 = col_number(),
          #                  co = col_number(),
          #                  so2 = col_number(),
          #                  o3 = col_number(),
          #                  temp = col_number(),
          #                  rh = col_number(),
          #                  press = col_number())
        ) 
      names(aqdata) <- aqdata %>%
        names() %>%
        str_replace_all(pattern = "pm25", replacement = "pm2.5")
      return(aqdata)
    } else {
      return(FALSE)
    }
    
  }

aq_data <- importODSAQ(
  siteid = "215",
  pollutant = "all",
  dateFrom = "2019-01-01",
  dateTo = "2019-02-01"
)
