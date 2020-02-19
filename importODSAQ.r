importODSAQ <-
  function(pollutant = c("all", "nox", "no2", "no", "pm10", "pm2.5", "o3"),
           siteid = c("203", "215", "463", "270", "500", "501"),
           dateFrom = "2018-01-01",
           dateTo = "2018-01-02",
           includeGeo = FALSE,
           includeMet = FALSE) {
#function to easily import continuous air quality data from the opendatasoft
#portal, using similar syntax to that used in openair functions, e.g. importAURN
#
#Returns a sorted, keyed, data.table either with met data or without depending on status of includeMet.
#includeGeo determines whether or not the geo_point_2d field is returned which can be helpful for e.g. mapping
#
# checks for invalid arguments as far as practicable!

#----------------------------LOAD \ INSTALL LIBRARIES IF NEEDED----------------
wants <- c("stringr", "data.table", "assertive", "fasttime")
has   <- wants %in% rownames(installed.packages())
if (any(!has))
    install.packages(wants[!has])
lapply(wants, library, character.only = T)
#set variables for function testing
# dateFrom  <-  "2019-12-01"
# dateTo <-  "2019-12-02"
# includeGeo <-  F
# includeMet <-  F
# siteid <- "all"
# pollutant = c("o3", "nox", "pm10")
#--------------------------------SET URLS AND VARIABLES-------------------
headurl_aq <-
    "https://opendata.bristol.gov.uk/explore/dataset/air-quality-data-continuous/download/?format=csv&disjunctive.location=true&q="

dt_query_url <- "&q=date_time:%5B"
tailurl = "%5D&timezone=GMT&use_labels_for_header=false&csv_separator=%3B"
dateFrom_url <- paste0(dateFrom, "T00:00:00Z")
dateTo_url <- paste0(dateTo, "T23:59:59Z")
to_url <- "+TO+"

dates <- c(dateFrom, dateTo)

#---------CHECKING ARGUMENTS WITH PACKAGE ASSERTIVE-----------------

assert_all_are_date_strings(dates, format = "%Y-%m-%d") # are they dates?

if (any(as.Date(dates) > Sys.Date())) {
    stop("dateFrom and dateTo cannot be in the future")
}# are they in the past?

if (!is_logical(c(includeGeo, includeMet))) {
    stop("includeGeo and includeMet must be TRUE or FALSE") #CHECK LOGICAL VARIABLES ARE
}

if (as.Date(dateFrom) - as.Date(dateTo) >= 0) {
    stop("The end date precedes or is equal to the start date.")
}
pollsites_url <- paste0("https://opendata.bristol.gov.uk/explore/dataset/air-quality-monitoring-sites/download/?format=csv&disjunctive.pollutants=true", paste0("&refine.pollutants=", toupper(pollutant), collapse =""), "&timezone=Europe/London&lang=en&use_labels_for_header=false&csv_separator=%3B")

pollsites <- fread(pollsites_url)
if (siteid != "all") {
    #siteids are selected - coerce to integer
    site_id <- as.integer(siteid)
    assert_is_integer(site_id)

#---------------RETRIEVE VECTOR OF SITEIDS FOR POLLUTANTS SELECTED AND CREATE FACET url PART
   p_sites <- pollsites[siteid %in% site_id, siteid]
    #download a vector of the sites offering the selected pollutants
    #from air-quality-monitoring-sites
   # subset with the specified siteid and return as a vector
    #and create an API filter for those sites
    #to avoid D/L of sites where that pollutant isn't measured
    
} else {
    
    p_sites <-  pollsites[, siteid]
    #just select the sites where the pollutant is measured
    
}
siteid_url <- paste("siteid%3D", p_sites, sep = "", collapse = "+OR+") 
#and make the url portion
    #construct this part of the url by D\L subset of air-quality-monitoring-sites dataset and parsing to the filter conditions required by air-quality-data-continuous
    
#--------------------DEFINING SELECT STRING FOR FREAD (POLLUTANTS)-------------------
if (any(pollutant != "all")) { #pollutant specified
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
    if (any(!(tolower(str_replace(
        pollutant, "[.]", ""
    )) %in% pollnames))) {
        stop(
            c(
                "You have entered or more invalid pollutant names. Possible values are: ",
                paste0(pollnames, collapse = " ")
            )
        )
    } #if any pollutant arguments don't match field names from the air-quality-data-continuous dataset, stop the function
    
    if (includeGeo) {
        geo = "geo_point_2d"
        selectedCols <-
            tolower(str_replace(
                c("date_time", "siteid", "location", pollutant, geo),
                "[.]",
                ""
            ))
    } else{
        selectedCols <-
            tolower(str_replace(
                c("date_time", "siteid", "location", pollutant),
                "[.]",
                ""
            ))
    }
    
} else{
    selectedCols <- ""
}
#all pollutants are selected so no select string required for fread
#--------------------------READ THE AQ DATA-----------------------------
(aq_url <-
    paste0(headurl_aq,
           siteid_url,
           dt_query_url,
           dateFrom_url,
           to_url,
           dateTo_url,
           tailurl))
#construct the url for the download csv

if (selectedCols[1] == "") {
    #all pollutants
    aq_data_DT <- fread(aq_url)
} else {
    #selected pollutants
    
aq_data_DT <- fread(aq_url, select = selectedCols) #money shot
    
}
aq_data_DT[, date := fastPOSIXct(date_time, "Etc/GMT-0")][, date_time := NULL]
if ("pm25" %in% names(aq_data_DT)) {
    setnames(aq_data_DT, "pm25", "pm2.5")
}

#convert and rename date to POSIX

#----------------------READ AND PROCESS MET DATA IF REQUIRED------------

if (includeMet) {
    headurl_met <-
        "https://opendata.bristol.gov.uk/explore/dataset/met-data-bristol-lulsgate/download/?format=csv&q=date_time:%5B"
    met_url <-
        paste0(headurl_met, dateFrom_url, to_url, dateTo_url, tailurl)
    met_DT <- fread(met_url,
                    select = c("date_time", "ws", "wd", "temp", "rh"))[, lapply(.SD, mean, na.rm = T),
                                                                       by = .(date = floor_date(fastPOSIXct(date_time), "hour")),
                                                                       .SDcols = c("ws", "wd", "temp", "rh")]#timeaverage and process met data
    # setkey(, date, siteid, location)
    setcolorder(aq_data_DT[met_DT, on = "date"],
                neworder = c("date", "siteid", "location"))#join met data to aqdata and reorder columns before returning
    
} else{
    setcolorder(aq_data_DT, neworder = c("date", "siteid", "location"))
}
}

# DT <- importODSAQ(pollutant = "pm2.5",
#                   siteid = "all",
#                   dateFrom = "2018-01-01",
#                   dateTo = "2018-01-02")
# 
# rm(importODSAQ)


