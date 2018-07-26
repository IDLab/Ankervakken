#Dit script is bedoeld om scheepsinformatie op te halen bij MarineTraffic.com
#INPUT is een datafile met unieke MMSI en IMO nummers in de AIS data van de ankervakken
#Output is een .csv met daarin de eigenschappen behorend bij deze IDs.

library(xml2)
library(rvest)

#a function to call any webaddress
#There is a loop involved to diminish '500' errors
callAddress <- function (webaddress) {
  message('trying url:', webaddress)
  breakloop <- FALSE
  i <- 0
  while (!breakloop) {
    #Pause system to pretend realworld user
    Sys.sleep(runif(1, 1,1.5))
    page<- try(readLines(webaddress), silent = TRUE)
    #if an error occurs exit the function
    if(!inherits(page,"try-error")) breakloop <- TRUE
    #if 404 then exit this function
    if(grepl("404 Not Found",capture.output(warnings())[3])) breakloop <- TRUE
    i <- i + 1
    if (i == 10) breakloop <- TRUE
  }
  closeAllConnections()
  message('used iterations:', i, ' out of 10')
  return(page)
}

extractData <- function (variable) {
  line <- page2[grep(variable, page2)]
  #Have to catch errors here, sometimes the varibles are not found, which is weird as the webaddress is working fine and page exists...
  if (identical (line, character(0)) | (length(line) == 1 & variable == ">IMO:")) {
    tmp <- c(rep("Data not found",2))
    value <- list(tmp,tmp)
  } else
  {  value <- strsplit(line, paste0(variable, ' <b class=\"text-primary text-dark\">'))
  }
}

#read data created from blending_ankervakken.R
temp <- read.csv2("Data/1_reading_cleaning/MMSIUniek_9d.csv", stringsAsFactors=FALSE)
#Push into vector
MMSIUniek_9d <- as.vector(as.integer(temp$x))

#temp selection of subset
#MMSIUniek_9d<- MMSIUniek_9d[15:20]

#columns to be read
vars <- c("IMO", "Name", "Vessel Type", "MMSI", "Call Sign", "Flag", "AIS Vessel Type", "Gross Tonnage", "Deadweight", 'Length Overall x Breadth Extreme', 'Year Built', 'Status')
PageShipResult <- as.data.frame(matrix(NA, nrow = length(MMSIUniek_9d), ncol = length(vars)))
colnames(PageShipResult) <- vars
rootShipAddress<- 'https://www.marinetraffic.com/en/ais/details/ships/mmsi:'

# Now loop through the IMO# per page
for (i in 266:length(MMSIUniek_9d)) {
  #Construct webaddress for a single ship
  webaddress <- paste0(rootShipAddress, MMSIUniek_9d[i])
  #call ship page if not testing
  if (TRUE) {page2 <- callAddress(webaddress)}
  if (grepl("cannot open the connection", page2)) {
    PageShipResult[i,] <- "webpage not found"
    PageShipResult$MMSI[i] <- MMSIUniek_9d[i]
  } else
  {
    #grab relevant data from page2
    #ShipName, Vesseltype and IMO number have a dedicated function 'extractData'
    ShipName <- strsplit(extractData('>Name:')[[1]][2], '</b></span>')
    PageShipResult$Name[i]<- ShipName[[1]][1]
    #Vessel Type 
    VesselType <- strsplit(extractData('>Vessel Type:')[[1]][2], '</b></span>')
    PageShipResult$'Vessel Type'[i] <- VesselType[[1]][1]
    #IMO
    IMO <-substr((extractData('>IMO:')[[2]][2]),1,7)
    PageShipResult$IMO[i] <- IMO[[1]][1]
    
    #part of info available generic
    for (j in 4:length(vars)) {
      if (j == 2) {text <- paste0('<span>', vars[j])
      } else {text <- paste0('<span>' ,vars[j], ': </span>')}
      line <- grep(text, page2)
      if (length(line) == 0) {PageShipResult[i,j] <- "failed retrieval"
      } else {PageShipResult[i,j] <-gsub(".*[>]([^.]+)[<].*", "\\1", page2[min(line)+1])}
    }
  }
  print(PageShipResult[i,])
}

filename <-paste0('Data/1_reading_cleaning/ShipSpecsMT','.csv')
write.csv(PageShipResult, file = filename)
