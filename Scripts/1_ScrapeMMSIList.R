#Dit script is bedoeld om scheepsinformatie op te halen bij MarineTraffic.com
#INPUT is een datafile met unieke MMSI en IMO nummers in de AIS data van de ankervakken
#Output is een .csv met daarin de eigenschappen behorend bij deze IDs.

###load the relevant libraries
library(xml2)
library(rvest)
library (stringr)

###Define some functions
#a function to call any webaddress
#There is a loop involved to diminish '500' errors
callAddress <- function (webaddress) {
  message('trying url:', webaddress)
  breakloop <- FALSE
  i <- 1
  while (!breakloop) {
    #Pause system to pretend realworld user
    Sys.sleep(runif(1, 5,15))
    message('Attempt ', i, ' out of 10')
    page<- try(readLines(webaddress), silent = TRUE)
    #if an try-error occurs exit the function
    if(!inherits(page,"try-error")) breakloop <- TRUE
    #if 404 warning(webadress does not exist) then exit the loop 
    if(!inherits(page,"404 Not Found")) break
    i <- i + 1
    if (i >= 10) breakloop <- TRUE
  }
  closeAllConnections()
  return(page)
}

#This function is used to take the given variable from the webpage code. It doesn't work for all variables
extractData <- function (variable) {
  line <- page2[grep(variable, page2)]
  #Have to catch errors here, sometimes the variables are not found, which is weird as the webaddress is working fine and page exists...
  if (identical (line, character(0)) | (length(line) == 1 & variable == ">IMO:")) {
    tmp <- c(rep("Data not found",2))
    value <- list(tmp,tmp)
  } else
  {  value <- strsplit(line, paste0(variable, ' <b class=\"text-primary text-dark\">'))
  }
}

#clever function that extracts numbers only from a string
Numextract <- function(string){
  unlist(regmatches(string,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",string)))
}

###Start the script
#read data created from blending_ankervakken.R
temp <- read.csv2("Data/1_reading_cleaning/MMSIUniek_9d.csv", stringsAsFactors=FALSE)
#Push into vector
MMSIUniek_9d <- as.vector(as.integer(temp$x))

#columns to be read
vars <- c("IMO", "Name", "Vessel Type", "MMSI", "Call Sign", "Flag", "ENI", "AIS Vessel Type", "Gross Tonnage", "Deadweight", 'Length Overall x Breadth Extreme', 'Year Built', 'Status')
PageShipResult <- as.data.frame(matrix(NA, nrow = length(MMSIUniek_9d), ncol = length(vars)))
colnames(PageShipResult) <- vars
rootShipAddress<- 'https://www.marinetraffic.com/en/ais/details/ships/mmsi:'

# Now loop through the IMO# per page
for (i in 1:length(MMSIUniek_9d)) {
  #Construct webaddress for a single ship
  webaddress <- paste0(rootShipAddress, MMSIUniek_9d[i])
  #call ship page if not testing
  if (TRUE) {page2 <- callAddress(webaddress)}
  #conditional statement to capture failure of finding page due to failed connection
  if (grepl("cannot open the connection", page2)) {
    PageShipResult[i,] <- "webpage not found"
    PageShipResult$MMSI[i] <- MMSIUniek_9d[i]
  } else
  {
    #grab relevant data from page2
    #dedicated code for ShipName
    regel<- str_subset(page2,"ls_vessel")
    start <- str_locate(regel, "vessel:")[2]
    end <- str_locate(regel, '\",\"URL')[1]
    PageShipResult$Name[i] <- str_sub(regel, start+1,end-1)
    #ShipName, Vesseltype and IMO number have a dedicated function 'extractData'
    #ShipName <- strsplit(extractData('>Name:')[[1]][2], '</b></span>')
    #PageShipResult$Name[i]<- ShipName[[1]][1]
    #Vessel Type 
    VesselType <- strsplit(extractData('>Vessel Type:')[[1]][2], '</b></span>')
    PageShipResult$'Vessel Type'[i] <- VesselType[[1]][1]
    #IMO
    IMO <-substr((extractData('>IMO:')[[2]][2]),1,7)
    PageShipResult$IMO[i] <- IMO[[1]][1]
    
    #part of info available generic
    for (j in 4:length(vars)) {
      if (j == 2) {text <- paste0('<span>', vars[j])
      } else {text <- gsub("[[:space:]]", "", paste0('<span>' ,vars[j], ': </span>'))}
      line <- grep(text, gsub("[[:space:]]", "", page2))
      if (length(line) == 0) {PageShipResult[i,j] <- "failed retrieval"
      } else {PageShipResult[i,j] <-as.character(gsub(".*[>]([^.]+)[<].*", "\\1", page2[min(line)+1]))}
    }
  }
  #Processing results for ship dimensions
 
  ShipSize <- Numextract(PageShipResult$`Length Overall x Breadth Extreme`[i])
  PageShipResult$Length[i] <- if (length(ShipSize) == 2) {ShipSize[1]} else {"failed retrieval"}
  PageShipResult$Breadth[i] <- if (length(ShipSize) == 2) {ShipSize[2]} else {"failed retrieval"}
  #Drop a line to check results
  print(PageShipResult[i,])
}

#postprocessing results 
# fix deadweight
PageShipResult$Deadweight <- as.numeric(str_sub(PageShipResult$Deadweight,1,-2))

#save results
if (FALSE) {
  filename <-paste0('Data/1_reading_cleaning/ShipSpecsMT','.csv')
  write.csv(PageShipResult, file = filename)
}


