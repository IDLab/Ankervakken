#### Overall script for 'Ankervakken', containing all project scripts in correct running order ####


### 1 - reading and cleaning ###

## Read csv-files, do some basic formatting and save as Rda-files 
#source("Scripts/1_reading_cleaning.R", echo=TRUE)

## Scrape ship information from Marine Traffic
#source("Scripts/1_ScrapeMMSIList.R", echo=TRUE)

## Merge all ship data to one dataset
#source("Scripts/1_merging.R", echo=TRUE)

## Pre-process data for analysis
#source("Scripts/1_preproc.R", echo=TRUE)


### 2 - analysis ###

## Network analysis of ships inside the ankervakken 
#rmarkdown::render("Scripts/2_netwerk_ankervakken.Rmd")


### 3 - presentation ###
