# Packages
library(readxl)
library(dplyr)
library(stringr)

# Ophalen data en wegschrijven als R-bestanden
ankervakken_2017Q1 <- read_xlsx("Bronnen/schepenankervakkenq1.xlsx", sheet="All")
ankervakken_9d <- read.csv2("Bronnen/20170401-0409.csv", sep = ",",stringsAsFactors=FALSE)
afstanden_9d_combis <- read.csv2("Bronnen/distances201704010409.csv", sep = ",", stringsAsFactors=FALSE)
ship_features <- read.csv2("Bronnen/Dataset Patrick.csv", sep = ",", stringsAsFactors=FALSE)

# Corrigeren voor inlezen als Date Time
afstanden_9d_combis$A <- as.POSIXct(afstanden_9d_combis$A)
ankervakken_9d$Starttime <- as.POSIXct(ankervakken_9d$Starttime)
ankervakken_9d$Updatetime <- as.POSIXct(ankervakken_9d$Updatetime)
ship_features$DateOfLastVisit <- as.POSIXct(ship_features$DateOfLastVisit)
ship_features$ShipKeelLayingDate <- as.POSIXct(ship_features$ShipKeelLayingDate)

# Corrigeren voor MMSI correct inlezen
ankervakken_9d$MMSI<- substr(ankervakken_9d$MMSI,1,9)
#Correctie voor IMO correct inlezen
ankervakken_9d$IMO<- str_sub(ankervakken_9d$IMO,1,-3)

# Uniek maken ship features o.b.v. IMO-nr (eerst volgnummers verwijderen)
ship_features <-
  ship_features %>%
  select(-X) %>%
  arrange(IMO) %>%
  mutate(double = if_else(IMO == lag(IMO), 1, 0)) %>%
  filter(double != 1) %>%
  select(-double)

save(ankervakken_2017Q1, file="Data/1_reading_cleaning/ankervakken_2017Q1.Rda")
save(ankervakken_9d, file="Data/1_reading_cleaning/ankervakken_9d.Rda")
save(afstanden_9d_combis, file="Data/1_reading_cleaning/afstanden_9d_combis.Rda")
save(ship_features, file="Data/1_reading_cleaning/ship_features.Rda")

# Ankervakken 9 dagen uniek maken op combinatie schip X dag (voor visualisatie in de tijd)
ankervakken_9d_schip_X_dag <-
  ankervakken_9d %>%
  mutate(dag = substr(Updatetime,1,10)) %>%
  filter(Name != "") %>%
  mutate(IMO = substr(IMO,1,7)) %>%
  group_by(Name, dag) %>%
  arrange(Updatetime) %>%
  summarise(IMO         = first(IMO),
            Lat         = first(Lat),
            Lon         = first(Lon),
            MMSI        = first(MMSI),
            Schiptype   = first(Schiptype),
            Destination = first(Destination))

# Ankervakken 9 dagen uniek maken op schip (voor aanmaken basisbestand met kenmerken schepen)
ankervakken_9d_uniek_schip <-
  ankervakken_9d %>%
  mutate(IMO = substr(IMO,1,7)) %>%
  arrange(MMSI, desc(IMO), desc(Name)) %>%
  group_by(MMSI) %>%
  summarise(IMO  = first(IMO),
            Name = first(Name))

# Wegschrijven data 9 dagen per schip
save(ankervakken_9d_schip_X_dag, file="Data/1_reading_cleaning/ankervakken_9d_schip_X_dag.Rda")

# Wegschrijven data 9 dagen per schip als csv t.b.v. Power BI
write.csv2(ankervakken_9d_schip_X_dag, file="Data/1_reading_cleaning/ankervakken_9d_schip_X_dag.csv", row.names=TRUE)

# Wegschrijven data unieke schepen
save(ankervakken_9d_uniek_schip, file="Data/1_reading_cleaning/ankervakken_9d_uniek_schip.Rda")

# Lijst maken en wegschrijven van unieke MMSI nummers in dataset, om te gebruiken voor bouwen scheepsDB
MMSIUniek_9d <- (unique(ankervakken_9d$MMSI))
write.csv2(MMSIUniek_9d, file="Data/1_reading_cleaning/MMSIUniek_9d.csv", sep =",", row.names=FALSE)

