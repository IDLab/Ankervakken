
# Packages
library(dplyr)
library(ggplot2)
library(reshape2)
library(parallel)

set.seed(1)

# Data ophalen
load('Data/1_reading_cleaning/afstanden_9d_combis.Rda')
load('Data/1_reading_cleaning/ship_specs_complete.Rda')

# Combi-variabele met beide MMSI's maken
afstanden_9d_combis <- afstanden_9d_combis %>% mutate(MMSI_combi = paste0(MMSI1, "-", MMSI2))

# Sample van een x aantal combi's. Overschrijft de volledige set. Geen sample? Dan deze chunk niet runnen.
if (FALSE) {
  n_combi <- 10000
  afstanden_9d_combis <-
    afstanden_9d_combis %>%
    sample_n(n_combi)
}

### Tijd x afstand
#Geeft onderlinge afstand combinaties in de tijd weer
if (FALSE) {
  ggplot(afstanden_9d_combis, aes(x = time, y = Distance, color = MMSI_combi)) +
    geom_line(size = 1, show.legend = FALSE) +
    xlab("Datum + tijd") +
    ylab("Afstand")
}

# Minimale afstand met bandbreedte (percentage) in combinatie met tijdsduur wegschrijven in data frame
bandbreedte <- 2

#functie definieren die per schipcombi de juiste data over onderlinge afstand verzameld
fMinAfstand <- function (i) {
  #selecteer alleen de waardes voor een specifiek paar
  selectie_i <- afstanden_9d_combis %>% filter(MMSI_combi == unique(afstanden_9d_combis$MMSI_combi)[i])
  #filter gevonden waardes op minimum plus de waardes die binnen de opgegeven bandbreedte liggen
  min_afstand_tijd_i <-
    selectie_i %>%
    filter(Distance <= (1 + bandbreedte) * selectie_i[which.min(selectie_i$Distance),7])
  return(min_afstand_tijd_i)
}

#standaard lapply call
if (FALSE) {
  list_min_afstand_tijd<-lapply (1:length(unique(afstanden_9d_combis$Distance)), fMinAfstand)
}

#Deze code is om op windows parallel te draaien
if (FALSE) {
  no_cores <- detectCores()-1
  cl <- makeCluster(no_cores)
  clusterExport (cl, "afstanden_9d_combis")
  clusterExport (cl, "bandbreedte")
  clusterEvalQ(cl, library(dplyr))
  list_min_afstand_tijd<-parLapply (cl, 1:length(unique(afstanden_9d_combis$Distance)), fMinAfstand)
  stopCluster(cl)
}

#Deze code werkt voor parallel rekenen op Mac, niet voor Windowns
if (TRUE) {
  list_min_afstand_tijd<- mclapply(1:length(unique(afstanden_9d_combis$Distance)), fMinAfstand, mc.cores = getOption("mc.cores", 4L))
}

#lijst converteren naar dataframe
min_afstand_tijd<-do.call(rbind.data.frame, list_min_afstand_tijd)
#even schoonmaken van grotere datasets
rm(list_min_afstand_tijd)

#kolommen delta afstand en tijd vullen
min_afstand_tijd <-
  min_afstand_tijd %>%
  group_by(MMSI1, MMSI2) %>%
  summarise(t_start = min(time), t_eind = max(time), d_min = min(Distance), d_max = max(Distance)) %>%
  mutate(delta_d = d_max - d_min) %>%
  mutate(delta_t = difftime(t_eind, t_start))

#ship specs koppelen
S1_ship_specs_complete <- ship_specs_complete
S2_ship_specs_complete <- ship_specs_complete
names(S1_ship_specs_complete) <- lapply(names(S1_ship_specs_complete), function(x) paste0("S1_", x))
names(S2_ship_specs_complete) <- lapply(names(S2_ship_specs_complete), function(x) paste0("S2_", x))
names(S1_ship_specs_complete)[1] <- "MMSI1"
names(S2_ship_specs_complete)[1] <- "MMSI2"
min_afstand_tijd <-
  min_afstand_tijd %>%
  left_join(S1_ship_specs_complete, by=(c("MMSI1", "MMSI1"))) %>%
  left_join(S2_ship_specs_complete, by=(c("MMSI2", "MMSI2")))

#wegschrijven
if (FALSE) {
  save (min_afstand_tijd,file="Data/1_reading_cleaning/min_afstand_tijd.Rda")
}

