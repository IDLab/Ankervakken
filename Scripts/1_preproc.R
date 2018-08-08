
# Packages
library(dplyr)
library(ggplot2)
library(reshape2)
library(parallel)

set.seed(1)

# Data ophalen
load('Data/1_reading_cleaning/afstanden_9d_combis.Rda')

# Dataset transposen; combi's onder elkaar zetten
afstanden_melt <- melt(afstanden_9d_combis, id.vars = c("X","A"))
afstanden_melt <-
  afstanden_melt %>%
  arrange(variable, A) %>%
  mutate(value = as.numeric(value)) %>%
  mutate_if(is.factor,as.character)

# Sample van een x aantal combis. Overschrijft de volledige set. Geen sample? Dan deze chunk niet runnen.
if (TRUE) {
  n_combi <- 50
  row_nrs <- round(runif(n_combi, 1, length(unique(afstanden_melt$variable))), 0)
  afstanden_melt <-
    afstanden_melt %>%
    filter(variable %in% unique(afstanden_melt$variable)[row_nrs])
}

### Tijd x afstand
#Geeft onderlinge afstand combinaties in de tijd weer
if (FALSE) {
  ggplot(afstanden_melt, aes(x = A, y = value, color = variable)) +
    geom_line(size = 1, show.legend = FALSE) +
    xlab("Datum + tijd") +
    ylab("Afstand")
}

# Minimale afstand met bandbreedte (percentage) in combinatie met tijdsduur wegschrijven in data frame
bandbreedte <- .1

#functie definieren die per schipcombi de juiste data over onderlinge afstand verzameld
fMinAfstand <- function (i) {
  #selecteer alleen de waardes voor een specifiek paar
  selectie_i <- afstanden_melt %>% filter(variable == unique(afstanden_melt$variable)[i])
  #filter gevonden waardes op minimum plus de waardes die binnen de opgegeven bandbreedte liggen
  min_afstand_tijd_i <-
    selectie_i %>%
    filter(value <= (1 + bandbreedte) * selectie_i[which.min(selectie_i$value),4])
  return(min_afstand_tijd_i)
}

#standaard lapply call
if (TRUE) {
  list_min_afstand_tijd<-lapply (1:length(unique(afstanden_melt$variable)), fMinAfstand)
}

#Deze code is om op windows parallel te draaien
if (FALSE) {
  no_cores <- detectCores()-1
  cl <- makeCluster(no_cores)
  clusterExport (cl, "afstanden_melt")
  clusterExport (cl, "bandbreedte")
  clusterEvalQ(cl, library(dplyr))
  list_min_afstand_tijd<-parLapply (cl, 1:length(unique(afstanden_melt$variable)), fMinAfstand)
  stopCluster(cl)
}

#Deze code werkt voor parallel rekenen op Mac, niet voor Windowns...
if (FALSE) {
  list_min_afstand_tijd<- mclapply(1:length(unique(afstanden_melt$variable)), testf, mc.cores = getOption("mc.cores", 4L))
}

#lijst converteren naar dataframe
min_afstand_tijd<-do.call(rbind.data.frame, list_min_afstand_tijd)
#even schoonmaken van grotere datasets
rm(list_min_afstand_tijd)

#kolommen delta afstand en tijd vullen
min_afstand_tijd <-
  min_afstand_tijd %>%
  group_by(combi = variable) %>%
  summarise(t_start = min(A), t_eind = max(A), d_min = min(value), d_max = max(value)) %>%
  mutate(delta_d = d_max - d_min) %>%
  mutate(delta_t = difftime(t_eind, t_start))

# Combi van scheepsnamen splitsen in twee kolommen
min_afstand_tijd$schip_1 <-
  sapply(min_afstand_tijd$combi,
         function(y) substr(y, 1, regexpr("...", y, fixed = TRUE)[1] - 1))
min_afstand_tijd$schip_2 <-
  sapply(min_afstand_tijd$combi,
         function(y) substr(y, regexpr("...", y, fixed = TRUE)[1] + 3, nchar(as.character(y))))

# NAs niet meenemen.
min_afstand_tijd <-
  min_afstand_tijd %>%
  filter(schip_1 != "nan" & schip_2 != "nan")

if (FALSE) {
  save (min_afstand_tijd,file="Data/1_reading_cleaning/min_afstand_tijd.Rda")
}

