# Packages
library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(lubridate)
library(BiocGenerics)
library(network)
library(gmodels)
library(graph)
library(GGally)
library(sna)

# Ophalen data en wegschrijven als R-bestanden
#ankervakken_2017Q1 <- read_xlsx("schepenankervakkenq1.xlsx", sheet="All")
#New Jvv 20180718
#ankervakken_9d <- read.csv2("20170401-0409.csv", sep = ",",stringsAsFactors=FALSE)
#afstanden_9d_combis <- read.csv2("distances201704010409.csv", sep = ",", stringsAsFactors=FALSE)

#NEW jvv 20180718
#corrigeren voor inlezen als Date Time
#afstanden_9d_combis$A <- as.POSIXct(afstanden_9d_combis$A)
#ankervakken_9d$Starttime <- as.POSIXct(ankervakken_9d$Starttime)
#ankervakken_9d$Updatetime <- as.POSIXct(ankervakken_9d$Updatetime)
#corrigeren voor MMSI correct inlezen
#ankervakken_9d$MMSI<- substr(ankervakken_9d$MMSI,1,9)

#save(ankervakken_2017Q1, file="ankervakken_2017Q1.Rda")
#save(ankervakken_9d, file="ankervakken_9d.Rda")
#save(afstanden_9d_combis, file="afstanden_9d_combis.Rda")

# Ophalen R-databestanden
load("ankervakken_2017Q1.Rda")
load("ankervakken_9d.Rda")
load("afstanden_9d_combis.Rda")

# Tijdvariabele omzetten zodat R dit daadwerkelijk ziet als tijd
afstanden_9d_combis <-
  afstanden_9d_combis %>%
  mutate(A = as.POSIXct(A))

# Sample nemen van combinaties
afstanden_9d_combis_smp <- sample_n(afstanden_9d_combis, 10)

# Ankervakken 9 dagen uniek maken op combinatie schip X dag (voor visualisatie in de tijd)
ankervakken_9d_uniek <-
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

# Wegschrijven data 9 dagen per schip
save(ankervakken_9d_uniek, file="ankervakken_9d_uniek.Rda")

# Wegschrijven data 9 dagen per schip t.b.v. Power BI
write.csv2(ankervakken_9d_uniek, file="ankervakken_9d_uniek.csv", row.names=TRUE)

#Lijst maken en wegschrijven van unieke MMSI nummers in dataset, om te gebruiken voor bouwen scheepsDB
MMSIUniek_9d <- (unique(ankervakken_9d$MMSI))
write.csv2(MMSIUniek_9d, file="MMSIUniek_9d.csv", sep =",", row.names=FALSE)

#NEW pm 20180725
# Sample n_col van het aantal combis
n_col <- 100
col_nrs <- round(runif(n_col, 3, length(afstanden_9d_combis)),0)
afstanden_9d_combis_sel <- afstanden_9d_combis[,c(1:2,col_nrs)]

# Combi's onder elkaar zetten
afstanden_melt <- melt(afstanden_9d_combis_sel, id.vars = c("X","A"))
afstanden_melt <-
  afstanden_melt %>%
  arrange(variable, A) %>%
  mutate(value = as.numeric(value))

#NEW pm 20180725
# Plotje tijd x afstand, op basis van een aantal combis (n_combi)
n_combi <- 10
row_nrs <- round(runif(n_combi, 1, length(unique(afstanden_melt$variable))), 0)
afstanden_melt_sample <-
  afstanden_melt %>%
  filter(variable %in% unique(afstanden_melt$variable)[row_nrs])
X11(30,20)
ggplot(afstanden_melt_sample, aes(x = A, y = value, color = variable)) +
  geom_line(size = 1, show.legend = FALSE) +
  xlab("Datum + tijd") +
  ylab("Afstand")

# Minimale afstand per combinatie wegschrijven in een data frame
min_afstand <- data.frame()
for (i in 1:length(unique(afstanden_melt$variable))) {
  
  selectie_i <- afstanden_melt %>% filter(variable == unique(afstanden_melt$variable)[i])
  
  min_afstand_i <- selectie_i[which.min(selectie_i$value),]

  min_afstand <- bind_rows(min_afstand, min_afstand_i)
  
}

# Minimale afstand met bandbreedte (percentage) in combinatie met tijdsduur wegschrijven in data frame
min_afstand_tijd <- data.frame()
bandbreedte <- .1

for (i in 1:length(unique(afstanden_melt$variable))) {
  
  selectie_i <- afstanden_melt %>% filter(variable == unique(afstanden_melt$variable)[i])
  
  min_afstand_tijd_i <-
    selectie_i %>%
    filter(value <= (1 + bandbreedte) * selectie_i[which.min(selectie_i$value),4])

  min_afstand_tijd <- bind_rows(min_afstand_tijd, min_afstand_tijd_i)
  
}

min_afstand_tijd <-
  min_afstand_tijd %>%
  group_by(combi = variable) %>%
  summarise(t_start = min(A), t_eind = max(A), d_min = min(value), d_max = max(value)) %>%
  mutate(delta_d = d_max - d_min) %>%
  mutate(delta_t = difftime(t_eind, t_start))

# Combi splitsen in twee kolommen
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

# Edge list
data_netwerk <-
  min_afstand_tijd %>%
  select(schip_1, schip_2, d_min) %>%
  arrange(desc(d_min))

# Combinaties komen allemaal twee keer voor, waarbij de volgorde van schepen is omgedraaid. Hierop ontdubbelen,
data_netwerk <-
  data_netwerk %>%
  filter((schip_1 != lead(schip_2)) | (schip_2 != lead(schip_1)))

# Van edge list naar adjacency matrix
adj_matrix <- ftM2adjM(as.matrix(data_netwerk[,1:2]), W=data_netwerk$d_min)

# Netwerkje maken
netwerk_input <- network(adj_matrix, directed=FALSE)

# Edge size bepalen
edgesize <- data_netwerk$d_min / mean(data_netwerk$d_min)

# Visualiseren
graphics.off()
x11(30,20)
ggnet2(
  netwerk_input,
  node.color = "green",
  edge.size = edgesize,
  label = TRUE,
  label.size = 3
)


