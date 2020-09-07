


### Setter working directory 
## oppgave 1

setwd("")

### Laster inn datasettet 
## oppgave 2
load(file = "nyedata_SEMINAR_2.Rdata")

## oppgave 3
### Viser meg variabel navn 

show(variable.names(nyedata))


### Oppgave 4

table(nyedata$kjonn) # Gir meg 1 og 2 

# For å finne ut hva 1 og 2 er bruk koden: show 

show(nyedata$kjonn) # 1 = mann, 2 = kvinne 

# Det er 777 menn i datasettet 
# Det er 629 kvinner i datasettet 

### Oppgave 5 
# Gjør om variablen alder til numerisk 

nyedata$alder <- as.numeric(nyedata$alder)
class(nyedata$alder)

# Gjør om variabelen kjønn til numerisk 
nyedata$kjonn <- as.numeric(nyedata$kjonn)

### Oppgave 6
# Gjennomsnittsalder for alle 

mean(nyedata$alder, na.rm = T)

# Jeg bruker NA fordi det er noen enheter som har missing, 
# R kan ikke regne ut gjennomsnittsalderen til enheter som 
# ikke har oppgitt sin alder

### Oppgave 7
mean(nyedata$alder[nyedata$kjonn == 1], na.rm = T) #menn 
mean(nyedata$alder[nyedata$kjonn == 2], na.rm = T) #kvinner 

### Oppgave 8

menn <- mean(nyedata$alder[nyedata$kjonn == 1], na.rm = T) #menn 
kvinner <- mean(nyedata$alder[nyedata$kjonn == 2], na.rm = T) #kvinner 

menn - kvinner # 1.34 

### Oppgave 9

library(ggthemes)
ggplot(nyedata) + 
  geom_bar(mapping = aes(valg)) + 
  xlab("Hva stemte du ved forrige valg?") + 
  scale_x_discrete(limits=c("Rødt", "SV", 
                            "Ap", "V", "Krf", "Sp", 
                            "Hoyre", "Frp", "Kystpartier", "MDG", "Andre"))+ 
  theme_fivethirtyeight()


### Oppgave 10

nyedata$blokk <- ifelse(nyedata$valg == 1, "venstresiden", # koder om rødt 
                        ifelse(nyedata$valg == 2, "venstresiden", # Koder om SV
                               ifelse(nyedata$valg == 3, "venstresiden", #Koder om Ap
                                      ifelse(nyedata$valg == 4, "hoyresiden", # Koder om V
                                             ifelse(nyedata$valg == 5, "hoyresiden", # Koder om krf
                                                    ifelse(nyedata$valg == 6, "hoyresiden", #Koder om Sp
                                                           ifelse(nyedata$valg == 7, "hoyresiden", #Koder om H
                                                                  ifelse(nyedata$valg == 8, "hoyresiden", #Koder om Frp
                                                                        ifelse(nyedata$valg == 10, "venstresiden", 
                                                                               ifelse(nyedata$valg, NA))))))))))

table(nyedata$blokk)
show(nyedata$blokk)
