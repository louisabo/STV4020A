########## VELKOMMEN TIL SEMINAR 3 ##########
# PLANEN I DAG ER Å JOBBE VIDERE MED DET VI HAR GJORT SÅ LANGT
# NYTT FOR I DAG ER OLS-DIAGNOSTIKK 
# DET ER OGSÅ NOEN NYE KODER VI SKAL LÆRE
# VI SKAL JOBBE MER MED PLOTTING OG OLS 

# Vi begynner med å fjerne det som er i "environmentet"
rm(list = ls())

# Laster inn data 
# Linken finner dere på github
# Disse dataene er også fra European Social Survey
data <- read.csv("https://raw.githubusercontent.com/louisabo/STV4020A/master/SEMINAR3/internett.csv")

# Vi kan se på data på følgende måter: 

# Deretter kan vi bruke følgende kode
str(data) # Denne koden viser hvilken klasse variabelene har!

# Summary viser beskrivende statistikk for hver av variabelene
# Den viser derimot ikke SD
summary(data$internettbruk)
sd(data$internettbruk, na.rm = T)

######### Over til NA
# Ofte vil man få oversikt over NA
# Dette for å være ryddig i analysen

table(complete.cases(data)) 

sum(is.na(data$internettbruk))



###################### OPPGAVER 
# 1) Presenter et histogram for variabelen internettbruk?

# 2) Hvor mange observasjoner har missing på variabelen kjønn?

# 3) Hvor mange observasjoner er det i datasettet? 

# 4) Opprett en ny dummyvariabel av kjønn: 
# Menn skal ha verdien 1 og kvinner 0

# 5) Opprett en ny variabel der alle som har utdanning over 
# 13 år får verdien 1, men alle andre får verdien 0.

# 6) Estimer en bivariat regresjonsmodell med internettbruk Y 
# og alder som uavhengig variabel. 

# Presenter resultatene i vha stargazer. 
# Tolk regresjonskoeffisienten. 

# 7) Legg til tillit som kontrollvariabelen i modell. 
# Presenter resultatene i en tabell og tolk konstantleddet og 
# R^2 og regresjonskoeffienten. 

# 8) Lag et plot som viser sammenhengen mellom 
# internettbruk og alder

############################################################
# Før neste steg skal jeg snakke litt mer om klasser og målenivå
# Vi må snakke igjen om koden as.factor og log 


# Vi skal bruke siste modell til å kjøre regresjonsdiagnostikk

#################################################
#################################################

####### MODELLVURDERING 
####### REGRESJONSDIAGNOSTIKK 

####### CHRISTOPHERSEN KAP. 7

# Forutsetninger for OLS / kritiske aspekter
# 1) Ingen OVB - teoretisk 
# 2) Lineær sammenheng mellom variablene - grafisk 
# 3) Ingen autokorrelasjon -  uavhengige observasjoner
# 4) Normalfordelte residualer 
# 5) Homoskedastiske residualer 
# 6) Ingen perfekt multikolinearitet X1 og X2 som 
# 7) Uteliggere / innflytelsesrike observasjoner 
# 8) Missing verdier (hvordan behandles disse) - teoretisk 

# Setter working directory 
# Vi laster inn datasett
# Vi lager en dikotom variabel 
# Vi lager en modell
# Den tolker vi -- så kjører vi regresjonsdiagnostikk!


### Linearitet 
install.packages("car")
library(car)

ceresPlots(modell)


#### Uavhengighet - ingen autokorr

durbinWatsonTest(modell)

#### Normalfordelte restledd (residualer) 

qqPlot(modell)

#### Homoskedastisitet 

spreadLevelPlot(modell)
?spread.level.plot

#### Multikolinearitet 

vif(modell)

#### Outliers 

influenceIndexPlot(modell, id.n = 5)
