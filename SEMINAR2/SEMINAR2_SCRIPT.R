######  VELKOMMEN TIL SEMINAR 2 ######


# DATAENE ER FRA
# EUROPEAN SOCIAL SURVEY (ESS)


# Den første delen er "tilrettelegging" av data
# Det er oppgaver helt på slutten
# Jeg har forklart hva jeg gjør underveis.  
# Prøv deres beste mtp på oppgavene på slutten av scriptet. 

# Legg merke til hvordan jeg organiseres arbeidet i R

# Man begynner alltid med å velge hvor man skal jobbe ut i fra

# Setwd
setwd("~/OneDrive - Universitetet i Oslo/STV1020-R/seminar_2")

# Sørg for at datasettet du skal jobbe med er i den filen 
# du velger som working directory 
load(file = "nyedata.Rdata") # Laster inn datasettet 
# Det finnes mange filtyper når det gjelder datasett 
# Vi har alledere laget våre egne 
# Vi har jobbet med excel-data
# Nå jobber vi med et datasett som jeg har laget i R
# ESS har egentlig over 500 variable
# Jeg har plukket ut de vi skal jobbe med


# Her kommer pakker - dette er den første jeg trenger 
library(tidyverse)

# Siden jeg liker å jobbe enkelt kaller jeg 
# alltid datasettet mitt data 
# her renamer jeg datasettet fra nyedata til data 
data <- rename(nyedata)
rm(nyedata)

# Så til oversikt over datasettet 
show(variable.names(data)) # Viser meg alle navnene på variablene 

# Så til oversikt over hver enkelt variabel 
show(data$rik)
# Important to be rich, have money and expensive things
# Very much like me = 1, 
# Not at all like me = 6


show(data$tillit) # Måler tillit til politikere 
# 10 = complete trust 
# 0 = no trust at all 

show(data$redusere)
# Government should reduce differences in income levels 
# Agree strongly = 1 
# Disagree strongly = 5 

# Jeg ønsker å undersøke om folk som 
# mener det er viktig å være rik og ha penger 
# og dyre ting har lavere tillit til politikere 

# Høy verdi på Y henger sammen med lavere verdi på X1 og X2

# For å gjøre det klart: 
# Det å være rik korrelerer negativt med tillit til 
# politikere og det at man er for at staten skal 
# redusere forskjeller 

# Vi må passe på at verdiene på variablene 
# har samme skalaretning (eller går den veien som gir mening 
# for undersøkelsen vår)
# Dette krever at vi gjør om på variablene våre 

# Jeg vil at høy verdi på variabelen rik (Y) skal gå sammen med 
# lav verdi på variabelene redusere (X1) og tillit (X2)

# Vi begynner med denne
show(data$rik)
# Important to be rich, have money and expensive things
# Very much like me = 1, 
# Not at all like me = 6

# Først må vi sjekke klasse 
class(data$rik)
# Denne må bli numerisk 

data$rik <- as.numeric(data$rik)
class(data$rik)
table(data$rik)

# Så vil vi at "very much like me" 
# Skal bli 6 (den høyeste verdien)
# Og 0 skal bli den minste verdien = not like med at all

# Oppretter ny variabel 
data$rik1 <- (data$rik)*-1+6 # prøv å endre 6 til 5 etterpå

table(data$rik1) # ser dere hva jeg har gjort?

### Neste variabel vi gjør det samme 
show(data$tillit) # Måler tillit til politikere 
# 10 = complete trust 
# 0 = no trust at all

class(data$tillit)
data$tillit <- as.numeric(data$tillit)
class(data$tillit)

#Denne variabelen har den retningen vi ønsker 

### Neste variabel 
# Government should reduce differences in income levels 
# Agree strongly = 1 
# Disagree strongly = 5 

class(data$redusere)
data$redusere <- as.numeric(data$redusere)
class(data$redusere)
table(data$redusere)

data$redusere1 <- (data$redusere)*-1+5
table(data$redusere1)
table(data$redusere)

### NÅ KJØRER VI REGRESJON: 
# rik1 som Y og redusere1 som X 

# presenterer tabellen vha pakken Stargazer 

# noen som vil tolke resultatene? 

# Vi legger til en variabel: 
# rik1 som Y og redusere1 og tillit som X 
# presenterer resultatene i stargazer 

# Ikke bare tall, vi må visualisere våre funn
ggplot(data, aes(x = redusere1, y = rik1)) + 
  geom_smooth(method = "lm")

ggplot(data = data) + 
  geom_smooth(method = "lm", mapping = aes(x = redusere1, y = rik1))+ 
  geom_smooth(method = "lm", mapping = aes(x = tillit, y = rik1))





# OPPGAVER 

### OPPGAVE 1: Hva er gjennomsnittet på hver av variabelene? 

# OPPGAVE 2 
# Lag et nytt datasett med alle observasjoner 
# som har alder mindre en gjennomsnittet 

# OPPGAVE 3 
# Se hvor mange kvinner og menn det er i dette nye datasettet
# med observasjoner under gjennomsnittsalderen 

# OPPGAVE 3 
# La en bivariat regresjonsanalyse hvor tillit er Y 
# og X er kjønn, lag en å se om det er noe forskjell i tillit 
# når man har alle observasjoner (det store datasettet) 
# og lag en regresjonsanalyse i det lille datasettet med observasjoner
# som har alder under gjennomsnittet 
# Tolk resultatene

# OPPGAVE 4 
# Lag nok en regresjonsanalyse og se på tillit, kjønn og alder
# Gjør det samme i begge datasettet 
# Print ut resultatene med stargazer 
# Tolk resultatene 


# OPPGAVE 5 
# Plot tillit og alder. Bruk geom_point. 
# Del opp observasjonene i kvinner og menn 

# OPPGAVE 6 
# Kjør en regresjonsanalyse med tillit og redusere1
# Er det slik at de som har høyere tillit til politikere 
# er mer positive til at myndighetene skal redusere forskjeller
# i inntekt? Print en stargazer tabell og forklar tallene. 

# OPPGAVE 7 
# Lag distribusjonsplott: geom_density
# a) av variablene tillit 
# b) av variabelen redusere1 
# c) av alderen 
# Vil du si at variablene er normalfordelte? 

# OPPGAVE 8 
# Se på variablene redusere1 og kjønn 
# Er det slik at det er kjønnsforskjeller når det gjelder 
# støtte til å statlig omfordeling? 
# Kjør en bivariat regresjonsanalyse med redusere1 som Y 
# og kjønn X.
# Print resultatene med stargazer. 
# Stemer antagelsen som at flere kvinner er mer for omfordeling?
# Se på skalaen for redusere1 for å kunne gi en god tolkning. 

# OPPGAVE 9
# Lag en krysstabell med redusere1 og kjønn 
# Gir dette inntrykk av at påstanden ovenfor stemmer? 