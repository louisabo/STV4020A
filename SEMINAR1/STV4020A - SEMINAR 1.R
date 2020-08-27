########################################################
########              VELKOMMEN TIL             ########
########              SEMINAR 1                 ########
########              STV4020 A - GRUPPE 1      ########
########################################################

######### PLANEN FOR I DAG DEL 1
######### JOBBE MED DATA OM OSS SELV 
######### OPPRETING AV DATASETT + OBJEKTER 
######### TABULLERING, INDEKSERING OG PLOTTING 

######## PLANEN FOR I DAG DEL 2 
######## ORGANISERING AV ARBEID I R 
######## OPPLASTING AV FORELIGGENDE DATASETT 
######## OVERSIKT AV DATASETT, TABULERING, INDEKSERING 
######## MER OM STATISTISKE MÅL 
######## BIVARIATE SAMMENHENGER 
######## KANSKJE OMKODING AV VARIABLER 


######## DATA OM OSS SELV ########
######## OPPRETTER OBJEKTER 

######## HVILKEN KLASSE HAR DE ULIKE OBJEKTENE? 
# NAVN
# ALDER 
# KJØNN 

######## HER LAGER JEG DATASETTET 
######## VÆR OPPMERKSOM PÅ HVA SOM SKJER I "ENVIRONMENTET"



######## OVERSIKT OVER DATASETTET + TABULLERING 

######## GJENNOMSNITTSALDEREN FOR KVINNER I SEMINARET - INDEKSERING
######## GJENNOMSNITTSALDEREN FOR MENN I SEMINARET - INDEKSERING

######## PLOTTING


######## ORGANISERING AV ARBEID I R ########
######## Først må vi fjerne de som er i "environmentet" 

#rm(list = ls())

# For enkeltobjekter kan du bruke 

rm()

######## SET.WORKING DIRECTORY - MANUEL VISNING 
# Koden du får her kan du huske til neste gang sånn at du slipper 
# å skrive koden hver gang 

######## PAKKER + info


install.packages("readxl")
library(readxl)

install.packages("tidyverse")
library(tidyverse)

# ggplot 
# PDF-saver?

######## LASTE INN DATASETT


######## NÅ HAR VI DATASETTET HVA GJØR VI SÅ? HVORDAN SER DET UT? 

view(data)
head(data)
view(variable.names(data))
summary(data)
str(personst)   
tail(personst)  

######## MER TABULLERING
# For bedre oversikt over statistikken: beskrivende statistikk 

######## OMKODING AV VARIABLEN GENDER 

######## STATISTISKE MÅL
#Hva er standardavviket til alderen? 
#Hvordan henter jeg mer info om alderen? 

### === OPPGAVER === ###
#Hva er gjennomsnittsalderen til menn? 
#Hva er gjennomsnittsalderen til kvinner? 

#Hva er gjennomsnittsinntekten til menn?
#Hva er gjennomsnittsinntekten til kvinner?

########################################################

######## OMKODING AV VARIABLEN GENDER 

######## VI SKAL INDEKSERE MER, MEN PÅ EN ANNEN MÅTE
# Vi må si til R hvilken informasjon vi vil hente ut
# Dette gjør vi vha symboler

#Symbol | Betydning     
# --    |---
# `<`   | mindre enn     
#`<=`   | mindre eller lik     
#`>`    | større enn    
#`>=`   | større eller lik    
# `==`  | helt lik    
#`!=`   | ulik    
#`!x`   | forskjellig fra x  
#`is.na`| logisk test for missing 
# `x\|y`| x eller y (`\|` betyr eller)    
#`x & y`| x og y (`&` betyr og)    

# De to siste symbolene, `|` og `&`, brukest til å lage 
# logiske tester med flere betingelser. 
# De resterende symbolene er logiske tester, som forteller deg om noe er `TRUE` eller `FALSE`.


# Personer med alder over 60 - hvor mange er det? 


# Kvinner med alder over 60 - hvor mange er det?


# Menn med alder over 60 - hvor mange er det?


######## GRAFIKK 

ggplot(data = data, mapping = aes(age, income)) + 
  geom_smooth(method = "lm") 

ggplot2::ggplot(data = data) + 
  geom_smooth(method = "lm", mapping = aes(x = age, y = income, color = gender))


#Slik lager jeg en pdf fil av dette plottet
pdf("plot1.pdf")
