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


navn <- c("Anna", "Linn", "Jonas", "Martin", "Eli", "Mathias",  
          "Matias", "Anna", "Mona", "Erik", "Live", "Jon", "Balz", "Louisa" )

kjonn <- c("J", "J", "G", "G", "J", "G", "G", "J", "J", "G", "J", "G", "G", "J")

studiested <- c("Edinburgh", "Bodo", "UiO", "UiO", "Durham", "UiB", "UiO", "Manchester", 
                "Agder", "Halden", "UiB", "UiO", "UiO", "UiO")

alder <- c("24", "25", "23", "28", "22", "26", "24", "24", "25", "21", "24", "22", "41", "25")

data <- data.frame(navn, alder, kjonn, studiested,
                   stringsAsFactors = F)


View(data)
head(data)
tail(data)

show(variable.names(data))

######## HVILKEN KLASSE HAR DE ULIKE OBJEKTENE? 
# NAVN
# ALDER 
# KJØNN 

class(data$navn)
class(data$alder)

data$alder <- as.numeric(data$alder)
class(data$alder)

mean(data$alder)
sd(data$alder)

######## GJENNOMSNITTSALDEREN FOR KVINNER I SEMINARET - INDEKSERING
mean(data$alder[data$kjonn =="J"])

######## GJENNOMSNITTSALDEREN FOR MENN I SEMINARET - INDEKSERING
mean(data$alder[data$kjonn =="G"])

######## TABULLERING 

table(data$studiested)
table(data$alder)
table(data$kjonn, data$alder)
######## PLOTTING


######## ORGANISERING AV ARBEID I R ########
######## Først må vi fjerne de som er i "environmentet" 

rm(list = ls())

# For enkeltobjekter kan du bruke 

#rm()

######## SET.WORKING DIRECTORY - MANUEL VISNING 
# Koden du får her kan du huske til neste gang sånn at du slipper 
# å skrive koden hver gang 
setwd("~/Desktop/R-SEMINAR1")
######## PAKKER + info


install.packages("readxl")
library(readxl)

install.packages("tidyverse")
library(tidyverse)

library(ggplot2)
######## LASTE INN DATASETT
data <- read_excel("DATA_SEMINAR1.xlsx")

######## NÅ HAR VI DATASETTET HVA GJØR VI SÅ? HVORDAN SER DET UT? 

view(data)
head(data)
show(variable.names(data))

table(data$gender)

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
# Først variabelnavn 

show(variable.names(data))

mean(data$income) #M for alle 
mean(data$income[data$gender == "Male"]) #M for menn 

table(data$gender) # Viser meg hva kategoriene heter

#Hva er gjennomsnittsinntekten til kvinner?
mean(data$income[data$gender == "Female"]) #M for kvinner 
########################################################

######## OMKODING AV VARIABLEN GENDER 

table(data$gender)

data$omkodet_gender <- ifelse(data$gender == "Female", 1, 0)


data$kjonn <- ifelse(data$gender == "Female", "Kvinne", 
                     ifelse(data$gender == "Male", "Mann", 
                            ifelse(data$gender))) 
View(data)

View(data)
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
data %>% 
  filter(age > 60)

# Kvinner med alder over 60 - hvor mange er det?

data %>% 
  filter(gender == "Female", age > 60)
  

# Menn med alder over 60 - hvor mange er det?

data %>% 
  filter()

######## GRAFIKK 

ggplot(data = data, mapping = aes(age, income)) + 
  geom_smooth(method = "lm") 

ggplot(data = data) + 
  geom_smooth(method = "lm", mapping = aes(x = age, y = income, color = gender))


#Slik lager jeg en pdf fil av dette plottet
pdf("plot1.pdf")
