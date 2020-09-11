#Løsningsforslag COVID19 OPPGAVER 

covidata <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-16-2020.csv?fbclid=IwAR1Snt_p5Ir0pmWKIHAO2grXrGI4uk0s7_oUetEi8zOaWoT7g134HiG0zeY",
                     stringsAsFactors = FALSE) 

# Dere kan bruke begge datasett og sammenligne dem. 
# Det som er spennende er at det som over denne linjen er fra 16. mars 2020 og det som er under er fra 1. september 2020. 
# Så hvis du løser oppgavene først med det første og så med det andre så får du et inntrykk av utviklingen over tid. 

data <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/09-01-2020.csv", 
                 stringsAsFactors = F)

library(tidyverse)
View(data)

## Oppgave 1:

ddata <- subset(data, Deaths > 0)

dddata <- data %>% 
filter (Deaths >0)

#92 observasjoner. Men mange av dem er forskjellige stater i 
#samme land. Vi kan bruke table() for ? se hvilke unike land:
table(ddata$Country.Region)

#hvor mange er det?
length(table(ddata$Country_Region))
#175

#eller bruk funskjonen unique():
unique(ddata$Country_Region)

## Oppgave 2:

kdata <- subset(data, Country_Region == "China")
#her er hver rad en provins
nrow(kdata) # = 33

## Oppgave 3:

ldata <- subset(data, Deaths == 0)
mean(ldata$Long_, na.rm = T) # = -86

## Oppgave 4:

data <- data %>%
  mutate(rec_frac = Recovered / Confirmed,
         death_frac = Deaths / Confirmed,
         sick_frac = (Confirmed - (Recovered + Deaths)) / Confirmed)

#evt:    sick_frac = 1 - (rec_frac + death_frac)

## Oppgave 5:

ggplot(data, aes(x = rec_frac)) + 
  geom_density()

ggplot(data, aes(x = death_frac)) + 
  geom_density()

ggplot(data, aes(x = sick_frac)) + 
  geom_density()

## Oppgave 6:
data$dist_eq <- abs(data$Lat)
mean(data$dist_eq, na.rm = T) # = 37

## Oppgave 7:

ggplot(data, aes(x = dist_eq, y = death_frac)) + 
  geom_point() +
  geom_smooth(method = "lm")

## Oppgave 8:

data$Province_State <- ifelse(data$Province_State == "",
                                  data$Country_Region,
                                  data$Province_State)
View(data)



