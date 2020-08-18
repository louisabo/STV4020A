#Løsningsforslag COVID19 OPPGAVER 

covidata <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-16-2020.csv?fbclid=IwAR1Snt_p5Ir0pmWKIHAO2grXrGI4uk0s7_oUetEi8zOaWoT7g134HiG0zeY",
                     stringsAsFactors = FALSE)
library(tidyverse)

## Oppgave 1:

ddata <- subset(covidata, Deaths > 0)

#92 observasjoner. Men mange av dem er forskjellige stater i 
#samme land. Vi kan bruke table() for ? se hvilke unike land:
table(ddata$Country.Region)

#hvor mange er det?
length(table(ddata$Country.Region))
#49

#eller bruk funskjonen unique():
unique(ddata$Country.Region)

## Oppgave 2:

kdata <- subset(covidata, Country.Region == "China")
#her er hver rad en provins
nrow(kdata) # = 33

## Oppgave 3:

ldata <- subset(covidata, Deaths == 0)
mean(ldata$Longitude) # = -8.087032

## Oppgave 4:

covidata <- covidata %>%
  mutate(rec_frac = Recovered / Confirmed,
         death_frac = Deaths / Confirmed,
         sick_frac = (Confirmed - (Recovered + Deaths)) / Confirmed)

#evt:    sick_frac = 1 - (rec_frac + death_frac)

## Oppgave 5:

ggplot(covidata, aes(x = rec_frac)) + 
  geom_density()

ggplot(covidata, aes(x = death_frac)) + 
  geom_density()

ggplot(covidata, aes(x = sick_frac)) + 
  geom_density()

## Oppgave 6:
covidata$dist_eq <- abs(covidata$Latitude)
mean(covidata$dist_eq) # = 31.63458

## Oppgave 7:

ggplot(covidata, aes(x = dist_eq, y = death_frac)) + 
  geom_point() +
  geom_smooth(method = "lm")

## Oppgave 8:

covidata$Province.State <- ifelse(covidata$Province.State == "",
                                  covidata$Country.Region,
                                  covidata$Province.State)
View(covidata)

## Utfordring:
covidata$day <- substr(covidata$Last.Update, 1, 10)
