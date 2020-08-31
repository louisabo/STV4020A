

### FASIT 

# OPPGAVE 1
# Hva er gjennomsnittet til alder-variablen? 

mean(data$alder, na.rm = T ) # 47 

# OPPGAVE 2 
# Lag et nytt datasett med alle observasjoner 
# som har alder mindre en gjennomsnittet 
# Trenger pakken tidyverse 

library(tidyverse)

data1 <- data %>% 
  filter(alder < 47.10116)

View(data1)  # Alle er under M
# Kan kjøre en ytterligere test 

mean(data1$alder, na.rm = T)

# Kan kjøre også denne koden som sjekk 

data1 %>% 
  filter(alder > 47) %>% 
  count(sort = T )

# OPPGAVE 3 
# Se hvor mange kvinner og menn det er i dette nye datasettet
# med observasjoner under gjennomsnittsalderen 

table(data1$kjonn1) # 0 = kvinner (321), 1 = menn (366)

# OPPGAVE 3 
# La en bivariat regresjonsanalyse hvor tillit er Y 
# og X er kjønn, lag en å se om det er noe forskjell i tillit 
# når man har alle observasjoner (det store datasettet) 
# og lag en regresjonsanalyse i det lille datasettet med observasjoner
# som har alder under gjennomsnittet 
# Tolk resultatene

# Reganalyse basert på data fra data 
modell1 <- lm(tillit ~ kjonn1, data = data )
summary(modell1)

# Reganalyse basert på data fra data1

modell2 <- lm(tillit ~ kjonn1, data = data1 )
summary(modell2)

# Se på forskjellen på estimatene: konstantledd og kjonn1 


# OPPGAVE 4 
# Lag nok en regresjonsanalyse og se på tillit, kjønn og alder
# Gjør det samme i begge datasettet 
# Print ut resultatene med stargazer 
# Tolk resultatene 

modell3 <- lm(tillit ~ kjonn1 + alder, data = data )
summary(modell3)

modell4 <- lm(tillit ~ kjonn1 + alder, data = data1 )
summary(modell4)

# Henter pakken stargazer 
library(stargazer)

stargazer(modell3, modell4, type = "text", alignt = T)



# OPPGAVE 5 
# Plot tillit og alder. Bruk geom_point. 
# Del opp observasjonene i kvinner og menn 

# Henter pakken ggplot2 

library(ggplot2)

ggplot(data, aes(x = alder, y = tillit, col = as.factor(kjonn1))) + 
  geom_point()

# OPPGAVE 6 
# Kjør en regresjonsanalyse med tillit og redusere1
# Er det slik at de som har høyere tillit til politikere 
# er mer positive til at myndighetene skal redusere forskjeller
# i inntekt? Print en stargazer tabell og forklar tallene. 

modell5 <- lm(tillit ~ redusere1, data = data)

stargazer(modell5, type = "text", aling = T)

# OPPGAVE 7 
# Lag distribusjonsplott: geom_density
# a) av variablene tillit 
# b) av variabelen redusere1 
# c) av alderen 
# Vil du si at variablene er normalfordelte? 

ggplot(data = data, aes(tillit)) + 
  geom_density()

ggplot(data = data, aes(redusere1)) + 
  geom_density()

ggplot(data = data, aes(alder)) + 
  geom_density()

# OPPGAVE 8
# Se på variablene redusere1 og kjønn 
# Er det slik at det er kjønnsforskjeller når det gjelder 
# støtte til å statlig omfordeling? 
# Kjør en bivariat regresjonsanalyse med redusere1 som Y 
# og kjønn X.
# Print resultatene med stargazer. 
# Stemer antagelsen som at flere kvinner er mer for omfordeling?
# Se på skalaen for redusere1 for å kunne gi en god tolkning. 

 

modell7 <- lm(redusere1 ~ kjonn1, data = data)
summary(modell7)

stargazer(modell7, type = "text", align = T)

# OPPGAVE 9
# Lag en krysstabell med redusere1 og kjønn 
# Gir dette inntrykk av at påstanden ovenfor stemmer? 

table(data$kjonn1, data$redusere1) # printer absolutt fordeling



