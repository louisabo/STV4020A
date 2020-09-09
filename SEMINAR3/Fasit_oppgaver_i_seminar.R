### Fasit oppgaver fra seminar 3 - fra timen
# Laster inn datasett
data <- read.csv("https://raw.githubusercontent.com/louisabo/STV4020A/master/SEMINAR3/internett.csv")

###################### OPPGAVER 
# 1) Presenter et histogram for variabelen internettbruk?

library(ggplot2)

ggplot(data, aes(internettbruk)) + 
  geom_bar() 

# 2) Hvor mange observasjoner har missing på variabelen kjønn?

sum(is.na(data$kjonn))

# 3) Hvor mange observasjoner er det i datasettet? 

# Her kan du bare se i environmentet 
# Eventuelt 

nrow(data)

# For å kun telle enkelte variabeler 
length(data$internettbruk)
length(data$utdanning)

# 4) Opprett en ny dummyvariabel av kjønn: 
# Menn skal ha verdien 1 og kvinner 1

data$mann <- ifelse(data$kjonn == 1, 1, 0)
table(data$kjonn, data$mann) # For å se om du har kodet riktig

# 5) Opprett en ny variabel der alle som har utdanning over 
# 13 år får verdien 1, men alle andre får verdien 0.

table(data$utdanning) # Viser meg fordeling av utdanning

data$utdanning2 <- ifelse(data$utdanning <13, 1,0)

table(data$utdanning2)

# 6) Estimer en bivariat regresjonsmodell med internettbruk Y 
# og alder som uavhengig variabel. 

mod1 <- lm(internettbruk~alder, data = data)

# Presenter resultatene i vha stargazer. 
# Tolk regresjonskoeffisienten. 

stargazer(mod1, type = "text", align = T)

# 7) Legg til tillit som kontrollvariabelen i modell. 
# Presenter resultatene i en tabell og tolk konstantleddet og 
# R^2 og regresjonskoeffienten. 

mod2 <- lm(internettbruk~alder + tillit, data = data)

stargazer(mod1, mod2, 
          type = "text")


# 8) Lag et plot som viser sammenhengen mellom 
# internettbruk og alder

ggplot(data, aes(x = alder, y = internettbruk)) +
  geom_point() +
  geom_smooth(method = "lm") 

############################################################









