



#### LØSNINGSFORSLAG - OPPGAVER TIL 3 SEMINAR ####



# Set working directory gjøres med koden 

#setwd()
setwd("~/OneDrive - Universitetet i Oslo/STV1020-R/seminar_3")

# Dette kan du også gjøre manuelt ved å gå på til menylinjen øverst
# trykke på "session"
# trykke på "set working directory" 
# og deretter velge selv fil som du skal jobbe ut i fra 




# Lager en variabel som er land 
land <- c("Kambodsja", "Kina", "Indonesia", "Japan", 
          "Pakistan", "Thailand", "Kasakhstan", "Kyrgystan", 
          "Vietnam")

# Lager en variabel som er Poverty Headcount Ratio
fattigdomsrate <- c(51.05, 28.33, 54.71, 0.35, 55.83, 4.56, 
         1.15, 18.96, 43.32)

# Lager datasettet 
data <- data.frame(land, fattigdomsrate, 
                   stringsAsFactors = FALSE)



# OPPGAVE 1
# Koder om land og lager ny dikotom variabel 

data$post_sov <- ifelse(data$land == "Kambodsja", 1, 
                        ifelse(data$land == "Kina", 1, 
                               ifelse(data$land == "Indonesia", 1, 
                                      ifelse(data$land == "Japan", 1, 
                                             ifelse(data$land == "Pakistan", 1, 
                                                    ifelse(data$land == "Thailand", 1, 
                                                           ifelse(data$land == "Kasakhstan", 0, 
                                                                  ifelse(data$land == "Kyrgystan", 0, 
                                                                         ifelse(data$land == "Vietnam", 1, 
                                                                                ifelse(data$land))))))))))

# Ser om jeg har gjort riktig 

table(data$post_sov)


# OPPGAVE 2

# Finner gjennomsnittet for land som ikke har post-sovjet historie 
mean(data$fattigdomsrate[data$post_sov == 1])

mean(data$fattigdomsrate[data$post_sov == 0])

# OPPGAVE 3

# Lager plot (jeg bruker vanlig barplot, men jeg 
# oppfordrer dere til å bruke ggplot og leke der frem med ulike 
# måter å visualisere data på)

barplot(fattigdomsrate, names.arg = land, 
        col = "pink", xlab = "Land", ylab = "Poverty Headcount Ratio")


# OPPGAVE 4


# Her må du laste ned pakken ggplot hvis du ikke 
# har gjort det på din egen PC. 
# Dette gjør du med koden install.packages("ggplot2") 
# Deretter kjører dere library som jeg har gjort... 

library(ggplot2)

# Forslag til hvordan løse oppgaven
ggplot(data = data, aes(x = factor(post_sov), y = fattigdomsrate)) + 
  geom_boxplot()

# Jeg vil gjerne at dere eksprimenterer litt med ggplot
# Gjerne del andre forslag i opprettet diskusjonstråd på CANVAS med oss andre!

# OPPGAVE 5
# Håper alle har sett litt på tidytuesday-videom som jeg la ut
# Vi bruker pakken tidyverse for å hente ut dataene - slik som vi gjorde i seminar 
# Først må dere KANSKJE installere pakken eller bare bruke library for å hente den 

library(tidyverse)

# Land med fattigdomsrate under 30 
data %>% 
  filter(fattigdomsrate < 30) %>% 

# Land med fattigdomsrate over 30 
data %>% 
  filter(fattigdomsrate > 30)

data %>% 
  filter(fattigdomsrate > 30) %>% 
  count() # med dette argumentet får jeg hvor kun antall land

# OPPGAVE 6

# Henter ut land som har fattigdomsrate over 2 

data %>% 
  filter(fattigdomsrate > 2)

# Henter ut land som har fattigdomsrate under 2 

data %>% 
  filter(fattigdomsrate < 2)


# OPPGAVE 7
# Lager en OLS-modell 

# Når jeg lager en OLS-modell bruker man symbolet ~ 
# for å skille mellom Y og X. 
mod <- lm(fattigdomsrate ~ post_sov, data = data)

# Denne koden printer ut en tabell som gjør at vi kan tolke 
# resultatene fra OLS-modellen 
summary(mod)


# OPPGAVE 8 

#Installerer pakken stargazer - nå kan dere koden 
# husk å kjøre library etter koden er kjørst som jeg har gjort 

library(stargazer)
# Denne koden gjør at tabellen med resultater ser anneledes ut
stargazer(mod, type = "text", align = T)

# Hvis jeg vil lagre tabellen bruker jeg følgende kode
# Her må type-argymentet være html 
# Out er navnet på filen 
stargazer(mod, type = "html", align = T, 
          out = "lagretfil.html")

# Gå til ditt working directory å se at filen er der
