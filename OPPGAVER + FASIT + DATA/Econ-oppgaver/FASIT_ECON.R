
#### FASIT

# 1. Plot inntekt som Y og utdanning som X og lag to 
# sorter observasjonene vha color = gender 

library(ggplot2)

ggplot(data, aes(income, education, col = gender)) + 
  geom_point() 

# 2. Plot inntekt som Y og utdanning X. Lag en 
# regresjonslinje for menn og en for kvinner 

#Dette plottet blit litt likt det over 
# Men man legger på et tilleggs argument 
# Man kan også eventuelt droppe geom_point()

ggplot(data, aes(income, education, col = gender)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# 3. Hvor mange kvinner tjener over 30.000? 
data %>%
  filter(data$income_1>30, gender1<1)

#Svaret blir dermed 7

# 4. Hvor mange menn tjener over 30.000? 

data %>%
  filter(data$income_1>30, gender1>0)

#Svaret er da 11

# 5. Lag en OLS med Y som inntekt, 
# og alder og kjønn som uavhengige variable 

reg <- lm(income_1 ~ age + gender1, data = data)


# 6. Plot modellen din 

ggplot(data = data, aes(gender2 + age, income_1 ))+ geom_smooth(method="lm", formula = y~x) +
  geom_point()

# 7. Lag en ny variabel hvor du slår sammen 
# utdanningsnivåene 2+3 (bachelor + master) til 1 
# og 1 til 0 (high school) 

data$education2 <- ifelse(data$education_1 == "high school", 0, 1)
table(data$education2)

# 8. Lag en ytterligere variabel hvor utdanningsnivået 
# 1 i den forrige variabelen heter "university education"
# mens 0 er "no university education"

data$education3 <- ifelse(data$education2 == 1, "university education", "no university education")

table(data$education3)

# 9. Hva er gjennomsnittsalderen til menn og kvinner?
# hva er forskjellen?

mean(data$age[data$gender1==0])

mean(data$age[data$gender1==1])

mean(data$age[data$gender1==1]) - mean(data$age[data$gender1==0])

# 10. Lag et nytt datasett med observasjoner som 
# har en høyere alder en gjennosnittet 
# Du skal ta utgangspunkt i det datasettet 
# Finne gjennomsnittet 

mean(data$age) # den er 41.2

library(tidyverse)
data1 <- data %>% 
  filter(age > 41 )

View(data1) # Alle er over 41 år i dette datasettet 

# 11. Estimer gjennomsnittsalderen i det nye datasettet

# Da må du estimere alderen i det nye datasettet 

mean(data1$age) # Den er 54 
# 12. Sentrer variabelen alder

# Så sentrere denne variabelen 

data1$age_m <- mean(data1$age)

data1$age_sentrert <- data1$age - data1$age_m

View(data)
# 13. Ta utgangspunkt i variabelen inntekt 
# Finn gjennomsnittet 
# Bruk gjennomsnittet til å dele variabelen i to 
# Du skal opprette en ny variabel hvor personer som har
# inntekt som er lavere enn gjennomsnittet får verdien 0
# og personer som har inntekt over gjennomsnittet får verdien 1

view(data)

mean(data$income)

data$income3 <- ifelse(data$income < 262977.5, 0, 1)

view(data)
