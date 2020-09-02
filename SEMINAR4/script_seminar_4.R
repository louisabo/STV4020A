#### ==== VELKOMMEN TIL SISTE SEMINAR ==== ####
#### ==== LOGISTISK REGRESJON ER GØY  ==== ####
#### ==== DA KAN MAN LAGE MODELLER    ==== ####
#### ==== SOM PREDIKERER VERDEN OG    ==== ####
#### ==== VURDERE MODELLER UT I FRA   ==== ####
#### ==== HVOR GODE DE I ER TIL Å     ==== ####
#### ==== FORTELLE MEG OM FREMTIDEN   ==== ####


# For dette seminaret har jeg brukt min hjemmeoppgave
# fra fjort som undervinsingsmateriell. Jeg jobbet med ESS
# Det som er under er også det jeg leverte inn med 
# selve oppgave teksten, og kan derfor være et godt 
# utgangspunkt til hvordan lage script som skal leveres. 
# Jeg valgte å beholde databehandlingen jeg gjorde i forkant, 
# dette går vi raskt gjennom før vi hopper i gang med 
# logistisk regresjon! 

# This script is organized as follows: 
# 1. European Social Survey download   
# 2. Recoding variables - vote to leave EU
## 2.1 Immigration1 
## 2.2 Money 
## 2.3 Economy 
## 2.4 Gender 
## 2.5 Age 
## 2.6 Years of education
# 3. Split data set  
# 4. Logistical regression 
## 4.1 Caret model
# 5. Calculating results from logit to probabilitites 
# 6. Data visualization: predicted values X on Y
# 7. Key assumptions 
# 8. Goodness of fit tests: 
## 8.1 Log likelihood 
## 8.2 Pseudo R^2 
## 8.3 Hosmer-Lemeshow-test 
# 9.0. How well is my model at predicting 
## 9.1 ROC Curve 
## 9.2 Predicting out of data


library(devtools)
library(ggplot2)
library(tidyverse)
library(moments)
library(corrplot)
library(psy)
library(stargazer)
library(caret)
library(ggpubr)
library(pROC)
library(pscl)

setwd("~/Desktop/R-MATERIELL/SEMINAR4")
##############################
###### 1.0 ESS DOWNLOAD ###### 
##############################

library(essurvey)

data <- import_country("France", 8, 
                       ess_email = "louisabo@student.sv.uio.no", 
                       format = "stata")

recode_missings(data)


###############################################################
###### 2.0 RECODING VOTE TO LEAVE OR REMAIN MEMBER OF EU ######
###############################################################

# Vote EU: Remain member of EU - vteurmmb --> vteu
# 1 = Remain member of EU --> 0
# 2 = Leave the EU --> 1
# Changing values in variable to 0 and 1 
data$vteu <- ifelse(data$vteurmmb == 2, "1", 
                    ifelse(data$vteurmmb == 1, "0", data$vteurmmb))

table(data$vteu)
table(data$vteurmmb)


# Removing all those who do not have the values 1 or 0.  
data$vteu <- ifelse(data$vteu > 11, NA, data$vteu)

# Making the variable numering
data$vteu <- as.numeric(data$vteu)
class(data$vteu)

table(data$vteu)
# 445 votes to leave 
# 1370 votes to remain 


######################################
###### 2.1 RECODING IMMIGRATION ######
######################################

show(data$imwbcnt)

# Changing direction of scale, 
# After change : 10 = worse place to live 
# 0 = better place to live 

data$immigration1 <- data$imwbcnt*-1+10

table(data$imwbcnt)
table(data$immigration1)

data$immigration1 <- as.numeric(data$immigration1)

# Removing those who do not have values 0-10 

data$immigration1 <- ifelse(data$immigration1 > 11, NA, data$immigration1)


################################
###### 2.2 RECODING MONEY ######
################################

# How likely not enough money for household necessities next 12 months
# Making sure the variable starts at 0 
# Range 0-3: 0 = not likely, 3 = very likely 

data$money <- ifelse(data$lknemny == "1", "0", 
                     ifelse(data$lknemny == "2", "1", 
                            ifelse(data$lknemny == "3", "2",
                                   ifelse(data$lknemny == "4", "3", 
                                          data$lknemny))))

# Class 
data$money <- as.numeric(data$money)
class(data$money) # = numeric 

table(data$money)

data$money <- ifelse(data$money > 11, NA, data$money)

##################################
###### 2.3 RECODING ECONOMY ######
##################################

# How satisfied with present state of economy in country 

show(data$stfeco) # values ranging from 0-10 
# 0 = Extremely satisfied 
# 10 = Extremely dissatisfied 

data$economy <- data$stfeco*-1+10

table(data$economy)
table(data$stfeco)
# Class
class(data$economy) # = Numeric 
data$economy <- as.numeric(data$economy)

data$economy <- ifelse(data$economy > 11, NA, data$economy)


#################################
###### 2.4 RECODING GENDER ###### 
#################################
# 1. gndr : Gender 

# Overview, distribution
table(data$gndr)
show(data$gndr)

# Changing values from 1 and 2 to 0 and 1
# 1 = male = 953
# 0 = female = 1117

data$gender <- ifelse(data$gndr == 1, "1", 
                      ifelse(data$gndr == 2, "0", 
                             data$gndr))

# Removing values that cannot be identified as 0 or 1 

data$gender <- ifelse(data$gender > 3, NA, data$gender)

show(data$gndr)
table(data$gender)
table(data$gndr)
class(data$gender) # = character 
data$gender <- as.numeric(data$gender)
class(data$gender) # = numeric 

################################
###### 2.5 (RECODING) AGE ######
################################

table(data$agea)
show(data$agea)
class(data$agea)

data$agea <- as.numeric(data$agea)
table(data$agea)

####################################
###### 2.6 YEARS OF EDUCATION ######
####################################

show(data$eduyrs)
table(data$eduyrs)
class(data$eduyrs)
data$eduyrs <- as.numeric(data$eduyrs)

data$eduyrs <- ifelse(data$eduyrs > 26, NA, data$eduyrs)
table(data$eduyrs)

#########################################
###### 3.0 SET SEED AND SPLIT DATA ###### 
#########################################
# New dataset, no NA.
data1 <- data %>% 
  drop_na(vteu, money, eduyrs, economy, immigration1, gender, agea)

# New dataset for summary of stats 
nyedata <- data1 %>% 
  select(money, eduyrs, economy, immigration1, agea)

# Summary statistics table 
stargazer(as.data.frame(nyedata), type = "html", 
          out = "Statistic_2.html", 
          title= "Summary_statistics")


# Frequency Gender 

ggplot(data = data1) + 
  geom_bar(mapping = aes(x = as.factor(gender))) +
  xlab("Gender")


# Frequency VOTE EU

ggplot(data = data1) + 
  geom_bar(mapping = aes(x = as_factor(vteu))) + 
  xlab("Vote to leave or remain member of EU")

######################### SET SEED
# Before running the regression 
# I make one test dataset and one train 
# I use the train dataset in the model 
# The testing data set is saved to see how well my model predicts

set.seed(24) 

train <- sample_frac(data1, 0.75)
testing <- data1 %>% 
  anti_join(train)

#######################################
###### 4.0 LOGISTICAL REGRESSION ######
#######################################

# Model 1 - Without control variable immigration
gm4 <- glm(vteu ~ agea + gender + eduyrs + economy + money,
           data = train, family = binomial(link = "logit"))
summary(gm4)

# Model 2 - With control variable immigration

gm5 <- glm(vteu ~ agea + gender + eduyrs + economy + money + immigration1, 
           data = train, family = binomial(link = "logit"))
summary(gm5)


stargazer(gm4, gm5, type = "text", title = "Results", align = T)

# More detailed table for paper
stargazer(gm4, gm5, type = "html",
          out= "Figure_1.html",
          title="Regression Results", align=TRUE, 
          dep.var.labels=c("Would vote for France to remain a member of European Union or leave"), 
          covariate.labels=c("Age",
                             "Man", 
                             "Years of education",
                             "Satisfaction with economy",
                             "Likely not enough money", 
                             "Anti-immigration attitudes"), no.space=TRUE)



#####################################
###### 5.0 CALCULATING RESULTS ######
#####################################
# RESULTS GM5
# Calculating from logodds to oddsratio 

summary(gm5)
coef(gm5) # Shows all my coefficients 

#If you are a man the odds of voting leave the EU
# increases with 29 % - ceteris paribus

# Effect of economy on vote leave
(exp(coef(gm5)[5])-1)*100 #28

# If "dissatisfaction" with the economy increases with 1 unit on the scale
# the odds of voting to leave the EU increases with 28%, ceteris paribus


# Effect of money on vote leave %-change 
(exp(coef(gm5)[6])-1)*100 # 42%

(exp(coef(gm4)[6])-1)*100 #results model 4
(exp(coef(gm4)[5])-1)*100 #results model 4
# If the likelihood of not having enough money in the next 12 months 
# increases with 1 unit on the scale, the odds of voting to leave the EU
# increases with 42%, ceteris paribus. 


# Effect of immgiration on vote leave in %-change 
(exp(coef(gm5)[7])-1)*100 #38%

# If "thinking" that immigrants make the country worse increases with 
# 1 unit on the scale, the odds of voting to leave the EU icreases with
# 38%, ceteris paribus. 

#########################################
###### 6.0 PREDICTED PROBABILITIES ###### 
#########################################

data_for_prediction <- tibble(
  agea = median(train$agea),
  gender = median(train$gender), 
  eduyrs = median(train$eduyrs), 
  money = median(train$money), 
  economy = seq(min(train$economy), 
              max(train$economy), .1), 
  immigration1 = median(train$immigration1))

predicted_data <- predict(gm5, newdata = data_for_prediction, type = "link", 
                          se = T)

plot_data <- cbind(predicted_data, data_for_prediction)

plot_data$low  <- exp(plot_data$fit - 1.96*plot_data$se)/(1 + exp(plot_data$fit - 1.96*plot_data$se))
plot_data$high <- exp(plot_data$fit + 1.96*plot_data$se)/(1 + exp(plot_data$fit + 1.96*plot_data$se))
plot_data$fit <- exp(plot_data$fit)/(1+ exp(plot_data$fit))

p <- ggplot(train, aes(x = economy, y = vteu)) +
  geom_rangeframe() +
  ggtitle("Vote to leave or remain EU")  + 
  theme_tufte() + 
  ylab("Predicted probabilities for voting leave") +
  xlab("Economy") + 
  geom_point() +
  geom_ribbon(data = plot_data, aes(y=fit, ymin=low, ymax=high), alpha=.2) +
  geom_line(data = plot_data, aes(y=fit))
p

library(ggthemes)
library(ggpubr)

#################################
###### 7.0 KEY ASSUMPTIONS ######
#################################
# 1. The regression has the shape of an -S-

preds <- predict(gm5, train, type = "response")

newdata1 <- train %>% 
  select(economy, money, 
         agea, immigration1, eduyrs)

newdata1 <- newdata1 %>%
  mutate(logit = log(preds/(1-preds))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)


ggplot(newdata1, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")


#######################
# 2. Influential values 

# Cook's distance 
plot(gm5, which = 4, id.n = 3)

# Computing standardized residuals and Cook's D

# install.packages("broom")
library(broom)
library(tidyverse)
# Model results:

model.data <- augment(gm5) %>% 
  mutate(index = 1:n()) 

model.data %>% top_n(3, .cooksd)

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = vteu), alpha = .5) +
  theme_bw() + 
  ylab("Standardized residuals") + 
  xlab("Observations") 

model.data %>% 
  filter(abs(.std.resid) > 3)



# We have no influential observations 

##########################
# 3. No multicollinearity 

library(car)

car::vif(gm5)

# None of my variables have a VIF-value above 2.


#################################
###### 8.0 GOODNESS OF FIT ###### 
#################################
### ANOVA 

anova(gm4, gm5, test = "LRT")

### PSEUDO R^2 
pR2(gm5)
pR2(gm4)

library(ResourceSelection)
### HOSMER-LEMESHOW TEST 
hl <- hoslem.test(gm5$y, fitted(gm5), g = 10)
hoslem.test(gm5$y, fitted(gm5), g = 10)

cbind(hl$expected, hl$observed)

summary(hl)

hl1 <- hoslem.test(gm4$y, fitted(gm4), g = 10)
hoslem.test(gm4$y, fitted(gm4), g = 10)

cbind(hl1$expected, hl1$observed)

summary(hl1)


#################################################
###### 9.0 HOW WELL DOES MY MODEL PREDICT? ###### 
#################################################
library(pROC)

 
mod5_train_roc <- roc(response = train$vteu, 
                      predictor = predict(gm5, train))
mod4_train_roc <- roc(response = train$vteu,
                      predictor = predict(gm4, train))

# R-curve for train data
ggroc(list("mod_2" = mod5_train_roc,
           "mod_1" = mod4_train_roc),
      alpha = 0.5, 
      linetype = 1, 
      size = 1)

## AUC

auc(mod5_train_roc)
auc(mod4_train_roc)

## ROC-curve test data
mod5_test_roc <- roc(response = testing$vteu, 
                     predictor = predict(gm5, testing))
mod4_test_roc <- roc(response = testing$vteu,
                     predictor = predict(gm4, testing))

ggroc(list("mod_2" = mod5_test_roc,
           "mod_1" = mod4_test_roc),
      alpha = 0.5, 
      linetype = 1, 
      size = 1)

auc(mod4_test_roc)
auc(mod5_test_roc)

# Confusion matrix to find which predictions my model predicts wrong

preds_test <- predict(gm5, testing, type =  "response")
preds_train <- predict (gm5, train, type = "response")



cm_test <- table(predikert = ifelse(preds_test > 0.7682, 1, 0),
                 faktisk = testing$vteu)
cm_test

cm_train <- table(prediket = ifelse(preds_train > 0.7741, 1, 0), 
                  faktisk = train$vteu)

cm_train



