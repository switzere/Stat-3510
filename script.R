library(AICcmodavg)
library(ggplot2)
library(ggthemes)

data <- read.csv(file.choose(), header=TRUE, fileEncoding="UTF-8-BOM")

ggcorrplot(data)

modelGender <- glm(Earnings ~ Gender, data = data)

modelEdu <- glm(Earnings ~ Education, data = data)

maleData <- data[data$Gender == "Male", ] 
modelEdMale <- glm(Earnings ~ Education, data = maleData)

femaleData <- data[data$Gender == "Female", ] 
modelEdFemale <- glm(Earnings ~ Education, data = femaleData)

noCanData <- data[data$Province != "Canada", ]

level_order <- c('High school diploma', 'Apprenticeship certificate', 'College diploma', 'Bachelor\'s degree')
ggplot(noCanData, aes(y=Earnings, x=factor(Education, level=level_order), shape=Gender, color=Province)) +
  geom_point() +
  ggtitle("Median Annual Earnings") + 
  xlab("Education Level") +
  expand_limits(y = 0) +
  theme_calc()

#model1
model1 <- glm(Earnings ~ factor(Gender) + factor(Education) + factor(Province), data = noCanData)
summary(model1)

ggplot(noCanData, aes(y=Earnings, x=factor(Education, level=level_order), shape=Gender, color=Province)) +
  geom_point() +
  ggtitle("Median Annual Earnings (Model 1)") + 
  xlab("Education Level") +
  expand_limits(y = 0) +
  geom_point(alpha=0.01, size=4, aes(x = "High school diploma", y = 60009+19136-7209-12836, color = "Ont", shape = "Male")) +
  geom_point(alpha=0.01, size=4, aes(x = "Apprenticeship certificate", y = 60009+19136+0-12836, color = "Ont", shape = "Male")) +
  geom_point(alpha=0.01, size=4, aes(x = "College diploma", y = 60009+19136+2880-12836, color = "Ont", shape = "Male")) +
  geom_point(alpha=0.01, size=4, aes(x = "Bachelor's degree", y = 60009+19136+19110-12836, color = "Ont", shape = "Male")) +  theme_calc()

#model2
model2 <- glm(Earnings ~ factor(Gender) + factor(Education), data = noCanData)
summary(model2)

ggplot(noCanData, aes(y=Earnings, x=factor(Education, level=level_order), shape=Gender, color=Province)) +
  geom_point() +
  ggtitle("Median Annual Earnings (Model 2)") + 
  xlab("Education Level") +
  expand_limits(y = 0) +
  geom_point(alpha=0.01, size=4, aes(x = "High school diploma", y = 44691+19136-7209, color = "Ont", shape = "Male")) +
  geom_point(alpha=0.01, size=4, aes(x = "Apprenticeship certificate", y = 44691+19136+0, color = "Ont", shape = "Male")) +
  geom_point(alpha=0.01, size=4, aes(x = "College diploma", y = 44691+19136+2880, color = "Ont", shape = "Male")) +
  geom_point(alpha=0.01, size=4, aes(x = "Bachelor's degree", y = 44691+19136+19110, color = "Ont", shape = "Male")) +  theme_calc()

#model3
model3 <- glm(Earnings ~ factor(Gender) * factor(Education), data = noCanData)
summary(model3)

ggplot(noCanData, aes(y=Earnings, x=factor(Education, level=level_order), shape=Gender, color=Province)) +
  geom_point() +
  ggtitle("Median Annual Earnings (Model 3)") + 
  xlab("Education Level") +
  expand_limits(y = 0) +
  geom_point(alpha=0.01, size=4, aes(x = "High school diploma", y = 38559+31400+2021-18459, color = "Ont", shape = "Male")) +
  geom_point(alpha=0.01, size=4, aes(x = "Apprenticeship certificate", y = 38559+31400+0+0, color = "Ont", shape = "Male")) +
  geom_point(alpha=0.01, size=4, aes(x = "College diploma", y = 38559+31400+8573-11386, color = "Ont", shape = "Male")) +
  geom_point(alpha=0.01, size=4, aes(x = "Bachelor's degree", y = 38559+31400+28715-19210, color = "Ont", shape = "Male")) +  theme_calc()

#model4
model4 <- glm(Earnings ~ factor(Gender) * factor(Education) + factor(Province), data = noCanData)
summary(model4)

ggplot(noCanData, aes(y=Earnings, x=factor(Education, level=level_order), shape=Gender, color=Province)) +
  geom_point() +
  ggtitle("Median Annual Earnings (Model 4)") + 
  xlab("Education Level") +
  expand_limits(y = 0) +
  geom_point(alpha=0.01, size=4, aes(x = "High school diploma", y = 53877+31400+2021-12836-18459, color = "Ont", shape = "Male")) +
  geom_point(alpha=0.01, size=4, aes(x = "Apprenticeship certificate", y = 53877+31400+0-12836+0, color = "Ont", shape = "Male")) +
  geom_point(alpha=0.01, size=4, aes(x = "College diploma", y = 53877+31400+8573-12836-11396, color = "Ont", shape = "Male")) +
  geom_point(alpha=0.01, size=4, aes(x = "Bachelor's degree", y = 53877+31400+28715-12836-19210, color = "Ont", shape = "Male")) +
  theme_calc()

#model5
model5 <- glm(Earnings ~ factor(Education), data = noCanData)
summary(model5)

ggplot(noCanData, aes(y=Earnings, x=factor(Education, level=level_order), shape=Gender, color=Province)) +
  geom_point() +
  ggtitle("Median Annual Earnings (Model 5)") + 
  xlab("Education Level") +
  expand_limits(y = 0) +
  geom_point(alpha=0.01, size=4, aes(x = "High school diploma", y = 54259-7209, color = "Ont", shape = "Male")) +
  geom_point(alpha=0.01, size=4, aes(x = "Apprenticeship certificate", y = 54259+0, color = "Ont", shape = "Male")) +
  geom_point(alpha=0.01, size=4, aes(x = "College diploma", y = 54259+2880, color = "Ont", shape = "Male")) +
  geom_point(alpha=0.01, size=4, aes(x = "Bachelor's degree", y = 54259+19110, color = "Ont", shape = "Male")) +
  theme_calc()

aic.list <- list(model1, model2, model3, model4, model5)
aic.names <- c("model1", "model2", "model3", "model4", "model5")
aictab(aic.list, modnames = aic.names)


par(mfrow = c(2,2))
plot(model4)

plot(predict.glm(model1, type = "link"), residuals.glm(model1, "deviance"), xlab="Linear Predictor",
     ylab="deviance residuals", main = "Linear predictor vs Deviance Residuals")




model <- glm(Earnings ~ factor(Gender) + factor(Education), data = data)

fullModel <- glm(Earnings ~ factor(Gender) + factor(Education) + factor(Province), data = data)

interactionModel <- glm(Earnings ~ factor(Gender)*factor(Education), data = data)

onlyUniData <- data
onlyUniData[onlyUniData == "High school diploma"] <- "Not"
onlyUniData[onlyUniData == "Apprenticeship certificate"] <- "Not"
onlyUniData[onlyUniData == "College diploma"] <- "Not"



onlyUniModel1 <- glm(Earnings ~ factor(Gender)+factor(Education), data = onlyUniData)
onlyUniModel2 <- glm(Earnings ~ factor(Gender)*factor(Education), data = onlyUniData)

anova(onlyUniModel2, onlyUniModel1, test="Chisq")

aic.list <- list(onlyUniModel1, onlyUniModel2)
aic.names <- c("onlyUniModel1", "onlyUniModel2")
aictab(aic.list, modnames = aic.names)







anova(modelEdMale, modelEdFemale, test="LR")
anova(model, modelGender, test="LR")
anova(modelEdu, modelGender, test="LR")
anova(modelGender, fullModel, test="F")
anova(interactionModel, modelGender, test="LR")

anova(interactionModel, modelGender, test="F")

a <- aov(Earnings ~ factor(Gender) + factor(Education), data = data)
a <- aov(Earnings ~ factor(Gender), data = data)

aic.list <- list(model, modelEdu, modelGender, interactionModel)
aic.names <- c("model", "modelEdu", "modelGender", "interactionModel")
aictab(aic.list, modnames = aic.names)

iM <- aov(interactionModel, data = data)

par(mfrow=c(2,2))
plot(iM)


#Could use offset if we get the population of each province(it would need to be the amount of people 25-64 who worked full time)
#Should remove Canada from the data set
#Why are we calculating it by hand?

#Look at the analysis part of the assignment description, most of this is missing
#Background research on the question/hypothesis
#Exploratory analysis to investigate the data set
#statistical analyses to address the hypothesis
#summarization and interpretation of the results
#Presentation of the work conducted in both an oral presentation and written report
