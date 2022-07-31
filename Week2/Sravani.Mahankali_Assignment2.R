## installing the necessary packages

install.packages("faraway", dependencies = TRUE)
install.packages("GGally", dependencies = TRUE)
library("car")

## getting the data set and information on that

require("faraway")
require("GGally")
data("diabetes")
variable.names(diabetes)
help(diabetes)

diabetes <- na.omit(diabetes)[, -1] 
summary(diabetes)

View(diabetes)

## model with glycosylated hemoglobin  and gender

lm.model.1 <- lm(glyhb ~ gender, data = diabetes)
summary(lm.model.1)

## model with glycosylated hemoglobin, location  and gender

lm.model.2 <- lm(glyhb ~ gender+ location, data = diabetes)
summary(lm.model.2)

## calculating the weight/height ratio and waist/hip ratio

diabetes$weight.height <- (diabetes$weight/diabetes$height)
diabetes$waist.hip <- (diabetes$waist/diabetes$hip)

View(diabetes)

## model with cholesterol, stabilized glucose, HDL, cholesterol/HDL ratio, age, and weight/height ratio and glycosylated hemoglobin

lm.model.3 <- lm(glyhb ~ chol+ stab.glu+ hdl+ ratio + age+ weight.height , data = diabetes)
summary(lm.model.3)

## model with cholesterol, stabilized glucose, HDL, cholesterol/HDL ratio, age, and weight/height ratio, interacting weight/height ratio and gender and glycosylated hemoglobin

lm.model.4 <- lm(glyhb ~ chol+ stab.glu+ hdl+ ratio + age+ weight.height + weight.height*gender , data = diabetes)
summary(lm.model.4)

## model with cholesterol, stabilized glucose, HDL, cholesterol/HDL ratio, age, waist/hip ratio, weight/height ratio and glycosylated hemoglobin

lm.model.5 <- lm(glyhb ~ chol+ stab.glu+ hdl+ ratio + age+ weight.height+ waist.hip , data = diabetes)
summary(lm.model.5)

## comparing all the models

anova(lm.model.1, lm.model.2, lm.model.3, lm.model.4, lm.model.5)

## checking assumptions

## multi-collinearity
vif(lm.model.3)

## independence of errors
durbinWatsonTest(lm.model.3)

## homoscadasticity
spreadLevelPlot(lm.model.3) 

plot(lm.model.3$fitted, lm.model.3$residuals, xlab="Fitted values",
     ylab="Residuals", main="Fitted values vs. Residuals - Model 3") ## Model 1

## outliers
influenceIndexPlot(lm.model.3)

## normality of residuals
shapiro.test(lm.model.3$residuals)

## linearity
plot(lm.model.3, 1)

##variance in outcome variable
hist(diabetes$glyhb)
var(diabetes$glyhb)

## checking the linearity of the variables

scatter.smooth(diabetes$chol, diabetes$glyhb)
scatter.smooth(diabetes$stab.glu, diabetes$glyhb)
scatter.smooth(diabetes$hdl, diabetes$glyhb)
scatter.smooth(diabetes$ratio, diabetes$glyhb)
scatter.smooth(diabetes$age, diabetes$glyhb)
scatter.smooth(diabetes$weight.height, diabetes$glyhb)

## transformation on variables

scatter.smooth(log(diabetes$chol), diabetes$glyhb)
scatter.smooth(sqrt(diabetes$chol), diabetes$glyhb)

scatter.smooth(log(diabetes$stab.glu), diabetes$glyhb)
scatter.smooth(sqrt(diabetes$stab.glu), diabetes$glyhb)

scatter.smooth(log(diabetes$ratio), diabetes$glyhb)
scatter.smooth(sqrt(diabetes$ratio), diabetes$glyhb)

## weighted regression analysis

q1 <- summary(diabetes$glyhb)[2]
q3 <- summary(diabetes$glyhb)[5]

diabetes$glyhb.cat = as.factor(ifelse(diabetes$glyhb < q1,
                                 "low",
                                 ifelse(diabetes$glyhb >= q1 & diabetes$glyhb < q3,
                                        "med",
                                        "high")))
summary(diabetes$glyhb.cat)

## model on subset of the data with on glyhb categories

lm.model.6 <- lm(glyhb ~ chol+ stab.glu+ hdl+ ratio + age+ weight.height+ waist.hip , data = diabetes, subset = (diabetes$glyhb.cat=='low'))
summary(lm.model.6)

## calculating weights

groupvar.glyhb <- aggregate(diabetes$glyhb,by=list(diabetes$glyhb.cat), FUN=var) ## compute within-group variance of mpg, for each origin value
print(groupvar.glyhb)
wts <- numeric(nrow(diabetes)) 
vct.categs <- c("low", "med", "high")
for(oid in 1:length(vct.categs)){
  wts[diabetes$glyhb.cat == vct.categs[oid]] <- groupvar.glyhb[oid, 2] ## assign
}

## model with weighted regression on model 3

lm.model.7 <- lm(glyhb ~ chol+ stab.glu+ hdl+ ratio + age+ weight.height+ waist.hip , data = diabetes, weights = 1/wts)
summary(lm.model.7)

## checking if there is good fit on transformed outcome variable

lm.model.7.1 <- lm(log(glyhb) ~ chol+ stab.glu+ hdl+ ratio + age+ weight.height+ waist.hip , data = diabetes, weights = 1/wts)
summary(lm.model.7.1)
