library(dplyr)
library(haven)
library(MASS)
library(ggplot2)
library(plotly)
library(GGally)
library(psych)
library(pander)
library(useful)
library(mice)
library(ggcorrplot)

# When reading the data sets, make sure the path is correct.
LDL2017 <- read_xpt("LDL_Trigy.XPT",
                    col_select = NULL,
                    skip = 0,
                    n_max = Inf,
                    .name_repair = "unique")
LDL2017
BodyMeasures2017 <- read_xpt("Body_Measures.XPT",
                             col_select = NULL,
                             skip = 0,
                             n_max = Inf,
                             .name_repair = "unique")

TotalCholestrol <- read_xpt("Total_Cholestrol.XPT",
                            col_select = NULL,
                            skip = 0,
                            n_max = Inf,
                            .name_repair = "unique")

BloodPressure <- read_xpt("Blood_Pressure.XPT",
                          col_select = NULL,
                          skip = 0,
                          n_max = Inf,
                          .name_repair = "unique")

demographics <- read_xpt("Demo_pop.XPT",
                         col_select = NULL,
                         skip = 0,
                         n_max = Inf,
                         .name_repair = "unique")

cardiohealth <- read_xpt("Cardio Health.XPT",
                         col_select = NULL,
                         skip = 0,
                         n_max = Inf,
                         .name_repair = "unique")


#"BMXWT" is weight
#"BMXHT" is standing height
#"BMXBMI" BMI
#"BMXWAIST" waist circumference


BodyVariables <- c("SEQN","BMXBMI","BMXWT","BMXHT")
BodyMeasures2017 <- BodyMeasures2017[BodyVariables]
names(BodyMeasures2017)[names(BodyMeasures2017) == 'BMXWT'] <- "WEIGHT"
names(BodyMeasures2017)[names(BodyMeasures2017) == 'BMXHT'] <- "HEIGHT"
names(BodyMeasures2017)[names(BodyMeasures2017) == 'BMXBMI'] <- "BMI"

#Number of missing values in body measurement
frequency <- colSums(is.na(BodyMeasures2017))
proportion <- (colSums(is.na(BodyMeasures2017)))/8704
pander(cbind(frequency, proportion))


#Blood pressure, systolic and diastolic
#first reading to 4th reading
#average out systolic and diastolic
pvars <- c("SEQN","BPXSY2", "BPXSY3")
BloodPressure <- BloodPressure[pvars]

#average out the LDL columns
BloodPressure$SYSTOLIC <- rowMeans(BloodPressure[ , c(2:3)], na.rm=TRUE)
rem <- c("SEQN","SYSTOLIC")
BloodPressure <- BloodPressure[rem]

#Demographic data
myvars <- c("SEQN","RIAGENDR", "RIDAGEYR")
demographics <- demographics[myvars]
names(demographics)[names(demographics) == 'RIAGENDR'] <- "GENDER"
names(demographics)[names(demographics) == 'RIDAGEYR'] <- "AGE_YEAR"

#LDL data set, subest the ones with mg/dL
#LBXTR is trygicerides
#"LBDLDL", "LBDLDLM", "LBDLDLN" are LDLs mg/dL
LDLvar <- c("SEQN", "LBXTR", "LBDLDL", "LBDLDLM", "LBDLDLN")
LDL2017 <- LDL2017[LDLvar]

#average out the LDL columns
LDL2017$LDL <- rowMeans(LDL2017[ , c(3:5)], na.rm=TRUE)

#remove LBDLDL, LBDLDLM, LBDLDLN
re <- c("SEQN", "LBXTR", "LDL")
LDL2017 <- LDL2017[re]

#LDL2017$LDL <- mice(LDL2017$LDL, method = "norm.predict", m = 1)
names(LDL2017)[names(LDL2017) == 'LBXTR'] <- "TRYGLICERIDES"

#Number of missing values in LDL and triglycerides
frequency <- colSums(is.na(LDL2017))
proportion <- (colSums(is.na(LDL2017)))/8704
pander(cbind(frequency, proportion))



#Join Datasets
demo_blood <- full_join(BloodPressure,demographics, by="SEQN")
joined <- full_join(BodyMeasures2017,demo_blood, by="SEQN")
joined2 <- full_join(joined, LDL2017, by = "SEQN")

#number
frequency <- colSums(is.na(joined2))
proportion <- (colSums(is.na(joined2)))/9254
pander(cbind(frequency, proportion))

# Imputation
Data <- mice(joined2,m=5,maxit=50,meth='pmm',seed=500)
joined3 <- complete(Data)

joined3$T_BINARY <- ifelse(joined3$TRIGLYCERIDES >= 500, 3,
                           ifelse(joined3$TRIGLYCERIDES >= 200, 2,
                                  ifelse(joined3$TRIGLYCERIDES >= 150,1,0)))
joined3$LDL_BINARY <- ifelse(joined3$LDL >= 190, 3,
                             ifelse(joined3$LDL >= 160, 2,
                                  ifelse(joined3$LDL >= 130,1,0)))
joined3$HIGHCHANCE <- with(joined3,
                               ifelse(T_BINARY==1 &
                                        LDL_BINARY==1, 1,0))

joined3$OBESITY <- ifelse(joined3$BMI >= 30, 1,0)
joined3$SYSTOLIC <- ifelse(joined3$SYSTOLIC >= 120, 1, 0)

joined3$GENDER <- ifelse(joined3$GENDER==1,"MALE","FEMALE")
drops <- c("SEQN","BMI","WEIGHT","HEIGHT","TRIGLYCERIDES","LDL")
joined4 <- joined3[, !(names(joined3) %in% drops)]
head(joined4)


library(stats)
library(gridExtra)

library(vcd)

joined4$GENDER <- ifelse(joined4$GENDER==1,"MALE","FEMALE")
joined4$GENDER <- as.factor(joined4$GENDER)
joined4$SHORTBREATH <- as.factor(joined4$SHORTBREATH)
joined4$CHESTPAIN <- as.factor(joined4$CHESTPAIN)


ggpairs(joined3, columns = c("TRIGLYCERIDES", "LDL", "HIGHCHANCE"))

plot3 <- ggplot(joined3, aes(BMI, WEIGHT),
       cor.coef = TRUE, cor.method = "pearson") +
  geom_point(alpha=0.9, aes(color = HIGHCHANCE),size=0.5) +
  geom_smooth() +
  scale_y_log10() +
  scale_x_log10()

plot4 <- ggplot(joined3, aes(BMI, WEIGHT),
       cor.coef = TRUE, cor.method = "pearson") +
  geom_point(alpha=0.9, aes(color = HIGHCHANCE), size=0.5) +
  geom_smooth()

grid.arrange(plot3, plot4, nrow=2)




plot1 <- ggplot(joined3, aes(TRIGLYCERIDES, LDL),
       cor.coef = TRUE, cor.method = "pearson") +
  geom_point(alpha=0.9, aes(color = HIGHCHANCE)) +
  geom_smooth() +
  scale_y_log10() +
  scale_x_log10()

plot2 <- ggplot(joined3, aes(TRIGLYCERIDES, LDL),
       cor.coef = TRUE, cor.method = "pearson") +
  geom_point(alpha=0.9, aes(color = HIGHCHANCE)) +
  geom_smooth()

grid.arrange(plot1, plot2, nrow=2)


ggscatter(joined3, x = "TRIGLYCERIDES", y = "LDL", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          alpha=I(0.02)) +
  scale_y_log10() +
  scale_x_log10()


a <- ggscatter(joined3, x = "AGE_YEAR", y = "LDL", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          alpha=I(0.005))

b <- joined3 %>%
  ggplot(aes(x=AGE_YEAR,y=LDL,color=GENDER))+
  geom_point(alpha=0.001)+xlab("Age") +geom_smooth()+
  ylab("LDL")+
  guides(fill = guide_legend(title = "Gender"))

grid.arrange(a, b, ncol=2)


corre <- joined3[,c("BMI","WEIGHT","HEIGHT","TRIGLYCERIDES")]
pairs(corre, pch = 19)

rela <- joined3[,c("BMI","WEIGHT","HEIGHT","TRIGLYCERIDES",
                   "SHORTBREATH","T_BINARY","LDL_BINARY","HIGHCHANCE")]
pairs.panels(rela, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
             )


# for detailed analysis report

# To help you guys a little bit

# split the data
set.seed(3)

#70% of the sample size
smp_size <- floor(0.75 * nrow(joined4))
train_ind <- sample(seq_len(nrow(joined4)), size = smp_size)
train <- joined4[train_ind, ]
test <- joined4[-train_ind, ]


# Fit the model
model1 <- glm(HIGHCHANCE ~ .,family=binomial(link="logit"),data = train)
summary(model1) #AIC is 998.63

stepAIC(model1) # Step:  992.5 lowest AIC
# Call:
# lm(formula = HIGHCHANCE ~ BMI + WEIGHT + HEIGHT + TRIGLYCERIDES + 
#     SHORTBREATH + T_BINARY + LDL_BINARY, data = train)


# Then
#by choosing the best AIC, fit the model again
model2 <- glm(formula = HIGHCHANCE ~ GENDER + AGE_YEAR + AGE_MONTH + SHORTBREATH + 
    T_BINARY + LDL_BINARY, family = binomial(link = "logit"), 
    data = train)


#Compare 2 models
anova(model1, model2, test='Chisq')

glance(model1) %>%
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)
glance(model2) %>%
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)






#take the columns from the ones with lowest AIC and put it in the new data frame

sample2 <- joined4[, c("GENDER","AGE_YEAR","AGE_MONTH","SHORTBREATH","T_BINARY","LDL_BINARY","HIGHCHANCE")]

# split the model again

set.seed(3)

#70% of the sample size
smp_size <- floor(0.75 * nrow(sample2))
train_ind <- sample(seq_len(nrow(sample2)), size = smp_size)
train2 <- sample2[train_ind, ]
test2 <- sample2[-train_ind, ]

model2 <- glm(HIGHCHANCE ~ GENDER + AGE_YEAR + AGE_MONTH + SHORTBREATH + T_BINARY + LDL_BINARY, data = train2)
summary(model2)







################# Accuracy and its graph

p.hat <- predict(model2, newdata = test2, type = "response")

sort(unique(factor(test2$HIGHCHANCE)))


pred <- prediction(p.hat,test2$HIGHCHANCE==c("0","1"))
rocc <- performance(pred,measure = "tpr", x.measure = "fpr")

auc <- performance(pred,measure = "auc")@y.values[[1]]
auc

# Plot the AUC Graph
plot(rocc)
points(c(0,1), c(0,1),type = "l", lty=2,col=2,lwd=1.5)
text(x=0.25,y=0.65,paste("AUC = ",round(auc,3),sep= ""))

#################






############### RUN THESE CHUNK (UNTIL THE END) IN ONE GO, will take about 10-30 minutes (depend on how many cores in ones machine)

############### this works ONLY on categorical predictor output, which is in our case, the HIGHCHANCE variable.

library(boot)
library(foreach)
library(parallel)
library(doParallel)
require(foreach)

#######################################
total.error.rate <- function(r, p){
  mean(r != as.numeric(p > 0.5))
}
area.under.curve <- function(r, p=0){
  require(ROCR)
  pred <- prediction(p, r)
  auc <- performance(pred, measure = "auc")
  auc@y.values[[1]]
}

variable.indices <- c(1,2,3,4,5,6,7)

all.comb <- expand.grid(as.data.frame(matrix(rep(0 : 1, length(variable.indices)),
                                             nrow = 2)))[-1,]


nrep <- 20

nclust <- makeCluster(detectCores() * 0.90)
registerDoParallel(nclust)
# Set random number generator seed for replicability of results.
set.seed(1)
# View CPU run time for processes up until this point.
before <- proc.time()

######################
## Total error rate ##
######################


error.rate.parallel <- foreach(i = 1 : nrep, .combine = "rbind", .packages = "boot") %:%
  foreach(j = 1 : nrow(all.comb), .combine = "c") %dopar%
  {
    logistic.regression.model <-glm(as.formula(paste("HIGHCHANCE~",
                                                     paste(names(sample2)[variable.indices[
                                                       all.comb[j,] == 1]], collapse = " + "))),
                                    data = sample2, family = "binomial")
    return(cv.glm(sample2, logistic.regression.model,
                  cost = total.error.rate, K = 10)$delta[1])
  }
AUC.parallel <-
  foreach(i = 1 : nrep, .combine = "rbind", .packages = "boot") %:%
    foreach(j = 1 : nrow(all.comb), .combine = "c") %dopar%
    {
      logistic.regression.model <-
        glm(as.formula(paste("HIGHCHANCE~",
                         paste(names(sample2)[variable.indices[
                           all.comb[j,] == 1]], collapse = " + "))),
          data = sample2, family = "binomial")
      return(cv.glm(sample2, logistic.regression.model,
                cost = area.under.curve, K = 10)$delta[1])
}


# Shut down cores.
stopCluster(nclust)

######################
## Total error rate ##
######################
# View error rates according to model.

boxplot(error.rate.parallel ~ matrix(rep(1 : nrow(all.comb), each = nrep),
                                     nrow = nrep),
        xlab = "Model", ylab = "Error rate")

# View all models within one SE of the best model.
best.models.error.rate <-
  (1 : nrow(all.comb))[apply(error.rate.parallel, 2, mean) <=
                         min(apply(error.rate.parallel, 2, mean)
                             +apply(error.rate.parallel, 2, sd))]

for(i in 1 : length(best.models.error.rate))
  {
  cat(paste("Model ", i, ":\n"))
  print(names(sample2)[variable.indices[all.comb[
    best.models.error.rate[i], ] == 1]]) # Variable names
  print(apply(error.rate.parallel, 2, mean)[best.models.error.rate[i]]) # Error rate
  cat("\n")
  }

boxplot(AUC.parallel ~ matrix(rep(1 : nrow(all.comb), each = nrep),
                              nrow = nrep), xlab = "Model", ylab = "AUC")
# View all models within one SE of the best model.
best.models.AUC <- (1 : nrow(all.comb))[apply(AUC.parallel, 2, mean) >=
                                          max(apply(AUC.parallel, 2, mean)
                                              -apply(AUC.parallel, 2, sd))]
for(i in 1 : length(best.models.AUC))
{
    cat(paste("Model ", i, ":\n"))
    print(names(sample2)[variable.indices[all.comb[
      best.models.AUC[i], ] == 1]]) # Variable names
    print(apply(AUC.parallel, 2, mean)[best.models.AUC[i]]) # AUC
    cat("\n")
}
########################
