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

LDL2017 <- read_xpt("~/Desktop/VUW/2021/Trimester 2/DATA301/Project/LDL_Trigy.XPT",
                    col_select = NULL,
                    skip = 0,
                    n_max = Inf,
                    .name_repair = "unique")
LDL2017
BodyMeasures2017 <- read_xpt("~/Desktop/VUW/2021/Trimester 2/DATA301/Project/Body_Measures.XPT",
                             col_select = NULL,
                             skip = 0,
                             n_max = Inf,
                             .name_repair = "unique")

TotalCholestrol <- read_xpt("~/Desktop/VUW/2021/Trimester 2/DATA301/Project/Total_Cholestrol.XPT",
                            col_select = NULL,
                            skip = 0,
                            n_max = Inf,
                            .name_repair = "unique")

BloodPressure <- read_xpt("~/Desktop/VUW/2021/Trimester 2/DATA301/Project/Blood_Pressure.XPT",
                          col_select = NULL,
                          skip = 0,
                          n_max = Inf,
                          .name_repair = "unique")

demographics <- read_xpt("~/Desktop/VUW/2021/Trimester 2/DATA301/Project/Demo_pop.XPT",
                         col_select = NULL,
                         skip = 0,
                         n_max = Inf,
                         .name_repair = "unique")

cardiohealth <- read_xpt("~/Desktop/VUW/2021/Trimester 2/DATA301/Project/Cardio Health.XPT",
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

joined3$TRYGLICERIDES_BINARY <- ifelse(joined3$TRYGLICERIDES >= 200, 1, 0)
joined3$LDL_BINARY <- ifelse(joined3$LDL >= 160, 1, 0)
joined3$HIGH_INCIDENCE <- with(joined3, ifelse(TRYGLICERIDES_BINARY==1 & LDL_BINARY==1, 1, 0))

table(joined3$HIGH_INCIDENCE)

#remove the sequence number and gender
# drops <- c("SEQN","GENDER","SYSTOLIC")
# joined3 <- joined3[ , !(names(joined3) %in% drops)]

ggpairs(joined3, columns = c("TRYGLICERIDES", "LDL", "HIGHCHANCE"))

ggpairs(joined3, columns = c("T_BINARY", "LDL_BINARY", "HIGHCHANCE"))

ggpairs(joined3, columns = c("GENDER","AGE_YEAR","TRYGLICERIDES", "LDL", "HIGHCHANCE"))

ggpairs(joined3, columns = c("GENDER","AGE_YEAR","T_BINARY", "LDL_BINARY", "HIGHCHANCE"))

Binomi <- glm(HIGHCHANCE~T_BINARY+LDL_BINARY+GENDER+AGE_YEAR, data = joined3, family = "binomial")
summary(Binomi)

eh.model <- glm(BMI~., data = joined3)
summary(eh.model)

eh.model2 <- glm(HIGHCHANCE~., data = joined3)
summary(eh.model2)

data <- joined3 %>%
  select(T_BINARY,LDL_BINARY, HIGHCHANCE)

ggcorrplot(cor(data),
           method = "circle",
           hc.order = TRUE,
           type = "lower")


counts <- table(joined3$T_BINARY, joined3$LDL_BINARY)


library(ggpubr)
ggscatter(joined3, x = "TRYGLICERIDES", y = "LDL", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          alpha=I(0.02)) +
  scale_y_log10() +
  scale_x_log10()


ggplot(joined3, aes(TRYGLICERIDES, LDL),
       cor.coef = TRUE, cor.method = "pearson") +
  geom_point(alpha=0.9, aes(color = HIGHCHANCE)) +
  geom_smooth() +
  scale_y_log10() +
  scale_x_log10()

ggplot(joined3, aes(T_BINARY, LDL_BINARY),
       cor.coef = TRUE, cor.method = "pearson") +
  geom_point(alpha=0.9, aes(color = HIGHCHANCE)) +
  geom_smooth() +
  scale_y_log10() +
  scale_x_log10()



ggscatter(joined3, x = "TRYGLICERIDES", y = "LDL",
   color = "black", shape = 21, size = 3, # Points color, shape and size
   add = "loess",  # Add regressin line
   add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
   conf.int = TRUE, # Add confidence interval
   cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
   cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n")
   )

ggscatter(joined3, x = "TRYGLICERIDES", y = "LDL",
   color = "black", shape = 21, size = 3, # Points color, shape and size
   add = "reg.line",  # Add regressin line
   add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
   conf.int = TRUE, # Add confidence interval
   cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
   cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n")
   )


# TRYGLICERIDES
ggqqplot(joined3$TRYGLICERIDES, ylab = "Tryglicerides")
# LDL
ggqqplot(joined3$LDL, ylab = "LDL")











