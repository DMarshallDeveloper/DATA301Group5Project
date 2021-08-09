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









LDL2017$LBXTR <- ifelse(LDL2017$LBXTR >= 200, 1, 0)
LDL2017$LBDLDL <- ifelse(LDL2017$LBDLDL >= 160, 1, 0)

table(LDL2017$LBXTR)
table(LDL2017$LBDLDL)

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

#check again if there are missing values
frequency <- colSums(is.na(joined3))
proportion <- (colSums(is.na(joined3)))/9254
pander(cbind(frequency, proportion))

#remove the sequence number and gender
drops <- c("SEQN","GENDER","SYSTOLIC")
joined3 <- joined3[ , !(names(joined3) %in% drops)]

pairs.panels(joined3,
             method = "pearson",
             hist.col = "#00AFBB",
             density = TRUE,
             ellipses = TRUE)


eh.model <- glm(BMI~., data = joined3)
summary(eh.model)











