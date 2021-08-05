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
myvars <- c("SEQN","RIAGENDR", "RIDAGEYR","WTMEC2YR")
demographics <- demographics[myvars]


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
LDL2017
#LDL2017$LDL <- mice(LDL2017$LDL, method = "norm.predict", m = 1)

#Number of missing values in LDL and triglycerides
frequency <- colSums(is.na(LDL2017))
proportion <- (colSums(is.na(LDL2017)))/8704
pander(cbind(frequency, proportion))



#cardio health
carvar <- c("SEQN", "CDQ001", "CDQ010")
cardiohealth <- cardiohealth[carvar]

#Join Datasets
demo_blood <- full_join(BloodPressure,demographics, by="SEQN")
joined <- full_join(BodyMeasures2017,demo_blood, by="SEQN")
joinedLDL <- full_join(joined, LDL2017, by = "SEQN")
joined2 <- full_join(joinedLDL, cardiohealth, by = "SEQN")
joined2

#number
frequency <- colSums(is.na(joined2))
proportion <- (colSums(is.na(joined2)))/8704
pander(cbind(frequency, proportion))

summary(joined2)

# Imputation
#joined3 <- mice::complete(joined3, "long")
joined2 <- simple.impute.data.frame(joined2)
joined2

#check again if there are missing values
frequency <- colSums(is.na(joined2))
proportion <- (colSums(is.na(joined2)))/8704
pander(cbind(frequency, proportion))

#remove the sequence number

pairs.panels(select(joined2,LDL,BMI,WEIGHT,SYSTOLIC),
             method = "pearson",
             hist.col = "#00AFBB",
             density = TRUE,
             ellipses = TRUE)

plot(select(joined2,LDL, BMI,HEIGHT,WEIGHT))

ggcorrplot(cor(joined2[,-1]),
           method = "circle",
           hc.order = TRUE,
           type = "lower")



#eh.model <- glm(BMI~BPXSY3+RIAGENDR+RIDAGEYR+WTMEC2YR+LBDHDD, data = joined3)
eh.model <- glm(BMI~., data = joined2)
summary(eh.model)











