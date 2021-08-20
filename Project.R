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

joined3$TRYGLICERIDES_BINARY <- ifelse(joined3$TRYGLICERIDES >= 200, 1, 0)
joined3$LDL_BINARY <- ifelse(joined3$LDL >= 160, 1, 0)
joined3$HIGH_INCIDENCE <- with(joined3, ifelse(TRYGLICERIDES_BINARY==1 & LDL_BINARY==1, 1, 0))

table(joined3$T_BINARY)
table(joined3$LDL_BINARY)
table(joined3$HIGHCHANCE)

joined3$GENDER <- ifelse(joined3$GENDER==1,"MALE","FEMALE")

library(stats)
library(gridExtra)

library(vcd)

joined3 %>%
  ggplot(aes(x=AGE_YEAR,y=LDL,color=GENDER))+
  geom_point(alpha=0.001)+xlab("Age") +geom_smooth()+
  ylab("LDL")+
  guides(fill = guide_legend(title = "Gender"))

ggpairs(joined3, columns = c("TRIGLYCERIDES", "LDL", "HIGHCHANCE"))


ggpairs(joined3, columns = c("T_BINARY", "LDL_BINARY", "HIGHCHANCE"),
        diag_panel = pairs_diagonal_mosaic(offset_varnames=-2.5),
        upper_panel_args = list(shade=TRUE),
        lower_panel_args = list(shade=TRUE))

p <- joined3[,c("T_BINARY", "LDL_BINARY", "HIGHCHANCE")]
pairs(table(p),
      diag_panel = pairs_diagonal_mosaic(offset_varnames=-3),
      upper_panel_args = list(shade=TRUE),
      lower_panel_args = list(shade=TRUE))

ggplot(joined3, 
       aes(x = GENDER, 
           y = HIGHCHANCE)) +
  geom_violin(fill = "cornflowerblue") +
  geom_boxplot(width = .2, 
               fill = "orange",
               outlier.color = "orange",
               outlier.size = 2) + 
  labs(title = "Salary distribution by rank")

# glm is used to fit generalized linear models, specified by giving a symbolic description of the linear predictor and a description of the error distribution.
eh.model <- glm(HIGHCHANCE~., data = joined3)
summary(eh.model)

high <- glm(HIGHCHANCE~BMI+WEIGHT+HEIGHT+TRIGLYCERIDES+CDQ010+T_BINARY+LDL_BINARY,data = joined3)
summary(high)

eh.model2 <- glm(HIGHCHANCE~T_BINARY + LDL_BINARY + TRIGLYCERIDES + LDL + AGE_YEAR + BMI + HEIGHT + WEIGHT, data = joined3)
summary(eh.model2)



# graphs
corre <- joined3[,c("BMI","WEIGHT","HEIGHT","TRIGLYCERIDES")]
pairs(corre, pch = 19)

pairs.panels(corre, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
             )


counts <- table(joined3$T_BINARY, joined3$LDL_BINARY)


library(ggpubr)

#Correlation between Triglycerides vs LDL, together with LDL
ggscatter(joined3, x = "TRIGLYCERIDES", y = "LDL", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          alpha=I(0.02)) +
  scale_y_log10() +
  scale_x_log10()

# LDL vs AGE_YEAR
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
   color = "black", shape = 21, size = 3, # Points color, shape and size
   add = "loess",  # Add regressin line
   add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
   conf.int = TRUE, # Add confidence interval
   cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
   cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n")
   )

ggscatter(joined3, x = "TRIGLYCERIDES", y = "LDL",
   color = "black", shape = 21, size = 3, # Points color, shape and size
   add = "reg.line",  # Add regressin line
   add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
   conf.int = TRUE, # Add confidence interval
   cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
   cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n")
   )


# TRYGLICERIDES
ggqqplot(joined3$TRIGLYCERIDES, ylab = "Triglycerides")
ggqqplot(joined3$LDL, ylab = "LDL")
ggqqplot(joined3$BMI, ylab = "BMI")
ggqqplot(joined3$SYSTOLIC, ylab = "Systolic Blood Pressure")
