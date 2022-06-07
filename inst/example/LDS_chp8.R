library(DanielBiostatistics10th)
# To run a line of code, use shortcut
# Command + Enter: Mac and RStudio Cloud
# Control + Enter: Windows, Mac and RStudio Cloud
# To clear the console
# Control + L: Mac and RStudio Cloud




# Large Data Sets from Chapter 8

library(reshape2)
library(multcomp)

# MEDSCORES (Optional: follow Example 8.4.2)
head(MedScore <- read.csv(system.file('extdata', 'LDS_C08_MEDSCORES.csv', 
                                      package = 'DanielBiostatistics10th')))
dim(MedScore)
MedScore_a = within(MedScore, expr = {
  day = factor(day)
  test = factor(test)
  ID = factor(ID)
})
(aov_MEDSCORES = aov(score ~ day * test + ID, data = MedScore_a))
anova(aov_MEDSCORES)





# LSADATA
head(LSA <- read.csv(system.file('extdata', 'LDS_C08_LSADATA.csv', 
                                 package = 'DanielBiostatistics10th'))[-1L])
# first column does not mean anything at all!  read the question carefully. 
dim(LSA)
names(LSA) = c('Normal', 'Benign', 'Primary', 'Metastatic')
head(LSAa <- melt(LSA, id.vars = NULL, variable.name = 'Group', value.name = 'LSA'))
boxplot(LSA ~ Group, data = LSAa)
(aov_LSA = aov(LSA ~ Group, data = LSAa))
anova(aov_LSA)
summary(tukey_LSA <- glht(aov_LSA, linfct = mcp(Group = 'Tukey')))
confint(tukey_LSA)




# SACEDATA
head(SACEDATA <- read.csv(system.file('extdata', 'LDS_C08_SACEDATA.csv', 
                                      package = 'DanielBiostatistics10th'))[-1L])
dim(SACEDATA)
names(SACEDATA) = c('Never', 'Active', 'Stable', 'Recovered')
head(SACE <- melt(SACEDATA, id.vars = NULL, variable.name = 'Group', value.name = 'SACE'))
boxplot(SACE ~ Group, data = SACE)
(aov_SACE = aov(SACE ~ Group, data = SACE))
anova(aov_SACE) 
summary(tukey_SACE <- glht(aov_SACE, linfct = mcp(Group = 'Tukey')))
confint(tukey_SACE)



# Page 408, Question 4
head(CFA <- read.csv(system.file('extdata', 'LDS_C08_CSFDATA.csv', 
                                 package = 'DanielBiostatistics10th'))[-1L])
# 1st column 'OBSERV' is both incorrect and meaningless
dim(CFA)
head(CFA_a <- melt(CFA, id.vars = NULL, variable.name = 'Disease', value.name = 'CSF'))
dim(CFA_a)
boxplot(CSF ~ Disease, data = CFA_a)
anova(aov_CSF <- aov(CSF ~ Disease, data = CFA_a)) 
# ANOVA shows that the five group means are not all same (p < .001)
summary(glht(aov_CSF, linfct = mcp(Disease = 'Tukey')))
# Tukey HSD of multiple comparisons shows that all pairwise differences are significant (p < .001) except for Normal vs. C (p = .435)







# RBCDATA
head(RBCDATA <- read.csv(system.file('extdata', 'LDS_C08_RBCDATA.csv', 
                                     package = 'DanielBiostatistics10th'))[-1L])
# 1st column 'OBSERV' is both incorrect and meaningless
dim(RBCDATA_raw)
head(RBCDATA <- melt(RBCDATA, id.vars = NULL, variable.name = 'Diet', value.name = 'RBC'))
boxplot(RBC ~ Diet, data = RBCDATA)
anova(aov_RBC <- aov(RBC ~ Diet, data = RBCDATA)) 
summary(glht(aov_RBC, linfct = mcp(Diet = 'Tukey')))






# SERUMCHO
head(SERUMCHO_raw <- read.csv(system.file('extdata', 'LDS_C08_SERUMCHO.csv', 
                                          package = 'DanielBiostatistics10th')))
dim(SERUMCHO_raw)
SERUMCHO_0 = within(SERUMCHO_raw, expr = {
  SUBJ = factor(SUBJ)
})
head(SERUMCHO <- melt(SERUMCHO_0, id.var = 'SUBJ', variable.name = 'Diet', value.name = 'SC'))
# anova(aov(SC ~ SUBJ * Diet, data = SERUMCHO)) # wrong
(aov_SERUMCHO = aov(SC ~ Diet + SUBJ, data = SERUMCHO)) # correct; 'SUBJ' as block factor
anova(aov_SERUMCHO)




