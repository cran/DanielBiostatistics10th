
library(car)

LDS_C11_TEACHERS






# Page 597, Question 3 (Large Data Set 'WGTLOSS')
head(WGTLOSS_raw <- read.csv('data/LDS_C11_WGTLOSS.csv'))
dim(WGTLOSS_raw)

WGTLOSS = within(WGTLOSS_raw, expr = {
  # create a dichotomos (or categorical) variable indicating the HEAL vs. CAN group
  Group = structure(startsWith(as.character(SUBJNO), prefix = 'CAN') + 1L, levels = c('HEAL', 'CAN'), class = 'factor')
  SUBJNO = NULL
})
table(WGTLOSS$Group) # typos in the textbook (in terms of sample size)
head(WGTLOSS)
names(WGTLOSS)[1:2] = c('IdealBW', 'Protein')

scatterplot(Protein ~ IdealBW | Group, data = WGTLOSS, regLine = TRUE, smooth = FALSE)
summary(lm(Protein ~ IdealBW * Group, data = WGTLOSS))
# both figure and regression model shows that the interaction between IdealBW and Group is not significant, thus the interaction term should be remove from the model.

summary(mod_WGTLOSS <- lm(Protein ~ IdealBW + Group, data = WGTLOSS))
# For both CAN and HEAL group, every one unit increase in IdealBW corresponds to .09 decrease in Protein
# Patient from CAN group has .32 lower Protein, compared to a patient from HEAL group with the same IdealBW
