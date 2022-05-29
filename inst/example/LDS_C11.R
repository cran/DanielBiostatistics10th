library(DanielBiostatistics10th)
# To run a line of code, use shortcut
# Command + Enter: Mac and RStudio Cloud
# Control + Enter: Windows, Mac and RStudio Cloud
# To clear the console
# Control + L: Mac and RStudio Cloud


library(car)

LDS_C11_TEACHERS






# Page 597, Question 3 (Large Data Set 'WGTLOSS')
WGTLOSS = read.csv(system.file('extdata', 'LDS_C11_WGTLOSS_corrected.csv', 
                               package = 'DanielBiostatistics10th'))
head(WGTLOSS)
table(WGTLOSS$Group) # typos in the textbook (in terms of sample size)

car::scatterplot(Protein ~ IdealBW | Group, data = WGTLOSS, smooth = FALSE)
summary(lm(Protein ~ IdealBW * Group, data = WGTLOSS))
# both figure and regression model shows that the interaction between IdealBW and Group is not significant, thus the interaction term should be remove from the model.

summary(mod_WGTLOSS <- lm(Protein ~ IdealBW + Group, data = WGTLOSS))
# For both CAN and HEAL group, every one unit increase in IdealBW corresponds to .09 decrease in Protein
# Patient from CAN group has .32 lower Protein, compared to a patient from HEAL group with the same IdealBW
