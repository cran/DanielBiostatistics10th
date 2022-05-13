library(DanielBiostatistics10th)
# To run a line of code, use shortcut
# Command + Enter: Mac and RStudio Cloud
# Control + Enter: Windows, Mac and RStudio Cloud
# To clear the console
# Control + L: Mac and RStudio Cloud




# LDS_C09_CALCIUM




# Page 485, Question 1 (Large Data Set ‘CEREBRAL’)
head(CEREBRAL <- read.csv('data/LDS_C09_CEREBRAL.csv'))
dim(CEREBRAL)
names(CEREBRAL)[2:3] = c('Pressure', 'Glycerol')
plot(Pressure ~ Glycerol, data = CEREBRAL)
cor.test(~ Pressure + Glycerol, data = CEREBRAL)
# Negative Pearson correlation -.845 (p < .001) between Pressure and Glycerol; i.e. the higher the the glycerol concentration in the blood, the lower the pressure.

summary(mod_CEREBRAL <- lm(Pressure ~ Glycerol, data = CEREBRAL))
# for every one unit increase in glycerol concentration, pressure decrease -0.084 (p < .001)
anova(mod_CEREBRAL)
# 71.4% of the variation of Pressure is explained by Glycerol (from both the 'Multiple R-squared' in regression AND 'Sum Sq' of ANOVA: in 177.530 / (177.530 + 70.983))  

plot(Pressure ~ Glycerol, data = CEREBRAL)
abline(reg = mod_CEREBRAL)

# optional: the regression is better performed around Glycerol \approx 15
summary(lm(Pressure ~ I(Glycerol - 15), data = CEREBRAL))





# LDS_C09_HYPERTEN




