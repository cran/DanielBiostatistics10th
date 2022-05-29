library(DanielBiostatistics10th)
# To run a line of code, use shortcut
# Command + Enter: Mac and RStudio Cloud
# Control + Enter: Windows, Mac and RStudio Cloud
# To clear the console
# Control + L: Mac and RStudio Cloud



library(car)




LDS_C10_LTEXER




LDS_C10_RESPDIS








# Page 536, Question 3 (Large Data Set ‘RISKFACT’)
RISKFACT0 = read.csv(system.file('extdata', 'LDS_C10_RISKFACT_corrected.csv', 
                                package = 'DanielBiostatistics10th'))
head(RISKFACT0)
car::scatterplotMatrix(~ O2 + SBP + TChol + HDLChol + TriG, data = RISKFACT0, smooth = FALSE, regLine = FALSE)
# There are obvious typos in SBP, the corresponding records should be removed
dim(RISKFACT <- RISKFACT0[-c(which.max(RISKFACT0$SBP), which.min(RISKFACT0$SBP)), , drop = FALSE])
car::scatterplotMatrix(~ O2 + SBP + TChol + HDLChol + TriG, data = RISKFACT, smooth = FALSE, regLine = FALSE)

summary(mod_RISKFACT <- lm(O2 ~ SBP + TChol + HDLChol + TriG, data = RISKFACT))
# 84% of total variance is explained by the multivariable linear regression model
# for every 1 unit increase in SBP (while having all other predictors fixed), O2 increase by .302 (p < .001)
# for every 1 unit increase in TChol (while having all other predictors fixed), O2 decrease by .168 (p < .001)
# for every 1 unit increase in HDLChol (while having all other predictors fixed), O2 increase by .511 (p < .001)
# for every 1 unit increase in TriG (while having all other predictors fixed), O2 increase by .068 (p < .001)







LDS_C10_STERLENGTH




LDS_C11_AQUATICS