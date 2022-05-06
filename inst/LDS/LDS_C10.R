
library(car)




LDS_C10_LTEXER




LDS_C10_RESPDIS








# Page 536, Question 3 (Large Data Set ‘RISKFACT’)
head(RISKFACT_raw <- read.csv('data/LDS_C10_RISKFACT.csv'))
dim(RISKFACT_raw)
names(RISKFACT_raw)[2:6] = c('O2', 'SBP', 'TChol', 'HDLChol', 'TriG')

scatterplotMatrix(~ O2 + SBP + TChol + HDLChol + TriG, data = RISKFACT_raw, smooth = FALSE, regLine = FALSE)
# There are obvious typos in SBP, the corresponding records should be removed
dim(RISKFACT <- RISKFACT_raw[-c(which.max(RISKFACT_raw$SBP), which.min(RISKFACT_raw$SBP)), , drop = FALSE])
scatterplotMatrix(~ O2 + SBP + TChol + HDLChol + TriG, data = RISKFACT, smooth = FALSE, regLine = FALSE)

summary(mod_RISKFACT <- lm(O2 ~ SBP + TChol + HDLChol + TriG, data = RISKFACT))
# 84% of total variance is explained by the multivariable linear regression model
# for every 1 unit increase in SBP (while having all other predictors fixed), O2 increase by .302 (p < .001)
# for every 1 unit increase in TChol (while having all other predictors fixed), O2 decrease by .168 (p < .001)
# for every 1 unit increase in HDLChol (while having all other predictors fixed), O2 increase by .511 (p < .001)
# for every 1 unit increase in TriG (while having all other predictors fixed), O2 increase by .068 (p < .001)







LDS_C10_STERLENGTH




LDS_C11_AQUATICS