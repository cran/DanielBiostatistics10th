library(DanielBiostatistics10th)
library(car)
library(DescTools)

# Page 540, Example 11.1.1
head(EXA_C11_S01_01)
head(log(EXA_C11_S01_01$conc, base = 10))
head(EXA_C11_S01_01$logConc)

# Page 542, Example 11.1.2
head(EXA_C11_S01_02)
cor.test(~ sbp + weight, data = EXA_C11_S01_02)
cor.test(~ sbp + bmi, data = EXA_C11_S01_02) 

# Page 545, Example 11.2.1
head(EXA_C11_S02_01)
d1121 = within(EXA_C11_S02_01, expr = {
  SMOKE = as.logical(SMOKE)
})
xlab1121 = 'Length of gestation (weeks)'; ylab1121 = 'Birth weight (grams)'
car::scatterplot(GRAMS ~ WEEKS | SMOKE, data = d1121, regLine = FALSE, smooth = FALSE,
                 xlab = xlab1121, ylab = ylab1121, main = 'Page 547, Figure 11.2.1')
# Page 547, Figure 11.2.2: main model (without interaction)
summary(m1121_main <- lm(GRAMS ~ WEEKS + SMOKE, data = d1121))
confint(m1121_main)
car::scatterplot(GRAMS ~ WEEKS | SMOKE, data = d1121, regLine = FALSE, smooth = FALSE,
                 xlab = xlab1121, ylab = ylab1121, main = 'Page 548, Figure 11.2.3')
(cf_main = m1121_main$coefficients)
abline(a = cf_main[1L], b = cf_main[2L], col = 'blue') # regression line for non-smoking mothers
abline(a = cf_main[1L] + cf_main[3L], b = cf_main[2L], col = 'magenta') 

# Page 551, Example 11.2.3
d1123 = within(EXA_C11_S02_03, expr = {
  METHOD = factor(METHOD, levels = c('C', 'A', 'B')) # textbook designated 'C' as reference level
})
summary(mod_1123 <- lm(EFFECT ~ AGE * METHOD, data = d1123)) # Page 555, Figure 11.2.5
confint(mod_1123)
car::scatterplot(EFFECT ~ AGE | METHOD, data = d1123, smooth = FALSE,
                 xlab = 'Age', ylab = 'Treatment effectiveness', main = 'Page 555, Figure 11.2.6')

# (optional) Page 561, Example 11.3.1
head(EXA_C11_S03_01)
names(EXA_C11_S03_01) = c('JOBPER', 'ASRV', 'ENTH', 'AMB', 'COMM', 'PROB', 'INIT')
summary(mod_1131_raw <- lm(JOBPER ~ ASRV + ENTH + AMB + COMM + PROB + INIT, data = EXA_C11_S03_01))
# summary(mod_1131 <- MASS::stepAIC(mod_1131_raw, direction = 'backward'))
# the stepwise selection criterion used in MINITAB is not necessarily AIC

# Page 572, Example 11.4.1
addmargins(d1141 <- array(c(92L, 21L, 15L, 20L), dim = c(2L, 2L), dimnames = list(
  OCAD = c('Present', 'Absent'), Sex = c('Male', 'Female')))) # Page 572, Table 11.4.2
(d1141a = within(as.data.frame(as.table(d1141)), expr = {
  OCAD = (OCAD == 'Present')
  Sex = factor(Sex, levels = c('Female', 'Male'))
}))
(m1141 = glm(OCAD ~ Sex, family = binomial(link = 'logit'), weights = Freq, data = d1141a))
summary(m1141) # Page 573, Figure 11.4.1
exp(m1141$coefficients[2L]) # exp(beta_M)
exp(confint(m1141)) # confidence interval of exp(beta)
predict(m1141, newdata = data.frame(Sex = setNames(nm = c('Male', 'Female'))), type = 'response')

# Page 573, Example 11.4.2
head(EXA_C11_S04_02)
summary(mod_1142 <- glm(ATT ~ AGE, family = binomial, data = EXA_C11_S04_02)) 
# .. Page 575, Figure 11.4.2
exp(mod_1142$coefficients[2L])
exp(confint(mod_1142))
car::Anova(mod_1142) # Optional
autoplot(predict_glm_binomial(mod_1142, newx = c(50, 65, 80)), title = 'Page 576, Figure 11.4.3')

# (optional) Page 576, Example 11.4.3
head(REV_C11_24)
summary(glm(ONSET ~ HIAA + TRYPT, family = binomial(link = 'logit'), data = REV_C11_24)) 
# Page 577, Figure 11.4.4
# Predictor TRYPT should be removed from model due to p-value \approx 1 
summary(glm(ONSET ~ HIAA, family = binomial(link = 'logit'), data = REV_C11_24)) 

# (optional) Page 578, Example 11.4.4
DescTools::PseudoR2(mod_1142, which = 'CoxSnell')
DescTools::PseudoR2(mod_1142, which = 'Nagelkerke')

# (optional) Page 579, Example 11.4.5 (same as Example 11.4.4)
