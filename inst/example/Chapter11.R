library(DanielBiostatistics10th)
# To run a line of code, use shortcut
# Command + Enter: Mac and RStudio Cloud
# Control + Enter: Windows, Mac and RStudio Cloud
# To clear the console
# Control + L: Mac and RStudio Cloud




# Page 540, Example 11.1.1
# data not provided by the publisher


# Page 542, Example 11.1.2
# data not provided by the publisher


# Page 545, Example 11.2.1
head(d1121_raw <- read.csv('data/EXA_C11_S02_01.csv'))
d1121 = within(d1121_raw, expr = {
  SMOKE = as.logical(SMOKE)
})
head(d1121_raw$SMOKE) # 0L/1L, integer
head(d1121$SMOKE) # TRUE/FALSE, binary

car::scatterplot(GRAMS ~ WEEKS | SMOKE, data = d1121, regLine = FALSE, smooth = FALSE,
                 xlab = 'Length of gestation (weeks)', ylab = 'Birth weight (grams)',
                 main = 'Page 547, Figure 11.2.1')

# Page 547, Figure 11.2.2: main model (without interaction)
summary(mod_1121_main <- lm(GRAMS ~ WEEKS + SMOKE, data = d1121))
confint(mod_1121_main)
(cf_main = mod_1121_main$coefficients)
car::scatterplot(GRAMS ~ WEEKS | SMOKE, data = d1121, regLine = FALSE, smooth = FALSE,
                 xlab = 'Length of gestation (weeks)', ylab = 'Birth weight (grams)',
                 main = 'Page 548, Figure 11.2.3')
abline(a = cf_main[1L], b = cf_main[2L], col = 'blue') # regression line for non-smoking mothers
abline(a = cf_main[1L] + cf_main[3L], b = cf_main[2L], col = 'magenta') # regression line for smoking mothers



# model with interaction (over fit!)
summary(mod_1121_ita <- lm(GRAMS ~ WEEKS * SMOKE, data = d1121)) # interaction term not significant
car::scatterplot(GRAMS ~ WEEKS | SMOKE, data = d1121, regLine = FALSE, smooth = FALSE,
                 xlab = 'Length of gestation (weeks)', ylab = 'Birth weight (grams)',
                 main = 'Birthweight Example with Interaction')
(cf_ita = mod_1121_ita$coefficients)
# let the four elements of cf_ita be beta_0, beta_1, beta_2, beta_3
# add regression lines manually
abline(a = cf_ita[1L], b = cf_ita[2L], col = 'blue') # Blue line for non-smokers: y = beta_0 + beta_1 * Week
abline(a = cf_ita[1L] + cf_ita[3L], b = cf_ita[2L] + cf_ita[4L], col = 'magenta')
# Magenta line for smokers: y = (beta_0 + beta_2) + (beta_1 + beta_3) * Week
# end of add regression lines manually
# equivalent to 
car::scatterplot(GRAMS ~ WEEKS | SMOKE, data = d1121, regLine = TRUE, smooth = FALSE,
                 xlab = 'Length of gestation (weeks)', ylab = 'Birth weight (grams)',
                 main = 'Birthweight Example with Interaction (R code ver.2)')
# see ?car:::scatterplot.default

if (FALSE) { # how do I locate the colours?
  ?car::scatterplot
  car::carPalette()[-1]
}





# Page 551, Example 11.2.3
head(datA_1123_raw <- read.csv('data/EXA_C11_S02_03.csv'))
datA_1123 = within(datA_1123_raw, expr = {
  METHOD = factor(METHOD, levels = c('C', 'A', 'B')) # textbook designated 'C' as reference level
})
car::scatterplot(EFFECT ~ AGE | METHOD, data = datA_1123, regLine = TRUE, smooth = FALSE,
                 xlab = 'Age', ylab = 'Treatment effectiveness',
                 main = 'Page 555, Figure 11.2.6')

summary(mod_1123 <- lm(EFFECT ~ AGE * METHOD, data = datA_1123)) # Page 555, Figure 11.2.5
confint(mod_1123)
(cf_1123 = mod_1123$coefficients)

car::scatterplot(EFFECT ~ AGE | METHOD, data = datA_1123, regLine = FALSE, smooth = FALSE,
                 xlab = 'Age', ylab = 'Treatment effectiveness',
                 main = 'Page 555, Figure 11.2.6 (R code ver.2)')
abline(a = cf_1123[1L], b = cf_1123[2L], col = 'blue')
abline(a = cf_1123[1L] + cf_1123[3L], b = cf_1123[2L] + cf_1123[5L], col = 'magenta')
abline(a = cf_1123[1L] + cf_1123[4L], b = cf_1123[2L] + cf_1123[6L], col = 'cyan')



# (optional) Page 561, Example 11.3.1
head(datA_1131 <- read.csv('data/EXA_C11_S03_01.csv'))
names(datA_1131) = c('JOBPER', 'ASRV', 'ENTH', 'AMB', 'COMM', 'PROB', 'INIT')
head(datA_1131)
summary(mod_1131_raw <- lm(JOBPER ~ ASRV + ENTH + AMB + COMM + PROB + INIT, data = datA_1131))
summary(mod_1131 <- MASS::stepAIC(mod_1131_raw, direction = 'backward'))
# the stepwise selection criterion used in MINITAB is not necessarily AIC  



# Page 572, Example 11.4.1
if (FALSE) {
  qlogis(.5) # probability (p = 0.5) -> logit (log(odd) = 0)
  plogis(qlogis(.5)) # logit -> probability
}

(datA_1141 = array(c(92L, 21L, 15L, 20L), dim = c(2L, 2L),
                   dimnames = list(OCAD = c('Present', 'Absent'), Gender = c('Male', 'Female'))))
addProbs(datA_1141)

(datA_1141_v2 = within(as.data.frame(as.table(datA_1141)), expr = {
  OCAD = (OCAD == 'Present')
  Gender = factor(Gender, levels = c('Female', 'Male'))
}))

summary(mod_1141 <- glm(OCAD ~ Gender, family = binomial(link = 'logit'), weights = Freq, data = datA_1141_v2)) # Page 573, Figure 11.4.1
(ci_1141 = confint(mod_1141)) # confidence intervals on beta (i.e. log-OR)
exp(mod_1141$coefficients[2L]) # exp(beta_M)
exp(ci_1141) # confidence interval of beta's

# Quiz in the slides
(newd_1141 = data.frame(Gender = c('Male', 'Female')))
rownames(newd_1141) = c('A new male patient', 'A new female patient')
if (FALSE) {
  ?`rownames<-` # https://en.wikipedia.org/wiki/Syntactic_sugar
  ?`row.names<-.data.frame`
  ?`.rowNamesDF<-`
}
(fit_1141 = predict(mod_1141, newdata = newd_1141)) # logit(Conditional Prob.)
plogis(fit_1141) # turning odds into probabilities




# Page 573, Example 11.4.2
head(datA_1142_raw <- read.csv('data/EXA_C11_S04_02.csv'))
datA_1142 = within(datA_1142_raw, expr = {
  ATT = as.logical(ATT)
})
summary(mod_1142 <- glm(ATT ~ AGE, family = binomial, data = datA_1142)) # Page 575, Figure 11.4.2
(ci_1142 = confint(mod_1142))
exp(mod_1142$coefficients[2L])
exp(ci_1142)
car::Anova(mod_1142) # Optional


class(pred_1142 <- predict(mod_1142, newdata = datA_1142, se.fit = TRUE)) # this is the prediction interval
names(pred_1142)
head(exp_pred_1142 <- plogis(with(pred_1142, fit + qnorm(.975) * se.fit %*% t(c(-1, 0, 1)))))
plot(ATT ~ AGE, data = datA_1142, main = 'Page 576, Figure 11.4.3')
age_ord <- order(datA_1142$AGE)
matlines(x = datA_1142$AGE[age_ord], y = exp_pred_1142[age_ord, , drop = FALSE], col = c(2, 1, 2), lty = c(2, 1, 2))




newd_1142 = data.frame(AGE = c(50, 65, 80))
rownames(newd_1142) = c('A new 50yr patient', 'A new 65yr patient', 'A new 80yr patient')
fit_1142 = predict(mod_1142, newdata = newd_1142)
plogis(fit_1142)







# (optional) Page 576, Example 11.4.3
head(datA_1143 <- read.csv('data/REV_C11_24.csv'))
dim(datA_1143)
summary(glm(ONSET ~ HIAA + TRYPT, family = binomial(link = 'logit'), data = datA_1143)) # Page 577, Figure 11.4.4
# Predictor TRYPT should be removed from model due to p-value \approx 1 
summary(mod_1143 <- glm(ONSET ~ HIAA, family = binomial(link = 'logit'), data = datA_1143))
1 - exp(mod_1143$coefficients) # For every one unit increase in HIAA, the odd of ONSET decrease by 1.33% (p = .014)





# (optional) Page 578, Example 11.4.4
library(DescTools)
PseudoR2(mod_1142, which = 'CoxSnell')
PseudoR2(mod_1142, which = 'Nagelkerke')


# (optional) Page 579, Example 11.4.5 (same as Example 11.4.4)

