
# exam questions used in the past
library(DanielBiostatistics10th)


# Page 32, Exercise 2.3.3
e233 = read.csv(system.file('extdata', 'EXR_C02_S03_03.csv', package = 'DanielBiostatistics10th'))
head(e233)
print_stats(e233$bmi)
print_freqs(e233$bmi, breaks = seq.int(from = 20, to = 55, by = 5), include.lowest = TRUE, right = FALSE)


# Page 83, Exercise 3.5.1 
addmargins(e351 <- matrix(c(744L, 775L - 744L, 21L, 1380L - 21L), ncol = 2L, dimnames = list(
  Symptom = paste0('(', c('+', '-'), ')'),
  Disease = paste0('(', c('+', '-'), ')')
)))
(pv351 = predictiveValues(e351, prevalence = c(1e-4, 1e-3, 1e-2, 1e-1)))
autoplot(pv351, xlim = c(0, .1))



# Page 85, Review Question 3
addProbs(array(c(319L, 130L, 88L, 41L, 738L, 329L, 402L, 95L), dim = c(4L, 2L), dimnames = list(
  MaritalStatus = c('Currently Married', 'Divorced', 'Widowed', 'Unmarried'),
  Ethnicity = c('Hispanic', 'Non-Hispanic'))))


# Page 88, Review Question 9
addProbs(matrix(c(2991L, 112L, 2244L, 115849L), nrow = 2L, dimnames = list(
  'CancerReported(A)' = c('Yes', 'No'),
  'CancerInRegistry(B)' = c('Yes', 'No'))))



# Page 113, Exercise 4.4.4
# (a) Exactly one live insect 
dpois(1L, lambda = .5) # Answer: 30.3%
# (b) No live insects
dpois(0L, lambda = .5) # Answer: 60.6%
# (c) Exactly four live insects 
dpois(4L, lambda = .5) # Answer: .2%
# (d) One or more live insects
ppois(0L, lambda = .5, lower.tail = FALSE) # Answer: 39.4%


# Page 122, Exercises 4.6.11-4.6.12.
# For Z ~ N(0, 1), Find z such that 
# 4.6.11. P(Z<=z) = .0055
qnorm(.0055) # Answer: -2.54
# 4.6.12. P(-2.67<=Z<=z) = .9718 
# Since: pnorm(z) - pnorm(-2.67) = .9718
qnorm(.9718 + pnorm(-2.67)) # Answer: 1.97


# Page 130, Review Question 15
dbinom(x = 7L, size = 10L, prob = .35) # 2.1% (a)
pbinom(q = 5L, size = 10L, prob = .35, lower.tail = FALSE) # 9.5% (b)
dbinom(x = 0L, size = 10L, prob = .35) # 1.3% (c)
pbinom(q = 6L, size = 10L, prob = .35) - pbinom(q = 2L, size = 10L, prob = .35) # 71.2% (d)


# Page 132, Review Question 26
# X~N(mu, sigma); P(mu - k*sigma <= X <= mu + k*sigma) = .754
# <=> P(-k <= Z <= k) = .754; where Z ~ N(0,1)
(Q3ans = qnorm((1-.754)/2, lower.tail = FALSE)) # 1.16 
pnorm(Q3ans) - pnorm(-Q3ans) # verified



# Page 145, Exercise 5.3.7 (a), (b).
# Given mu = 50, sigma = 16 and n = 64, find:
# (a) P(45<=x_bar<=55)
pnorm(q = 55, mean = 50, sd = 16/sqrt(64L)) - pnorm(q = 45, mean = 50, sd = 16/sqrt(64L)) # Answer 98.8%
# (b) P(x_bar > 53)
pnorm(q = 53, mean = 50, sd = 16/sqrt(64L), lower.tail = FALSE) # Answer: 6.7%



# Page 158, Exercise 11
pnorm(25, mean = 23.1, sd = 3.7/sqrt(45L), lower.tail = FALSE) # 0.000286




# Page 171, Exercise 6.2.1
aggregated_z(xbar = 90, sd = 10, n = 49L, conf.level = .9) # 87.65~92.35
aggregated_z(xbar = 90, sd = 10, n = 49L, conf.level = .95) # 87.20~92.80
aggregated_z(xbar = 90, sd = 10, n = 49L, conf.level = .99) # 86.32~93.68



# Page 206, Exercise 20. 
aggregated_t(xbar = c(95, 125), n = c(35L, 35L), xsd = c(25, 30), conf.level = .95)
aggregated_t(xbar = c(95, 125), n = c(35L, 35L), xsd = c(25, 30), conf.level = .95, var.equal = TRUE)


# Page 207, Exercise 24
aggregated_z(xbar = c(358, 483), sd = c(308, 144), n = c(52L, 53L), conf.level = .99)


# Page 207, Exercise 25
aggregated_t(xbar = 435, n = 34L, xsd = 215, conf.level = .95)


# Page 207, Exercise 26
prop_CLT(xbar = c(.293, .173), n = c(220L, 106L), conf.level = .95)


# Page 277, Exercise 7.9.1. 
curve(expr = power_z(x, null.value = 516, sd = 32, n = 16L, alternative = 'greater'), from = 500, to = 550, 
      ylab = 'Power curve for Page 277, Exercise 7.9.1.', xlab = 'Alternative values of \u03bc')



# Page 283, Question 18
# let group 1 be IVF and group 2 be spontaneous conception
aggregated_t(xbar = c(3071, 3172), xsd = c(761, 702), n = c(163L, 321L), alternative = 'less')
# Birth weight from IVF is lower than spontaneous conception (p = .078) at alpha = .1 level.


# Page 283, Question 19
r719 = read.csv(system.file('extdata', 'REV_C07_19.csv', package = 'DanielBiostatistics10th'))
head(r719)
with(r719, t.test(Before, AFter, alternative = 'greater', paired = TRUE))
# Total cholesterol decreased after taking a statin drug (mean of before 245.8, mean of after 166.4, p < .001, paired t-test).




# Page 379, Question 16
r816 = read.csv(system.file('extdata', 'REV_C08_16.csv', package = 'DanielBiostatistics10th'))
head(r816)
anova(aov(Count ~ Sens * Treat, data = r816))
# we see that the interaction term is not significant (p = .078 > .05)
# therefore we choose the model without interaction
anova(aov(Count ~ Sens + Treat, data = r816))
# A two-way ANOVA without interaction indicates that
# 1. we do have a difference between the two levels of `Sens`
# 2. we do have a difference among the three levels of `Treat`



# Page 465, Exercise 17
# (a) Perform the linear regression
# (b) Draw the scatter-plot as well as the regression line.
r917 = read.csv(system.file('extdata', 'REV_C09_17.csv', package = 'DanielBiostatistics10th'))
plot(BOARD ~ AVG, data = r917)
summary(lm1_917 <- lm(BOARD ~ I(AVG - 85), data = r917)) # ideally
summary(lm2_917 <- lm(BOARD ~ AVG, data = r917)) # also acceptable
with(lm1_917, abline(a = coefficients[1L] + coefficients[2L]*(-85), b = coefficients[2L], col = 2L))
abline(reg = lm2_917, col = 3L) # two regression models not too much difference
# using ?car::scatterplotMatrix is also acceptable
car::scatterplotMatrix(~ AVG + BOARD, data = r917, smooth = FALSE, regLine = TRUE)

# Page 465, Question 17
r917 = read.csv(system.file('extdata', 'REV_C09_17.csv', package = 'DanielBiostatistics10th'))
head(r917)
summary(m917 <- lm(BOARD ~ AVG, data = r917))
plot(BOARD ~ AVG, data = r917); abline(reg = m917)

# Page 466, Question 18
r918 = read.csv(system.file('extdata', 'REV_C09_18.csv', package = 'DanielBiostatistics10th'))
head(r918)
summary(lm(ATT ~ AGE, data = r918))
plot(ATT ~ AGE, data = r918)
cor.test(~ ATT + AGE, data = r918)
# Age is correlated with ATT (estimated correlation = .399, p = .026)
summary(lm(SOC ~ AGE, data = r918))
plot(SOC ~ AGE, data = r918)
cor.test(~ SOC + AGE, data = r918)
# No evidence support that Age is correlated with SOC (estimated correlation = -.185, p = .319) 
summary(lm(HYP ~ AGE, data = r918))
plot(HYP ~ AGE, data = r918)
cor.test(~ HYP + AGE, data = r918)
# No evidence support that Age is correlated with HYP (estimated correlation = .010, p = .957) 




# Page 557, Exercise 11.2.2
e1122 = read.csv(system.file('extdata', 'EXR_C11_S02_02.csv', package = 'DanielBiostatistics10th'))
dim(e1122)
e1122 = within(e1122, expr = {
  DISEASE = structure(DISEASE, levels = c('Cholelithiasis', 'Gallbladder Cancer'), class = 'factor')
})
head(e1122)
summary(lm(MDA ~ CYTO * DISEASE, data = e1122)) # interaction not significant.  Remove.
summary(lm_1122 <- lm(MDA ~ CYTO + DISEASE, data = e1122))
car::scatterplot(MDA ~ CYTO | DISEASE, data = e1122, regLine = TRUE, smooth = FALSE,
                 xlab = 'Cytochrome P-450 (nmol/mg)', ylab = 'Melonaldehyde (\u03bcg/mg)',
                 main = 'Page 557, Exercise 11.2.2')





# Page 581, Exercise 11.4.2
# (a) Perform the logistic regression
# (b) Draw the logistic regression line
e1142 = read.csv(system.file('extdata', 'EXR_C11_S04_02.csv', package = 'DanielBiostatistics10th'))
dim(e1142)
e1142 = within(e1142, expr = {
  PART = as.logical(PART)
})
summary(mod_1142 <- glm(PART ~ INDEX, family = binomial, data = e1142))

index_ord <- order(e1142$INDEX)
class(pred_1142 <- predict(mod_1142, newdata = e1142, se.fit = TRUE)) # this is prediction interval
head(exp_pred_1142 <- plogis(with(pred_1142, fit + qnorm(.975) * se.fit %*% t(c(-1, 0, 1)))))
plot(PART ~ INDEX, data = e1142, main = 'Page 581, Exercise 11.4.2')
matlines(x = e1142$INDEX[index_ord], y = exp_pred_1142[index_ord, ], col = c(2, 1, 2), lty = c(2, 1, 2))



# Page 585, Question 15
r1115 = read.csv(system.file('extdata', 'REV_C11_15.csv', package = 'DanielBiostatistics10th'))
summary(m1115 <- lm(CARDIAC ~ VO2 * GROUP, dat = r1115))
summary(m1115 <- lm(CARDIAC ~ VO2 + GROUP, dat = r1115))


# Page 589, Question 23
r1123 = read.csv(system.file('extdata', 'REV_C11_23.csv', package = 'DanielBiostatistics10th'))
scatterplot(V ~ W | GROUP, data = r1123, regLine = TRUE, smooth = FALSE)
summary(lm(V ~ W * GROUP, data = r1123))
# both figure and the regression model show that the interaction between W and GROUP is significant
# In A(dult) group, which is the reference group, for every 1kg increase in infants weight, 
# the steady-state apparent volume of distribution (V) will have an increase of .137 liters (coefficient of 'W', p = .010)
# In C(hildren) group, for every 1kg increase in W, V will increase for (.137+.223) liters (coefficient of 'W:GROUPC').
# The slope of C group is significantly higher than the slope of A group (p = .002).
# In I(nfant) group, for every 1kg increase in W, V will increase for (.137+.226) liters (coefficient of 'W:GROUPI').
# We do not detect a significantly difference in the slope of I group vs. the slope of A group (p = .311).


# Page 659, Review Question 17
r1217 = array(c(72L, 54L, 16L, 8L, 230L, 192L, 63L, 15L), dim = c(4L, 2L), dimnames = list(
  BloodGroup = c('O', 'A', 'B', 'AB'), 
  Antigen = c('Carriers', 'Noncarriers')))
addProbs(r1217)
chisq.test(r1217)
# Exam Answer: 
# No significant relationship between BloodGroup and Antigen detected (p = .49),
# which is also illustrated by the similarities of conditional probabilities (conditional on BloodGroup and/or Antigen) 



# Page 774, Review Question 11
library(survival)
library(ggfortify)
r1411 = read.csv(system.file('extdata', 'REV_C14_11.csv', package = 'DanielBiostatistics10th'))
head(r1411)
subset(r1411, GRADE == '0')$OUTCOME # no death for `Grade == 0`
m1411 = coxph(Surv(DAYS, OUTCOME) ~ factor(GRADE), data = subset(r1411, GRADE != '0'))
summary(m1411)
autoplot(survfit(Surv(DAYS, OUTCOME) ~ factor(GRADE), data = subset(r1411, GRADE != '0')))



# Page 775, Review Question 12
r1412 = read.csv(system.file('extdata', 'REV_C14_12.csv', package = 'DanielBiostatistics10th'))
head(r1412)
m1412 = coxph(Surv(MONTHS, STATUS == 'D') ~ factor(NUMBER), data = r1412)
summary(m1412)
autoplot(survfit(Surv(MONTHS, STATUS == 'D') ~ factor(NUMBER), data = r1412))


