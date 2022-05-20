library(DanielBiostatistics10th)
# To run a line of code, use shortcut
# Command + Enter: Mac and RStudio Cloud
# Control + Enter: Windows, Mac and RStudio Cloud
# To clear the console
# Control + L: Mac and RStudio Cloud



# Page 417, Example 9.3.1
d931 = read.csv(system.file('extdata', 'EXA_C09_S03_01.csv', package = 'DanielBiostatistics10th'))
dim(d931)
head(d931)
names(d931)[2:3] = c('Waist', 'AT') # change the variable names so that they are intuitive
head(d931)
plot(AT ~ Waist, data = d931, xlab = 'Waist circumference (cm), X', ylab = 'Deep abdominal AT area (cm2), Y', 
     main = 'Page 419, Figure 9.3.1')

# Page 436, Example 9.4.2
# Inference for linear regression 
summary(mod_931 <- lm(AT ~ Waist, data = d931))
# Look to section of 'Coefficients:'
# '(Intercept)' is beta_0. We seldom look to the significance of intercept,
# ... as it's a 'value'
# 'Waist' is beta_1.  We focus on the significance of beta_1, as it's a 'rate'.
# Data supports a linear relationship between Waist and AT (p < .001)

cor(d931[2:3]) # ?stats::cor
cor.test(~ AT + Waist, data = d931) # test H0: rho = 0 
# ?stats::cor.test uses t-test described in Page 435-436, equation (9.4.9)
# This is the same t-statistics for beta_1 of regression model

confint(mod_931) # confidence interval of regression coefficients
anova(mod_931)

plot(AT ~ Waist, data = d931, xlab = 'Waist circumference (cm), X', ylab = 'Deep abdominal AT area (cm2), Y', 
     main = 'Page 422, Figure 9.3.3')
mod_931$coefficients # beta_0 and beta_1
abline(a = mod_931$coefficients[1L], b = mod_931$coefficients[2L], col = 'red')
abline(reg = mod_931, col = 'blue')  # another usage of ?graphics::abline; simpler and equivalent


# Page 440, Example 9.4.3
# Diagnostic plot for linear regression
plot(mod_931, which = 1, main = 'Page 440, Figure 9.4.8')


# Page 441, Section 9.5
head(pred_931 <- predict(mod_931, newdata = d931, interval = 'predict'))
head(conf_931 <- predict(mod_931, newdata = d931, interval = 'confidence'))
plot(AT ~ Waist, data = d931, xlab = 'Waist circumference (cm), X', ylab = 'Deep abdominal AT area (cm2), Y', 
     main = 'Page 442, Figure 9.5.1', sub = 'Confidence Intervals and Prediction Intervals')
abline(reg = mod_931)
waist_ord <- order(d931$Waist)
matlines(x = d931$Waist[waist_ord], y = pred_931[waist_ord,2:3], col = 2, lty = 2)
matlines(x = d931$Waist[waist_ord], y = conf_931[waist_ord,2:3], col = 4, lty = 3)


# Quiz in class
summary(mod_931_centered <- lm(AT ~ I(Waist - 90), data = d931))
# How to interpret this '(Intercept)'?
# If a patient has Waist = ???, his AT would have a mean of 95.32
plot(AT ~ I(Waist - 90), data = d931)
abline(reg = mod_931_centered)


# Page 447, Example 9.7.1
d971 = read.csv(system.file('extdata', 'EXA_C09_S07_01.csv', package = 'DanielBiostatistics10th'))
dim(d971)
head(d971)
summary(mod_971 <- lm(CV ~ HEIGHT, data = d971))
plot(CV ~ HEIGHT, data = d971, xlab = 'Height (cm)', ylab = 'Cv (units)', main = 'Page 449, Figure 9.7.2')
abline(reg = mod_971)

# Page 452, Example 9.7.2
cor(d971) # Page 451, Figure 9.7.4, Figure 9.7.5
cor.test(~ CV + HEIGHT, data = d971)

# Page 453, When the Hypothesized rho Is a Nonzero Value
# R does not have a function to do this

