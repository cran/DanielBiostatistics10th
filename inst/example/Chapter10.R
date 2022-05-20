library(DanielBiostatistics10th)
# To run a line of code, use shortcut
# Command + Enter: Mac and RStudio Cloud
# Control + Enter: Windows, Mac and RStudio Cloud
# To clear the console
# Control + L: Mac and RStudio Cloud


library(car)

# Page 493, Example 10.3.1
d1031 = read.csv(system.file('extdata', 'EXA_C10_S03_01.csv', package = 'DanielBiostatistics10th'))
head(d1031)
car::scatterplotMatrix(~ AGE + EDLEVEL + CDA, data = d1031, smooth = FALSE, regLine = TRUE,
                       main = 'Page 494, Figure 10.3.1')
# perform multivariable linear model
summary(mod_1031 <- lm(CDA ~ AGE + EDLEVEL, data = d1031))
# Page 502, Example 10.4.1, see 'Multiple R-squared' (not 'Adjusted R-squared')
# ... percentages of variation being explained by the model

# Page 504, Example 10.4.2, see 'F-statistic'
# ... testing H0: no linear relationship between CDA vs. {Age and EDLEVEL}

# Page 505, Example 10.4.3, see the regression table under 'Coefficients:'
# ... significance of `Age`
# ... for every 1 year increase in `Age`, `CDA` will decrease .184 (p<.001)


# Page 506, confidence interval for beta's
confint(mod_1031)


# Page 509, Example 10.5.1
(newd_1031 = data.frame(AGE = 68, EDLEVEL = 12))
predict(mod_1031, newdata = newd_1031, interval = 'prediction')
predict(mod_1031, newdata = newd_1031, interval = 'confidence')



# Page 511, Example 10.6.1
d1061 = read.csv(system.file('extdata', 'EXA_C10_S06_01.csv', package = 'DanielBiostatistics10th'))
head(d1061)
car::scatterplotMatrix(~ W + P + S, data = d1061, smooth = FALSE, regLine = TRUE,
                       main = 'Scatter Plot Matrix of Example 10.6.1')
summary(mod_1061 <- lm(W ~ P + S, data = d1061))
# Both the regression model and scatter plot matrix indicate that 
# `P` does not have a significant linear relationship with the endpoint `W`
# Ideally, `P` should be removed from the multivariable regression model
# (which is not performed by the textbook though)
confint(mod_1061)

# (optional) Page 515, Example 10.6.2
psych::partial.r(d1061, x = 2:3, y = 1L)


