library(DanielBiostatistics10th)

# Page 493, Example 10.3.1
head(EXA_C10_S03_01)
pairs(EXA_C10_S03_01, main = 'Page 494, Figure 10.3.1')
summary(mod_1031 <- lm(CDA ~ AGE + EDLEVEL, data = EXA_C10_S03_01))

# Page 502, Example 10.4.1
# .. see 'Multiple R-squared' (not 'Adjusted R-squared')

# Page 504, Example 10.4.2
# .. see 'F-statistic'

# Page 505, Example 10.4.3
# .. see 'Coefficients:'

# Page 506, confidence interval for beta's
confint(mod_1031)

# Page 509, Example 10.5.1
(newd_1031 = data.frame(AGE = 68, EDLEVEL = 12))
predict(mod_1031, newdata = newd_1031, interval = 'prediction')
predict(mod_1031, newdata = newd_1031, interval = 'confidence')

# Page 511, Example 10.6.1
head(EXA_C10_S06_01)
pairs(EXA_C10_S06_01, main = 'Scatter Plot Matrix of Example 10.6.1')
summary(mod_1061 <- lm(W ~ P + S, data = EXA_C10_S06_01))
confint(mod_1061)

# (optional) Page 515, Example 10.6.2
psych::partial.r(EXA_C10_S06_01, x = 2:3, y = 1L)
