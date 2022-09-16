library(DanielBiostatistics10th)

# Page 417, Example 9.3.1
head(EXA_C09_S03_01)
names(EXA_C09_S03_01)[2:3] = c('Waist', 'AT')
plot(AT ~ Waist, data = EXA_C09_S03_01, xlab = 'Waist circumference (cm), X', 
     ylab = 'Deep abdominal AT area (cm2), Y', main = 'Page 419, Figure 9.3.1')

# Page 436, Example 9.4.2
summary(m931 <- lm(AT ~ Waist, data = EXA_C09_S03_01))
cor(EXA_C09_S03_01[2:3]); cor.test(~ AT + Waist, data = EXA_C09_S03_01)
confint(m931) # confidence interval of regression coefficients
anova(m931)

# Page 440, Example 9.4.3
plot(m931, which = 1, main = 'Page 440, Figure 9.4.8')

# Page 441, Section 9.5
autoplot(predict_lm(m931), xlab = 'Waist circumference (cm), X', 
         ylab = 'Deep abdominal AT area (cm2), Y',
         title = 'Page 422, Figure 9.3.3; Page 442, Figure 9.5.1')

# Page 447, Example 9.7.1
head(EXA_C09_S07_01)
summary(mod_971 <- lm(CV ~ HEIGHT, data = EXA_C09_S07_01))
autoplot(predict_lm(mod_971), xlab = 'Height (cm)', ylab = 'Cv (units)', 
         title = 'Page 449, Figure 9.7.2')

# Page 452, Example 9.7.2
cor(EXA_C09_S07_01); cor.test(~ CV + HEIGHT, data = EXA_C09_S07_01) 
# Page 451, Figure 9.7.4, Figure 9.7.5

# Page 453, When the Hypothesized rho Is a Nonzero Value
# R does not have a function to do this

