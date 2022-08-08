library(DanielBiostatistics10th)

# Page 417, Example 9.3.1
d931 <- read.csv(system.file('extdata', 'EXA_C09_S03_01.csv', package = 'DanielBiostatistics10th'))
head(d931)
names(d931)[2:3] = c('Waist', 'AT')
plot(AT ~ Waist, data = d931, xlab = 'Waist circumference (cm), X', 
     ylab = 'Deep abdominal AT area (cm2), Y', main = 'Page 419, Figure 9.3.1')

# Page 436, Example 9.4.2
summary(m931 <- lm(AT ~ Waist, data = d931))
cor(d931[2:3]); cor.test(~ AT + Waist, data = d931)
confint(m931) # confidence interval of regression coefficients
anova(m931)

# Page 440, Example 9.4.3
plot(m931, which = 1, main = 'Page 440, Figure 9.4.8')

# Page 441, Section 9.5
autoplot(predict_lm(m931), xlab = 'Waist circumference (cm), X', 
         ylab = 'Deep abdominal AT area (cm2), Y',
         title = 'Page 422, Figure 9.3.3; Page 442, Figure 9.5.1')

# Page 447, Example 9.7.1
d971 = read.csv(system.file('extdata', 'EXA_C09_S07_01.csv', package = 'DanielBiostatistics10th'))
head(d971)
summary(mod_971 <- lm(CV ~ HEIGHT, data = d971))
autoplot(predict_lm(mod_971), xlab = 'Height (cm)', ylab = 'Cv (units)', 
         title = 'Page 449, Figure 9.7.2')

# Page 452, Example 9.7.2
cor(d971); cor.test(~ CV + HEIGHT, data = d971) # Page 451, Figure 9.7.4, Figure 9.7.5

# Page 453, When the Hypothesized rho Is a Nonzero Value
# R does not have a function to do this

