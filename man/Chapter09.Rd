% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chp9.R
\name{Chapter09}
\alias{Chapter09}
\title{Chapter 9: Simple Linear Regression and Correlation}
\value{
No function defined for Chapter 9.
}
\description{
Examples in Chapter 9, \emph{Simple Linear Regression and Correlation}.
}
\examples{
library(DanielBiostatistics10th)

# Example 9.3.1; Page 417 (10th ed), Page 358 (11th ed) 
head(EXA_C09_S03_01)
names(EXA_C09_S03_01)[2:3] = c('Waist', 'AT')
plot(AT ~ Waist, data = EXA_C09_S03_01, 
     xlab = 'Waist circumference (cm), X', 
     ylab = 'Deep abdominal AT area (cm2), Y', 
     main = 'Figure 9.3.1')
m931 = lm(AT ~ Waist, data = EXA_C09_S03_01)
plot(AT ~ Waist, data = EXA_C09_S03_01, 
     xlab = 'Waist circumference (cm), X', 
     ylab = 'Deep abdominal AT area (cm2), Y', 
     main = 'Figure 9.3.3'); abline(m931, col = 'red', lty = 2)

# Example 9.4.1; Page 432 (10th ed), Page 372 (11th ed) 
anova(m931) # Table 9.4.1. Page 434 (10th ed)

# Example 9.4.2; Page 436 (10th ed), Page 375 (11th ed)
summary(m931)
confint(m931) # Page 438 (10th ed)

# (omitted) Example 9.4.3; Page 376 (11th ed) 

# (optional)
# Example 9.4.3; Page 440 (10th ed)
# Example 9.4.4; Page 379 (11th ed)
plot(m931, which = 1, main = 'Figure 9.4.8 (10th) or 9.4.9 (11th)')

# Example 9.7.1; Page 447 (10th ed), Page 386 (11th ed) 
head(EXA_C09_S07_01)
plot(CV ~ HEIGHT, data = EXA_C09_S07_01, 
     xlab = 'Height (cm)', 
     ylab = 'Cv (units)', 
     main = 'Figure 9.7.2')
m971 = lm(CV ~ HEIGHT, data = EXA_C09_S07_01)
summary(m971); anova(m971) # Figure 9.7.3; Page 450 (10th ed)

# Example 9.7.2; Page 452 (10th ed), Page 390 (11th ed) 
cor(EXA_C09_S07_01); 
cor.test(~ CV + HEIGHT, data = EXA_C09_S07_01) # Figure 9.7.4, 9.7.5

# Page 453, When the Hypothesized rho Is a Nonzero Value
# R does not have a function to do this
}
