library(DanielBiostatistics10th)
library(reshape2)

# Page 318, Example 8.2.1
head(EXA_C08_S02_01)
d821 = within(EXA_C08_S02_01, expr = {
  type = factor(type)
})
boxplot(selenium ~ type, data = d821, main = 'Page 323, Figure 8.2.7')
(aov_d821 = aov(selenium ~ type, data = d821)) 
# ?stats::aov # analysis-of-variance model
anova(aov_d821) 
# ?stats::anova # ANOVA table

# Page 325, Example 8.2.2
(tukey_d822 <- TukeyHSD(aov_d821, conf.level = 0.95)) # Page 326, Figure 8.2.8
plot(tukey_d822)

# Page 339, Example 8.3.1
head(EXA_C08_S03_01)
head(d831 <- within(EXA_C08_S03_01, expr = {
  ageGroup = structure(ageGroup, levels = c('<20', '20s', '30s', '40s', '>50'), class = 'factor')
}))
(aov_831 = aov(time ~ method + ageGroup, data = d831))
anova(aov_831)

# Page 348, Example 8.4.1
head(EXA_C08_S04_01)
head(d841 <- within(EXA_C08_S04_01, expr = {
  SUBJ = factor(SUBJ)
  TIME = structure(TIME, levels = c('Baseline', '1-Mon', '3-Mon', '6-Mon'), class = 'factor')
}))
(aov_841 = aov(FUNC ~ SUBJ + TIME, data = d841))
anova(aov_841)

# Page 352, Example 8.4.2 (optional; out of the scope of this course)
head(EXA_C08_S04_02)
names(EXA_C08_S04_02)[3:6] = c('baseline', '2wk', '4wk', '6wk')
head(d842a <- within(EXA_C08_S04_02, expr = {
  subject = factor(subject)
  treatment = structure(treatment, levels = c('placebo', 'aloe_juice'), class = 'factor')
}))
head(d842b <- reshape2::melt(d842a, id.vars = c('subject', 'treatment'), 
                             variable.name = 'time', value.name = 'OralScores'))
# Hypothesis: 
# Main effect of 'treatment';
# Main effect of 'time';
# Interaction between 'treatment' and 'time'
(aov_842 = aov(OralScores ~ treatment * time + Error(subject), data = d842b))
class(aov_842)
summary(aov_842)
# Section 'Error: subject' in R output
# .. is Figure 8.4.4 (Page 355) 'Tests of Between-Subjects Effects' (without the row of 'Intercept')
# .. 'treatment' row: effect of treatment at \strong{baseline} (i.e. reference time), degree-of-freedom (dof) = 2-1 = 1
# .. 'Residuals' row: residual at baseline, dof = (25-1) - (2-1) = 23
# .. It's important to note that 'treatment' is a \strong{between-subject factor}.
# Section 'Error: Within' in R output
# .. is Figure 8.4.4 'Tests of Within-Subjects Effects'
# .. 'time' row: effect of time within subject for placebo (i.e. reference treatment), dof = 4-1 = 3
# .. 'treatment:time' row: interation of treatment and time, dof = (2-1)*(4-1) = 3
# .. 'Residuals' row: residual at 2wk, 4wk and 6wk, dof = (4-1)*23 = 69 [(4-1) timepoints, 23 dof at each timepoints]
# Analysis Interpretation
# .. No significant difference detected between placebo vs. aloe at baseline (p = .815)
# .. No significant difference detected in the trends over time between placebo vs. aloe (p = .974)
# .. Significant difference detected among the four measurement times, for either placebo or aloe patients (p = 3e-7)
# R code below creates an equivalent ANOVA model
anova(aov(OralScores ~ treatment * time + subject, data = d842b))
# .. 'subject' is considered as a block factor 

# Page 364, Example 8.5.2
head(EXA_C08_S05_02)
head(d852 <- within(EXA_C08_S05_02, expr = {
  A = structure(A, levels = c('Cardiac', 'Cancer', 'CVA', 'Tuberculosis'), class = 'factor')
  B = structure(B, levels = c('20s', '30s', '40s', '50+'), class = 'factor')
}))
(aov_852 = aov(HOME ~ A * B, data = d852))
anova(aov_852)
summary(lm(HOME ~ A * B, data = d852)) # produces alpha, beta and (alpha beta)'s in the formulation 
