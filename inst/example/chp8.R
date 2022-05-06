library(DanielBiostatistics10th)

# Page 318, Example 8.2.1
dim(d821_raw <- read.csv('data/EXA_C08_S02_01.csv'))
head(d821_raw) # one `Meat` per column, not a desired format for analysis
colSums(!is.na(d821_raw)) # the data has 42 venison, 30 squirrel, etc
head(d821 <- reshape2::melt(
  data = d821_raw, 
  # measure.vars = names(d821_raw),
  value.name = 'Selenium', variable.name = 'Meat'))
# `d821` has a desired format: 1st column denote `Meat` and 2nd column `Selenium`
boxplot(Selenium ~ Meat, data = d821) # Page 323, Figure 8.2.7
# ?stats::aov fits an analysis-of-variance model
(aov_821 = aov(Selenium ~ Meat, data = d821))
# ?stats::anova provides the ANOVA table
anova(aov_821)
# Interpretation: ANOVA shows that the three group means are not all same (p < .001)
# you don't need to specify 'alpha' here
# the line 'Signif. codes' gives you
# 1. potential choices of alpha's as the 'cut-off values'
# 2. significance level based on your choice of alpha
# for example, '< 2.2e-16 ***' means you should reject the null at 1/1000 significance level 



# Page 325, Example 8.2.2
library(multcomp)
summary(tukey_822 <- glht(aov_821, linfct = mcp(Meat = 'Tukey'))) # Tukey's Honestly Significant Difference
# Tukey HSD of multiple comparisons shows that all but one pairwise differences are significant (p < .001)
confint(tukey_822) # Page 326, Figure 8.2.8



# Page 339, Example 8.3.1
head(d831_raw <- read.csv('data/EXA_C08_S03_01.csv'))
class(d831_raw$ageGroup)
d831 = within(d831_raw, expr = {
  ageGroup = structure(ageGroup, levels = c('<20', '20s', '30s', '40s', '>50'), class = 'factor')
})
class(d831$ageGroup)
head(d831$ageGroup)
(aov_831 = aov(time ~ method + ageGroup, data = d831))
anova(aov_831)



# Page 348, Example 8.4.1
head(d841_raw <- read.csv('data/EXA_C08_S04_01.csv'))
d841 = within(d841_raw, expr = {
  SUBJ = factor(SUBJ)
  TIME = structure(TIME, levels = c('Baseline', '1-Mon', '3-Mon', '6-Mon'), class = 'factor')
})
(aov_841 = aov(FUNC ~ SUBJ + TIME, data = d841))
anova(aov_841)



# Page 352, Example 8.4.2 (optional; out of the scope of this course)
(d842_raw <- read.csv('data/EXA_C08_S04_02.csv'))
# 'subject': patient ID (NOT repeated measures, REMOVE)
# treatment: 1 (placebo) and 2 (aloe juice)
# totalC[1-4]: measurement at four timepoints, baseline, 2wk, 4wk and 6wk
d842 = reshape2::melt(within(d842_raw, expr = {
  subject = factor(subject)
  treatment = factor(treatment)
}), id.vars = c('subject', 'treatment'), variable.name = 'time', value.name = 'OralScores')
head(d842)
# Hypothesis: 
# Main effect of 'treatment';
# Main effect of 'time';
# Interaction between 'treatment' and 'time'
(aov_842 = aov(OralScores ~ treatment * time + Error(subject), data = d842))
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
# Analyasis Interpretation
# .. No significant difference detected between placebo vs. aloe at baseline (p = .815)
# .. No significant difference detected in the trends over time between placebo vs. aloe (p = .974)
# .. Significant difference detected among the four measurement times, for either placebo or aloe patients (p = 3e-7)
# R code below creates an equivalent ANOVA model
anova(aov(OralScores ~ treatment * time + subject, data = d842))
# .. 'subject' is considered as a block factor 


# Page 364, Example 8.5.2
head(d852_raw <- read.csv('data/EXA_C08_S05_02.csv'))
d852 = within(d852_raw, expr = {
  A = structure(A, levels = c('Cardiac', 'Cancer', 'CVA', 'Tuberculosis'), class = 'factor')
  B = structure(B, levels = c('20s', '30s', '40s', '50+'), class = 'factor')
})
(aov_852 = aov(HOME ~ A * B, data = d852))
anova(aov_852)

summary(lm(HOME ~ A * B, data = d852)) # produces alpha, beta and (alpha beta)'s in the formulation 




