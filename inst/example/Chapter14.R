library(DanielBiostatistics10th)
# To run a line of code, use shortcut
# Command + Enter: Mac and RStudio Cloud
# Control + Enter: Windows, Mac and RStudio Cloud
# To clear the console
# Control + L: Mac and RStudio Cloud




library(survival)
library(ggfortify)

# In this class we introduce 4 functions
# ?survival::Surv. to create a time-to-event variable
# ?survival::survfit. to find the Kaplan-Meier estimates for the survival function
# ?survival::survdiff.  log rank test to compare survival functions
# ?survival::coxph.  to perform Cox proportional hazard model
# ?ggfortify:::autoplot.survfit. to plot the Kaplan-Meier curves


# Page 756, Example 14.3.1
head(d1431_raw <- read.csv('data/EXA_C14_S03_01.csv'))
head(d1431 <- within(d1431_raw, expr = {
  edp = Surv(TIME, event = (VITAL != 'ned')) # ?survival::Surv
  rm(SUBJ, TIME, VITAL)
}))
class(d1431$edp) # 'Surv'
head(d1431$edp)
summary(sf_1431 <- survfit(edp ~ TUMOR, data = d1431)) # Page 758, Table 14.3.2
# ?survival::survfit
autoplot(sf_1431) # Page 761, Figure 14.3.1


# Page 764, Example 14.4.1
survdiff(edp ~ TUMOR, data = d1431, rho = 0) # `rho = 0` for log-rank test




# Page 769, Example 14.5.1
head(d1451_raw <- read.csv('data/EXA_C14_S05_01.csv'))
head(d1451 <- within(d1451_raw, expr = {
  edp = Surv(time, status)
  rm(time, status, subject)
  drug = relevel(structure(drug, levels = c('Opiate', 'Other'), class = 'factor'), ref = 'Other')
}))

# ?survival::coxph : Cox proportional hazard model
summary(model1_1451 <- coxph(edp ~ drug + age, data = d1451))
# 'Opiate' has higher hazard compared to Drug='Other' (hazard ratio (HR) = 9.407, p < .001)
# Under the 'proportional hazard' assumption, we don't need to discuss Opiate vs. Other at any specific time point
confint(model1_1451)

# 'age' is not significant (p = .772), so it should be removed from the model
summary(model2_1451 <- coxph(edp ~ drug, data = d1451))
confint(model2_1451)
# 'Opiate' has higher hazard compared to Drug='Other' (hazard ratio (HR) = 9.923, p < .001)
autoplot(survfit(edp ~ drug, data = d1451)) # Page 771, Figure 14.5.1
