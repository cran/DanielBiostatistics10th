library(DanielBiostatistics10th)
library(survival)
library(ggfortify)

# Page 756, Example 14.3.1
head(EXA_C14_S03_01)
head(d1431 <- within(EXA_C14_S03_01, expr = {
  edp = Surv(TIME, event = (VITAL != 'ned')) # ?survival::Surv # create a time-to-event variable
}))
class(d1431$edp) # 'Surv'
head(d1431$edp)
summary(sf_1431 <- survfit(edp ~ TUMOR, data = d1431)) # Page 758, Table 14.3.2
# ?survival::survfit # to find the Kaplan-Meier estimates for the survival function
autoplot(sf_1431) + labs(title = 'Page 761, Figure 14.3.1')
# ?ggfortify:::autoplot.survfit # to plot the Kaplan-Meier curves

# Page 764, Example 14.4.1
survdiff(edp ~ TUMOR, data = d1431, rho = 0) # `rho = 0` for log-rank test
# ?survival::survdiff # log rank test to compare survival functions

# Page 769, Example 14.5.1
head(EXA_C14_S05_01)
head(d1451 <- within(EXA_C14_S05_01, expr = {
  edp = Surv(time, status)
  drug = relevel(structure(drug, levels = c('Opiate', 'Other'), class = 'factor'), ref = 'Other')
}))
summary(model1_1451 <- coxph(edp ~ drug + age, data = d1451))
# ?survival::coxph # Cox proportional hazard model
# 'Opiate' has higher hazard compared to Drug='Other' (hazard ratio (HR) = 9.407, p < .001)
# With 'proportional hazard' assumption, 
# ... we don't need to discuss Opiate vs. Other at any specific time point
confint(model1_1451)
# 'age' is not significant (p = .772), so it should be removed from the model
summary(model2_1451 <- coxph(edp ~ drug, data = d1451))
confint(model2_1451)
# 'Opiate' has higher hazard compared to Drug='Other' (hazard ratio (HR) = 9.923, p < .001)
autoplot(survfit(edp ~ drug, data = d1451)) + labs(title = 'Page 771, Figure 14.5.1')
