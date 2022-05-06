library(DanielBiostatistics10th)

# Page 605, Example 12.3.1
d1231 = c(1L, 3L, 8L, 18L, 6L, 4L, 4L, 3L) # Page 605, Table 12.3.1
breaks <- c(-Inf, seq(from = 125, to = 275, by = 25), Inf)
p_1231 <- diff.default(pnorm(q = breaks, mean = 198.67, sd = 41.31))
e_1231 = sum(d1231) * p_1231
chisq_1231 = (d1231 - e_1231)^2 / e_1231
noquote(matrix(
  c(d1231, sprintf('%.4f', p_1231), sprintf('%.1f', e_1231), sprintf('%.3f', chisq_1231)), 
  ncol = 4L,
  dimnames = list(
    levels(cut(double(), breaks = breaks, right = FALSE, include.lowest = TRUE)),
    c('Observed Freq', 'Expected Relative Freq', 'Expected Freq', '(O-E)^2/E')
  )), right = TRUE) # Page 607, Table 12.3.2; Page 608, Table 12.3.3
pchisq(sum(chisq_1231), df = length(d1231) - 3L, lower.tail = FALSE)
# Explanation: I cannot reject the null hypothesis of the observations comes from 
# a population of N(198.67, 41.31^2) at 5% significance level
# -3L: three restrictions (explained on Page 608)
# (1) making sum(xo) == sum(xe)
# (2) estimating mean
# (3) estimating sd



# Page 609, Example 12.3.2
# 100 doctors, 25 patients per doctor
d1232 = c(5L, 6L, 8L, 10L, 10L, 15L, 17L, 10L, 10L, 9L, 0L)
o_1232 = c(sum(d1232[1:2]), d1232[-(1:2)])
(prob_1232 = sum((0:10) * d1232) / (25 * 100)) # binomial `prob`
p_1232 = c(pbinom(1L, size = 25L, prob = prob_1232),
           dbinom(2:9, size = 25L, prob = prob_1232),
           pbinom(9, size = 25L, prob = prob_1232, lower.tail = FALSE))
e_1232 = sum(o_1232) * p_1232
noquote(matrix(
  c(o_1232, sprintf('%.4f', p_1232), sprintf('%.2f', e_1232)),
  ncol = 3L,
  dimnames = list(
    c('0-1', 2:9, '10 or more'),
    c('Observed Freq', 'Expected Relative Freq', 'Expected Freq')
  )), right = TRUE) # Page 610, Table 12.3.5
(chi_1232 = sum((o_1232 - e_1232)^2 / e_1232))
pchisq(chi_1232, df = length(o_1232) - 2L, lower.tail = FALSE)
# Conclusion: we reject the null hypothesis that my observations are from a binomial distribution
# -2L: two restrictions (explained on Page 611)
# (1) making sum(o) == sum(e)
# (2) estimating prob_1232




# Page 611, Example 12.3.3
d1233 = c(5L, 14L, 15L, 23L, 16L, 9L, 3L, 3L, 1L, 1L, 0L)
o_1233 = c(d1233[1:8], sum(d1233[-(1:8)]))
# lambda = 3 is provided by the textbook
p_1233 = c(dpois(0:7, lambda = 3),
           ppois(7L, lambda = 3, lower.tail = FALSE))
e_1233 = sum(o_1233) * p_1233
chisq_1233 = (o_1233 - e_1233)^2 / e_1233
noquote(matrix(
  c(o_1233, sprintf('%.3f', p_1233), sprintf('%.2f', e_1233), sprintf('%.3f', chisq_1233)),
  ncol = 4L,
  dimnames = list(
    c(0:7, '8 or more'),
    c('Observed Freq', 'Expected Relative Freq', 'Expected Freq', '(O-E)^2/E')
  )), right = TRUE) # Page 613, Table 12.3.8
pchisq(sum(chisq_1233), df = length(o_1233) - 1L, lower.tail = FALSE)
# Conclusion: cannot reject the null hypothesis that observations come from Poisson(lambda = 3)
# -1L: one restrictions
# (1) making sum(xo) == sum(xe)




# Page 614, Example 12.3.4
d1234 = c('Dec 05' = 62L, 'Jan 06' = 84L, 'Feb 06' = 17L, 'Mar 06' = 16L, 'Apr 06' = 21L)
e_1234 = mean(d1234)
chisq_1234 = (d1234 - e_1234)^2 / e_1234
noquote(
  cbind(Observed = d1234, Expected = e_1234, 'Contribution to Chi-Sq' = chisq_1234),
  right = TRUE) # Page 615, Figure 12.3.2
pchisq(sum(chisq_1234), df = length(d1234) - 1L, lower.tail = FALSE)



# Page 616, Example 12.3.5
d1235 = c(dominant = 43L, heterozygous = 125L, recessive = 32L)
(e_1235 = sum(d1235) * c(1,2,1) / sum(c(1,2,1)))
(chisq_1235 = sum((d1235 - e_1235)^2 / e_1235))
pchisq(chisq_1235, df = length(d1235) - 1L, lower.tail = FALSE)


# Page 621, Example 12.4.1
addmargins(d1241 <- array(c(260L, 15L, 7L, 299L, 41L, 14L), dim = c(3L, 2L), dimnames = list(
  Race = c('White', 'Black', 'Other'),
  FolicAcid = c('TRUE', 'FALSE')
)))
chisq.test(d1241) # ?stats::chisq.test
# Conclusion: I would reject the null hypothesis 
# .. that the perception of usage of folic acid and race are independent (p = .011).


# Page 626, Example 12.4.2
addmargins(d1242 <- array(c(131L, 14L, 52L, 36L), dim = c(2L, 2L), dimnames = list(
  Type = c('Faller', 'Non-Faller'),
  LifestyleChange = c('TRUE', 'FALSE')
)))
chisq.test(d1242, correct = FALSE)
chisq.test(d1242, correct = TRUE) # Page 627, Yates's Correction
# Conclusion: we determined that the fear of falling does result in lifestyle change (p<.001)



# Page 631, Example 12.5.1
addmargins(d1251 <- array(c(21L, 19L, 75L, 77L), dim = c(2L, 2L), dimnames = list(
  Group = c('Narcoleptic', 'Healthy'),
  Migraine = c('TRUE', 'FALSE')
)))
(chisq_1251 = chisq.test(d1251, correct = FALSE))
# Conclusion: we cannot reject the null hypothesis 
# .. that the two populations are homogeneous in migrane (p = .722)
#
# (Optional) using test on two proportions
# prop_test_CLT(x = c(21L, 19L), n = 96L, null.value = 0)
# unname(0.355^2 - chisq_1251$statistic) # only true for 2*2 contingency table



# Page 638, Example 12.6.1
addmargins(d1262 <- array(c(2L, 8L, 7L, 4L), dim = c(2L, 2L), dimnames = list(
  Group = c('PI_Naive', 'PA_Experienced'),
  Regimen2yr = c('TRUE', 'FALSE')
)))
fisher.test(d1262)




# Page 644, Example 12.7.1
addmargins(d1271 <- array(c(22L, 18L, 216L, 199L), dim = c(2L, 2L), 
 dimnames = list(Exercising = c('Extreme', 'No'), PretermLabor = c('TRUE', 'FALSE'))))
relativeRisk(d1271)
# textbook confidence interval (.65, 1.86) wrong (too many rounding in intermediate steps)



# Page 647, Example 12.7.2
addmargins(d1272 <- array(c(64L, 68L, 342L, 3496L), dim = c(2L, 2L), dimnames = list(
 SmkPregnancy = c('TRUE', 'FALSE'),
 Obesity = c('TRUE', 'FALSE')
)))
oddsRatio(d1272)



# Page 650, Example 12.7.3
# Page 652, Example 12.7.4
(d1273 <- array(c(21L, 16L, 11L, 6L, 50L, 18L, 14L, 6L), dim = c(2L, 2L, 2L), 
 dimnames = list(HTN = c('Present', 'Absent'), OCAD = c('Cases', 'Controls'), 
  Age = c('<=55', '>55'))))
addmargins(d1273, margin = 1:2) # Page 651, Table 12.7.6
mantelhaen.test(d1273)
