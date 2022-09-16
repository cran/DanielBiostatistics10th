library(DanielBiostatistics10th)

# Page 605, Example 12.3.1
d1231_b = c(-Inf, seq.int(from = 125, to = 275, by = 25), Inf)
(d1231 = setNames( # Page 605, Table 12.3.1
  c(1L, 3L, 8L, 18L, 6L, 4L, 4L, 3L), 
  nm = levels(cut(double(), breaks = d1231_b, right = FALSE, include.lowest = TRUE))))
chi1231 = print_OE(d1231, prob = diff.default(pnorm(q = d1231_b, mean = 198.67, sd = 41.31)))
pchisq(sum(chi1231), df = length(d1231) - 3L, lower.tail = FALSE)
# -3L: three restrictions (explained on Page 608)
# (1) making sum(xo) == sum(xe)
# (2) estimating mean
# (3) estimating sd

# Page 609, Example 12.3.2
# 100 doctors, 25 patients per doctor
d1232 = c(5L, 6L, 8L, 10L, 10L, 15L, 17L, 10L, 10L, 9L, 0L)
o1232 = setNames(c(sum(d1232[1:2]), d1232[-(1:2)]), nm = c('0-1', 2:9, '10 or more'))
(p1232 = sum((0:10) * d1232) / (25 * 100)) # binomial `prob`
chi1232 = print_OE(o1232, prob = c(
  pbinom(1L, size = 25L, prob = p1232),
  dbinom(2:9, size = 25L, prob = p1232),
  pbinom(9, size = 25L, prob = p1232, lower.tail = FALSE)))
pchisq(sum(chi1232), df = length(o1232) - 2L, lower.tail = FALSE)
# -2L: two restrictions (explained on Page 611)
# (1) making sum(o) == sum(e)
# (2) estimating p1232

# Page 611, Example 12.3.3
d1233 = c(5L, 14L, 15L, 23L, 16L, 9L, 3L, 3L, 1L, 1L, 0L)
o_1233 = setNames(c(d1233[1:8], sum(d1233[-(1:8)])), nm = c(0:7, '8 or more'))
p_1233 = c(dpois(0:7, lambda = 3), # lambda = 3 is provided by the textbook
           ppois(7L, lambda = 3, lower.tail = FALSE))
chi1233 = print_OE(o_1233, prob = p_1233)
pchisq(sum(chi1233), df = length(o_1233) - 1L, lower.tail = FALSE)
# -1L: one restrictions
# (1) making sum(xo) == sum(xe)
chisq.test(o_1233, p = p_1233) # equivalent # warning on any(E < 5)

# Page 614, Example 12.3.4
d1234 = c('Dec 05' = 62L, 'Jan 06' = 84L, 'Feb 06' = 17L, 'Mar 06' = 16L, 'Apr 06' = 21L)
chi1234 = print_OE(d1234)
pchisq(sum(chi1234), df = length(d1234) - 1L, lower.tail = FALSE)
chisq.test(d1234) # equivalent

# Page 616, Example 12.3.5
d1235 = c(dominant = 43L, heterozygous = 125L, recessive = 32L)
chi1235 = print_OE(d1235, prob = c(1, 2, 1))
pchisq(sum(chi1235), df = length(d1235) - 1L, lower.tail = FALSE)
chisq.test(d1235, p = c(1, 2, 1), rescale.p = TRUE) # equivalent

# Page 621, Example 12.4.1
addmargins(d1241 <- array(c(260L, 15L, 7L, 299L, 41L, 14L), dim = c(3L, 2L), dimnames = list(
  Race = c('White', 'Black', 'Other'),
  FolicAcid = c('TRUE', 'FALSE'))))
chisq.test(d1241) # ?stats::chisq.test

# Page 626, Example 12.4.2
addmargins(d1242 <- array(c(131L, 14L, 52L, 36L), dim = c(2L, 2L), dimnames = list(
  Type = c('Faller', 'Non-Faller'),
  LifestyleChange = c('TRUE', 'FALSE'))))
chisq.test(d1242, correct = FALSE)
chisq.test(d1242, correct = TRUE) # Page 627, Yates's Correction

# Page 631, Example 12.5.1
addmargins(d1251 <- array(c(21L, 19L, 75L, 77L), dim = c(2L, 2L), dimnames = list(
  Group = c('Narcoleptic', 'Healthy'),
  Migraine = c('TRUE', 'FALSE'))))
(chisq_1251 = chisq.test(d1251, correct = FALSE))
if (FALSE) {
  # (optional) using test on two proportions
  # only equivalent for 2*2 contingency table
  (clt_1251 = prop_CLT(x = c(21L, 19L), n = 96L, null.value = 0))
  all.equal.numeric(unname(clt_1251$statistic^2), unname(chisq_1251$statistic))
}

# Page 638, Example 12.6.1
addmargins(d1262 <- array(c(2L, 8L, 7L, 4L), dim = c(2L, 2L), dimnames = list(
  Group = c('PI_Naive', 'PA_Experienced'),
  Regimen2yr = c('TRUE', 'FALSE'))))
fisher.test(d1262)

# Page 644, Example 12.7.1
addmargins(d1271 <- array(c(22L, 18L, 216L, 199L), dim = c(2L, 2L), 
 dimnames = list(Exercising = c('Extreme', 'No'), PretermLabor = c('TRUE', 'FALSE'))))
relativeRisk(d1271)
# textbook confidence interval (.65, 1.86) wrong (too many rounding in intermediate steps)

# Page 647, Example 12.7.2
addmargins(d1272 <- array(c(64L, 68L, 342L, 3496L), dim = c(2L, 2L), dimnames = list(
  SmkPregnancy = c('TRUE', 'FALSE'),
  Obesity = c('TRUE', 'FALSE'))))
oddsRatio(d1272)

# Page 650, Example 12.7.3
# Page 652, Example 12.7.4
(d1273 <- array(c(21L, 16L, 11L, 6L, 50L, 18L, 14L, 6L), dim = c(2L, 2L, 2L), dimnames = list(
  HTN = c('Present', 'Absent'), OCAD = c('Cases', 'Controls'), 
  Age = c('<=55', '>55'))))
addmargins(d1273, margin = 1:2) # Page 651, Table 12.7.6
mantelhaen.test(d1273)
