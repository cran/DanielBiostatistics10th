library(DanielBiostatistics10th)
# To run a line of code, use shortcut
# Command + Enter: Mac and RStudio Cloud
# Control + Enter: Windows, Mac and RStudio Cloud
# To clear the console
# Control + L: Mac and RStudio Cloud

# To view the help files of functions in Chapter 5-7
# ?Chapter05to07


# Page 142, Example 5.3.2
aggregated_z_test(xbar = 190, sd = 12.7, n = 10L, null.value = 185.6, alternative = 'greater')
# Page 143, Example 5.3.3
pnorm(125, mean = 120, sd = 15/sqrt(50)) - pnorm(115, mean = 120, sd = 15/sqrt(50))
 
# Page 145, Example 5.4.1
aggregated_z_test(xbar = c(92, 105), sd = 20, n = 15L, alternative = 'less') 
# Page 148, Example 5.4.2
aggregated_z_test(xbar = 20, sd = c(15, 20), n = c(35L, 40L), null.value = 45-30, 
  alternative = 'greater')

# Page 150, Example 5.5.1
prop_test_CLT(xbar = .4, n = 150L, prob = .357, alternative = 'greater')
# Page 152, Example 5.5.2
prop_test_CLT(xbar = .45, n = 200L, prob = .51, alternative = 'less')

# Page 155, Example 5.6.1
prop_test_CLT(xbar = .1, prob = c(.28, .21), n = 100L, alternative = 'greater')
# Page 155, Example 5.6.2
prop_test_CLT(xbar = .05, prob = c(.34, .26), n = c(250L, 200L), alternative = 'less')

# Page 166, Example 6.2.1
aggregated_z_test(xbar = 22, n = 10L, sd = sqrt(45))
# Page 168, Example 6.2.2
aggregated_z_test(xbar = 84.3, n = 15L, sd = sqrt(144), conf.level = .99)
# Page 168, Example 6.2.3
aggregated_z_test(xbar = 17.2, n = 35L, sd = 8, conf.level = .9)
# Page 169, Example 6.2.4
dat_624 = read.csv(system.file('extdata', 'EXA_C06_S02_04.csv', 
  package = 'DanielBiostatistics10th'))
head(dat_624)
aggregated_z_test(xbar = mean(dat_624$ACTIVITY), n = nrow(dat_624), sd = sqrt(.36))

# Page 173, Example 6.3.1
aggregated_t_test(xbar = 250.8, xsd = 130.9, n = 19L)

# Page 177, Example 6.4.1
aggregated_z_test(xbar = c(4.5, 3.4), sd = sqrt(c(1, 1.5)), n = c(12L, 15L))
# Page 178, Example 6.4.2
aggregated_z_test(xbar = c(4.3, 13), sd = c(5.22, 8.97), n = c(328L, 64L), conf.level = .99)
# Page 180, Example 6.4.3
aggregated_t_test(xbar = c(4.7, 8.8), xsd = c(9.3, 11.5), n = c(18L, 10L), var.equal = TRUE)
# Page 181, Example 6.4.4
aggregated_t_test(xbar = c(4.7, 8.8), xsd = c(9.3, 11.5), n = c(18L, 10L)) 
# Welch slightly different from Cochran; textbook explained on Page 182

# Page 185, Example 6.5.1
prop_test_CLT(xbar = .18, n = 1220L)

# Page 187, Example 6.6.1
prop_test_CLT(x = c(31L, 53L), n = c(68L, 255L), conf.level = .99)
prop.test(x = c(31L, 53L), n = c(68L, 255L), conf.level = .99, correct = FALSE)

# Page 190, Example 6.7.1
n_671 = uniroot(f = function(n, sd, level = .95) {
  qnorm(1-(1-level)/2) * sd/sqrt(n) - 5 # half-width of CI <= 5 grams
}, interval = c(0, 2e2), sd = 20
)
sprintf('Example 6.7.1 requires a sample size of %d.', ceiling(n_671$root))

# Page 192, Example 6.8.1
n_681 = uniroot(f = function(n, p, level = .95) {
  qnorm(1-(1-level)/2) * sqrt(p*(1-p)/n) - .05
}, interval = c(0, 1e3), p = .35)
sprintf('Example 6.8.1 requires a sample size of %d.', ceiling(n_681$root))

# Page 196, Example 6.9.1
d691 = c(9.7, 12.3, 11.2, 5.1, 24.8, 14.8, 17.7)
sqrt(aggregated_var_test(xsd = sd(d691), n = length(d691))$conf.int)

# Page 200, Example 6.10.1
aggregated_var_test(xsd = c(8.1, 5.9), n = c(16L, 4L))

# Page 222, Example 7.2.1
aggregated_z_test(xbar = 27, sd = sqrt(20), n = 10L, null.value = 30)
# Page 226, Example 7.2.2
aggregated_z_test(xbar = 27, sd = sqrt(20), n = 10L, null.value = 30, alternative = 'less')
# Page 228, Example 7.2.3
head(d723 <- read.csv(system.file('extdata', 'EXA_C07_S02_03.csv', 
  package = 'DanielBiostatistics10th')))
t.test(d723$DAYS, mu = 15)
# Page 231, Example 7.2.4
aggregated_z_test(xbar = 146, sd = 27, n = 157L, null.value = 140, alternative = 'greater')
# Page 232, Example 7.2.5
d725 = c(33.38, 32.15, 34.34, 33.95, 33.46, 34.13, 33.99, 34.10, 33.85, 
  34.23, 34.45, 34.19, 33.97, 32.73, 34.05)
t.test(d725, mu = 34.5)

# Page 237, Example 7.3.1
aggregated_z_test(xbar = c(4.5, 3.4), sd = sqrt(c(1, 1.5)), n = c(12L, 15L))
# Page 239, Example 7.3.2
head(d732 <- read.csv(system.file('extdata', 'EXA_C07_S03_02.csv', 
  package = 'DanielBiostatistics10th')))
with(d732, t.test(x = CONTROL, y = SCI, alternative = 'less', var.equal = TRUE))
# Page 240, Example 7.3.3
aggregated_t_test(xbar = c(19.16, 9.53), xsd = c(5.29, 2.69), n = c(15L, 30L))
# Page 242, Example 7.3.4
aggregated_z_test(xbar = c(59.01, 46.61), sd = c(44.89, 34.85), n = c(53L, 54L), 
  alternative = 'greater')

# Page 251, Example 7.4.1
head(d741 <- read.csv(system.file('extdata', 'EXA_C07_S04_01.csv', 
  package = 'DanielBiostatistics10th')))
with(d741, t.test(x = POSTOP, y = PREOP, alternative = 'greater', paired = TRUE))

# Page 258, Example 7.5.1
prop_test_CLT(x = 24L, n = 301L, prob = .063, alternative = 'greater') 

# Page 261, Example 7.6.1
prop_test_CLT(x = c(24L, 11L), n = c(44L, 29L), p.equal = TRUE, alternative = 'greater')

# Page 264, Example 7.7.1
aggregated_var_test(xsd = sqrt(670.81), n = 16L, null.value = 600)

# Page 268, Example 7.8.1
aggregated_var_test(xsd = c(30.62, 11.37), n = 6L, alternative = 'greater')
# Page 270, Example 7.8.2
with(d732, var.test(x = CONTROL, y = SCI))
