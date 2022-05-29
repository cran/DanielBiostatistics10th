library(DanielBiostatistics10th)
# To run a line of code, use shortcut
# Command + Enter: Mac and RStudio Cloud
# Control + Enter: Windows, Mac and RStudio Cloud
# To clear the console
# Control + L: Mac and RStudio Cloud

# To view the help files
# ?Chapter07_power

# Page 272, Example 7.9.1
(p791 = power_z(seq.int(from = 16, to = 19, by = .5), null.value = 17.5, sd = 3.6, n = 100L, 
                alternative = 'two.sided')) # Page 275, Table 7.9.1
autoplot(p791, title = 'Page 275, Figure 7.9.2')

# Page 276, Example 7.9.2
# To mimic the set up of Page 275, Table 7.9.1, calculate the power at c(50, 55, 60, 65)
(p792 = power_z(c(50, 55, 60, 65), null.value = 65, sd = 15, n = 20L, sig.level = .01, 
                alternative = 'less'))
autoplot(p792, title = 'Page 277, Figure 7.9.4')

# Page 278, Example 7.10.1
# Textbook requires Pr(Fail to reject H0) = beta = .05, which is equivalent to power = .95
(n7101 <- uniroot(f = function(x) {
  power_z(55, null.value = 65, sd = 15, n = x, sig.level = .01, alternative = 'less') - .95
}, interval = c(0, 50))$root)
# Why ?stats::power.t.test gives different answer?
power.t.test(delta = abs(55-65), sd = 15, sig.level = .01, power = .95, 
  type = 'one.sample', alternative = 'one.sided')
# Textbook result uses z-test (i.e., population sd is 15)
# while ?stats::power.t.test uses t-test.   
# There is no built in R function to do power analysis based on z-test, 
# because in reality we seldom know the population sd.

# 1-sided vs. 2-sided test
autoplot(p792, all.alternative = TRUE, title = '1-sided vs. 2-sided test')

