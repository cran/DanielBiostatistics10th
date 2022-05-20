library(DanielBiostatistics10th)
# To run a line of code, use shortcut
# Command + Enter: Mac and RStudio Cloud
# Control + Enter: Windows, Mac and RStudio Cloud
# To clear the console
# Control + L: Mac and RStudio Cloud

# To view the help files
# ?Chapter07_power


# Page 272, Example 7.9.1
(m791 = seq.int(from = 16, to = 19, by = .5)) # Page 275, Table 7.9.1, 1st column
power_z(m791, null.value = 17.5, sd = 3.6, n = 100L, alternative = 'two.sided')
  # Page 275, Table 7.9.1, column "1-beta"
curve(expr = power_z(x, null.value = 17.5, sd = 3.6, n = 100L, alternative = 'two.sided'), 
  from = 15, to = 20, main = 'Page 275, Figure 7.9.2',
  ylab = 'Power curve for Example 7.9.1.', xlab = 'Alternative values of \u03bc')

# Page 276, Example 7.9.2
# To mimic the set up of Page 275, Table 7.9.1, calculate the power at c(50, 55, 60, 65)
m792 = c(50, 55, 60, 65)
power_z(m792, null.value = 65, sd = 15, n = 20L, sig.level = .01, alternative = 'less')
curve(expr = power_z(x, null.value = 65, sd = 15, n = 20L, sig.level = .01, alternative = 'less'), 
  from = 49, to = 66, main = 'Page 276, Figure 7.9.4',
  ylab = 'Power curve for Example 7.9.2.', xlab = 'Alternative values of \u03bc')
  
# for testing H0: mu <= 65 vs. Ha: mu > 65
m792_g = c(65, 70, 75, 80) 
power_z(m792_g, null.value = 65, sd = 15, n = 20L, sig.level = .01, alternative = 'greater')
curve(power_z(x, null.value = 65, sd = 15, n = 20L, sig.level = .01, alternative = 'greater'), 
  from = 63, to = 80, 
  ylab = 'Power curve for Example 7.9.2(a) ', xlab = 'Alternative values of \u03bc')
      
# Page 278, Example 7.10.1
# Textbook requires Pr(Fail to reject H0) = beta = .05, which is equivalent to power = .95
# The sample size calculated is n = 35.55 (Page 279), so the actual sample size required is 36
power_z(55, null.value = 65, sd = 15, n = 36L, sig.level = .01, alternative = 'less')
power_z(55, null.value = 65, sd = 15, n = 35L, sig.level = .01, alternative = 'less')
# rejection region (-Inf, 59.184) is equivalent to the textbook results (two `C` on Page 279)

# Why ?stats::power.t.test gives different answer?
power.t.test(delta = abs(55-65), sd = 15, sig.level = .01, power = .95, 
  type = 'one.sample', alternative = 'one.sided')
# Textbook result uses z-test (i.e., population sd is 15)
# while ?stats::power.t.test uses t-test.   
# There is no built in R function to do power analysis based on z-test, 
# because in reality we seldom know the population sd.

