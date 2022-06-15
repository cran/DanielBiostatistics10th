library(DanielBiostatistics10th)

# Page 272, Example 7.9.1
(p791 = power_z(seq.int(from = 16, to = 19, by = .5), null.value = 17.5, sd = 3.6, n = 100L, 
                alternative = 'two.sided')) # Page 275, Table 7.9.1
autoplot(p791, title = 'Page 275, Figure 7.9.2')

# Page 276, Example 7.9.2
(p792 = power_z(c(50, 55, 60, 65), null.value = 65, sd = 15, n = 20L, sig.level = .01, 
                alternative = 'less'))
autoplot(p792, title = 'Page 277, Figure 7.9.4')
autoplot(p792, all.alternative = TRUE, title = '1-sided vs. 2-sided test')

# Page 278, Example 7.10.1
(n7101 <- uniroot(f = function(x) {
  power_z(55, null.value = 65, sd = 15, n = x, sig.level = .01, alternative = 'less') - .95
}, interval = c(0, 50))$root)
