library(DanielBiostatistics10th)

# Page 272, Example 7.9.1
(p791 = power_z(seq.int(from = 16, to = 19, by = .5), null.value = 17.5, sd = 3.6, n = 100L))
# Page 275, Table 7.9.1
autoplot(p791) + labs(title = 'Page 275, Figure 7.9.2')

# Page 276, Example 7.9.2
(p792 = power_z(seq.int(from = 50, to = 70, by = 5), null.value = 65, sd = 15, n = 20L, 
                sig.level = .01, alternative = 'less'))
autoplot(p792) + labs(title = 'Page 277, Figure 7.9.4')

# Page 278, Example 7.10.1
(n_d7101 <- uniroot(f = function(x) {
  power_z(55, null.value = 65, sd = 15, n = x, sig.level = .01, alternative = 'less') - .95
}, interval = c(0, 50))$root)
(C_d7101 = qnorm(p = .01, mean = 65, sd = 15/sqrt(ceiling(n_d7101)), lower.tail = TRUE))
