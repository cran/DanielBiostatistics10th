library(DanielBiostatistics10th)

# Page 93-97, Example 4.2.1 - Example 4.2.7
d421 = rep(1:8, times = c(62L, 47L, 39L, 39L, 58L, 37L, 4L, 11L))
(fq421 = print_freqs(factor(d421))) # Page 94, Table 4.2.1 and 4.2.2; Page 96, Table 4.2.3

# ?dbinom # 'd' for binomial 'density'; calculate Prob(X = x)
# ?pbinom # 'p' for binomial 'probability' 
# `lower.tail = TRUE` (default), calculate Prob(X <= x)
# `lower.tail = FALSE`, calculate Prob(X > x)

# Page 99, Example 4.3.1
dbinom(x = 3L, size = 5L, prob = .858)
# Page 103, Example 4.3.2
dbinom(x = 4L, size = 10L, prob = .14)
# Page 103, Example 4.3.3
(pL = pbinom(q = 5L, size = 25L, prob = .1, lower.tail = TRUE)) # (a) including!
(pU = pbinom(q = 5L, size = 25L, prob = .1, lower.tail = FALSE)) # (b) excluding!
pL + pU # R makes sure they add up to 1
# Page 105, Example 4.3.4
dbinom(x = 7L, size = 12L, prob = .55)
pbinom(q = 5L, size = 12L, prob = .55)
pbinom(q = 7L, size = 12L, prob = .55, lower.tail = FALSE)

# Page 110, Example 4.4.1
dpois(x = 3L, lambda = 12) 
# Page 110, Example 4.4.2
ppois(2L, lambda = 12, lower.tail = FALSE)
# Page 110, Example 4.4.3
ppois(1L, lambda = 2) 
# Page 111, Example 4.4.4
dpois(3L, lambda = 2)
# Page 112, Example 4.4.5
ppois(5L, lambda = 2, lower.tail = FALSE)

# Page 119. Example 4.6.1
pnorm(2)
# Page 120. Example 4.6.2
pnorm(2.55) - pnorm(-2.55)
1 - 2 * pnorm(-2.55) # alternative solution
# Page 121. Example 4.6.3
pnorm(1.53) - pnorm(-2.74)
# Page 121. Example 4.6.4
pnorm(2.71, lower.tail = FALSE)
# Page 122. Example 4.6.5
pnorm(2.45) - pnorm(.84)

# Page 122. Example 4.7.1
pnorm(q = 3, mean = 5.4, sd = 1.3)
pnorm(q = (3-5.4)/1.3) # manual solution
# Page 125. Example 4.7.2
pnorm(649, mean = 491, sd = 119) - pnorm(292, mean = 491, sd = 119)
# Page 122. Example 4.7.3
1e4L * pnorm(8.5, mean = 5.4, sd = 1.3, lower.tail = FALSE)
