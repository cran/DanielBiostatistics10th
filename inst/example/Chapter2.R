library(DanielBiostatistics10th)

# Page 20, Example 2.2.1
d141 = read.csv(system.file('extdata', 'EXA_C01_S04_01.csv', package = 'DanielBiostatistics10th'))
head(d141)
class(d141$AGE) # 'integer'
class(age <- as.numeric(d141$AGE)) # 'numeric'
sort(age) # Page 21, Table 2.2.1 # 'ordered vector'

# Page 23, Example 2.3.1
(ageB = seq.int(from = 30, to = 90, by = 10))
(r231 = print_freqs(age, breaks = ageB, right = FALSE)) # Page 25, Table 2.3.2
# The open/close of interval ends is determined by textbook using 30-39, 40-49, etc.
autoplot(r231, title = 'Page 27, Figure 2.3.2')

# Page 38-42, Example 2.4.1 - Example 2.4.6
# Page 44-46, Example 2.5.1 - Example 2.5.3
print_stats(age) # or some other data input

# Page 49, Example 2.5.4 (omitted)

# Page 50, Example 2.5.5
d255 = read.csv(system.file('extdata', 'EXA_C02_S05_05.csv', package = 'DanielBiostatistics10th'))
head(d255)
boxplot(d255$GRF, main = c('GRF from Page 50, Example 2.5.5'))
print_stats(d255$GRF)
print_freqs(d255$GRF, breaks = seq.int(10, 45, by = 5))
