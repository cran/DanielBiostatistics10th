library(DanielBiostatistics10th)
# To run a line of code, use shortcut
# Command + Enter: Mac and RStudio Cloud
# Control + Enter: Windows, Mac and RStudio Cloud
# To clear the console
# Control + L: Mac and RStudio Cloud

# To view the help files
# ?Chapter02

# Page 20, Example 2.2.1
d141 = read.csv(system.file('extdata', 'EXA_C01_S04_01.csv', package = 'DanielBiostatistics10th'))
head(d141)
class(d141$AGE) # 'integer'
class(age <- as.numeric(d141$AGE)) # 'numeric'
sort(age) # 'ordered vector'

# Page 23, Example 2.3.1
hist(age) # to create a histogram
(ageB = seq.int(from = 30, to = 90, by = 10))
print_freqs(age, breaks = ageB, include.lowest = TRUE, right = FALSE) # Page 25, Table 2.3.2
# `include.lowest` actually implies 'include.highest' when `right = FALSE`
if (FALSE) {
  # optional: read ?base::cut to understand `include.lowest` and `right`
  print_freqs(age, breaks = ageB, include.lowest = FALSE, right = FALSE)
  print_freqs(age, breaks = ageB, include.lowest = TRUE, right = TRUE)
  print_freqs(age, breaks = ageB, include.lowest = FALSE, right = TRUE)
}

# Page 31, Exercise 2.3.2 
e232 = read.csv(system.file('extdata', 'EXR_C02_S03_02.csv', package = 'DanielBiostatistics10th'))
head(e232)
hist(e232$sizes)
print_freqs(e232$sizes, breaks = seq(0, 30, by = 5), include.lowest = TRUE, right = FALSE)
# The open/close of interval ends is determined 
# .. by textbook question phrased as '10 and 14.9 inclusive', which indicates [10, 15)
# Class activity: use this table to answer question (b)-(e)

# Page 38, Example 2.4.1
# Page 39, Example 2.4.2
# Page 40, Example 2.4.3
# Page 40, Example 2.4.4
# Page 41, Example 2.4.5
# Page 42, Example 2.4.6
# Page 44, Example 2.5.1
# Page 44, Example 2.5.2
# Page 46, Example 2.5.3
print_stats(age) # or some other data input

# Page 49, Example 2.5.4 (omitted)

# Page 50, Example 2.5.5
d255 = read.csv(system.file('extdata', 'EXA_C02_S05_05.csv', package = 'DanielBiostatistics10th'))
head(d255)
boxplot(d255$GRF, main = c('GRF from Page 50, Example 2.5.5'))
print_stats(d255$GRF)
print_freqs(d255$GRF, breaks = seq.int(10, 45, by = 5), include.lowest = TRUE)
