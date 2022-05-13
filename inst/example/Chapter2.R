library(DanielBiostatistics10th)
# To run a line of code, use shortcut
# Command + Enter: Mac and RStudio Cloud
# Control + Enter: Windows, Mac and RStudio Cloud
# To clear the console
# Control + L: Mac and RStudio Cloud

# To view the help files of functions in Chapter 2
# ?Chapter02

# Page 20, Example 2.2.1
d141 = read.csv(system.file('extdata', 'EXA_C01_S04_01.csv', package = 'DanielBiostatistics10th'))
head(d141)
class(age <- d141$AGE) # 'integer'
class(age <- as.numeric(d141$AGE)) # 'numeric'
sort(age) # 'ordered vector'

# Page 23, Example 2.3.1
hist(age) # to create a histogram
(ageB = seq.int(from = 30, to = 90, by = 10))
print_freqs(age, breaks = ageB, include.lowest = TRUE, right = FALSE) # Page 25, Table 2.3.2
# `include.lowest` actually implies 'include.highest' when `right = FALSE`
if (FALSE) {
  # optional: read ?base::cut to understand `include.lowest` and `right`
  print_freqs(x = age, breaks = ageB, include.lowest = FALSE, right = FALSE)
  print_freqs(x = age, breaks = ageB, include.lowest = TRUE, right = TRUE)
  print_freqs(x = age, breaks = ageB, include.lowest = FALSE, right = TRUE)
}



# Page 31, Exercise 2.3.2 
e232_0 = read.csv(system.file('extdata', 'EXR_C02_S03_02.csv', 
                              package = 'DanielBiostatistics10th'))
head(e232_0)
hist(e232 <- e232_0$sizes)
print_freqs(e232, breaks = seq(from = 0, to = 30, by = 5), include.lowest = TRUE, right = FALSE)
# I determine the open/close of interval ends by textbook question 
#  .. phrased as '10 and 14.9 inclusive', which indicates [10, 15)
# Use this table to answer question (b)-(e)
# (b) 15.72%  
# (c) 95.60%
# (d) 2.52%
# (e) 1 - 15.72% - 7.55%



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

