library(DanielBiostatistics10th)
# To run a line of code, use shortcut
# Command + Enter: Mac and RStudio Cloud
# Control + Enter: Windows, Mac and RStudio Cloud
# To clear the console
# Control + L: Mac and RStudio Cloud

# To view the help files of functions in Chapter 3
# ?Chapter03

# Page 69, Example 3.4.1
# Page 70, Example 3.4.2
# Page 71, Example 3.4.3, Example 3.4.4
# Page 72, Example 3.4.5
# Page 73, Example 3.4.6
# Page 74, Example 3.4.7
# Page 75, Example 3.4.8
(d341 = matrix(c(28L, 19L, 41L, 53L, 35L, 38L, 44L, 60L), ncol = 2L, dimnames = list(
  FamilyHx = c('none', 'Bipolar', 'Unipolar', 'UniBipolar'), 
  Onset = c('Early', 'Late'))))
class(d341) # 'matrix', i.e., a two-dimensional 'array'
addProbs(d341)

# Page 81, Example 3.5.1
(d351 = matrix(c(436L, 14L, 5L, 495L), nrow = 2L, dimnames = list(
  Test = c('Positive', 'Negative'), Alzheimer = c('Yes', 'No'))))
predictiveValues(d351, prevalence = .113)

# toy example
predictiveValues(d351, prevalence = c(.005, .98))


