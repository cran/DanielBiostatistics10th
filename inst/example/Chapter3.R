library(DanielBiostatistics10th)

# Page 69-75, Example 3.4.1 - Example 3.4.8
(d341 = matrix(c(28L, 19L, 41L, 53L, 35L, 38L, 44L, 60L), ncol = 2L, dimnames = list(
  FamilyHx = c('none', 'Bipolar', 'Unipolar', 'UniBipolar'), 
  Onset = c('Early', 'Late'))))
class(d341) # 'matrix', i.e., a two-dimensional 'array'
addProbs(d341)
addProbs(d341, margin = 1L)
addProbs(d341, margin = 2L)

# Page 81, Example 3.5.1
(d351 = matrix(c(436L, 14L, 5L, 495L), nrow = 2L, dimnames = list(
  Test = c('Positive', 'Negative'), Alzheimer = c('Yes', 'No'))))
summary(BooleanTable(t(d351)), prevalence = .113)

