library(DanielBiostatistics10th)
# To run a line of code, use shortcut
# Command + Enter: Mac and RStudio Cloud
# Control + Enter: Windows, Mac and RStudio Cloud
# To clear the console
# Control + L: Mac and RStudio Cloud

# Page 666, Question (Large Data Set 'SMOKING')
head(SMOKING_raw <- read.csv(system.file('extdata', 'LDS_C12_SMOKING.csv', 
                                         package = 'DanielBiostatistics10th')))
SMOKING = within(SMOKING_raw, expr = {
  Sex = structure(A + 1L, levels = c('Male', 'Female'), class = 'factor'); A = NULL
  Smoking = (B == 1L); B = NULL
  Drinking = structure(C + 1L, levels = c('None', 'Light', 'Heavy')); C = NULL
  Respiratory = (D == 1L); D = NULL
  HighBloodPres = (E == 1L); E = NULL
})