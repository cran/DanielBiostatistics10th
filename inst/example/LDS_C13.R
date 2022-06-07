library(DanielBiostatistics10th)
# To run a line of code, use shortcut
# Command + Enter: Mac and RStudio Cloud
# Control + Enter: Windows, Mac and RStudio Cloud
# To clear the console
# Control + L: Mac and RStudio Cloud

# Page 747, Question 1 (Large Data Set 'KLETTER')
head(KLETTER <- read.csv(system.file('extdata', 'LDS_C13_KLETTER.csv', 
                                     package = 'DanielBiostatistics10th')))
