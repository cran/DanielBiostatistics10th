library(DanielBiostatistics10th)
# To run a line of code, use shortcut
# Command + Enter: Mac and RStudio Cloud
# Control + Enter: Windows, Mac and RStudio Cloud
# To clear the console
# Control + L: Mac and RStudio Cloud




# Page 666, Question (Large Data Set 'SMOKING')
head(SMOKING_raw <- read.csv(system.file('extdata', 'LDS_C12_SMOKING.csv', 
                                     package = 'DanielBiostatistics10th')))
# data and description do not match

# dont know what are the columns..  do not use this example