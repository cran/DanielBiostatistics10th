library(DanielBiostatistics10th)
# To run a line of code, use shortcut
# Command + Enter: Mac and RStudio Cloud
# Control + Enter: Windows, Mac and RStudio Cloud
# To clear the console
# Control + L: Mac and RStudio Cloud




# Large Data Sets in Chapter 7, Page 299

# PCKDATA
head(PCKDATA <- read.csv(system.file(
  'extdata', 'LDS_C07_PCKDATA.csv', 
  package = 'DanielBiostatistics10th')))
dim(PCKDATA)

shapiro.test(PCKDATA$A)
shapiro.test(PCKDATA$B)

with(PCKDATA, t.test(A, B, paired = TRUE))
with(PCKDATA, t.test(A, B, alternative = 'greater', paired = TRUE))



# PROTHROM
head(PROTHROM <- read.csv(system.file(
  'extdata', 'LDS_C07_PROTHROM_corrected.csv', 
  package = 'DanielBiostatistics10th')))
# `header = FALSE` means 'the first row of csv file is NOT the variable names'
dim(PROTHROM)

boxplot(TIME ~ Group, data = PROTHROM)
t.test(TIME ~ Group, data = PROTHROM)



# HEADCIRC
head(HEADCIRC <- read.csv(system.file(
  'extdata', 'LDS_C07_HEADCIRC.csv', 
  package = 'DanielBiostatistics10th')))
dim(HEADCIRC)

boxplot(HEADCIRC$SCA - HEADCIRC$NC, main = 'Difference of SCA - NC')
t.test(HEADCIRC$SCA, HEADCIRC$NC, alternative = 'less', paired = TRUE)



# HEMOGLOB
head(HEMOGLOB <- read.csv(system.file(
  'extdata', 'LDS_C07_HEMOGLOB_corrected.csv', 
  package = 'DanielBiostatistics10th')))
dim(HEMOGLOB)

boxplot(Hb ~ Group, data = HEMOGLOB)
t.test(Hb ~ Group, data = HEMOGLOB)




# MANDEXT
head(MANDEXT <- read.csv(system.file(
  'extdata', 'LDS_C07_MANDEXT_corrected.csv', 
  package = 'DanielBiostatistics10th')))
dim(MANDEXT)
boxplot(SCORE ~ Learning, data = MANDEXT)
t.test(SCORE ~ Learning, data = MANDEXT, alternative = 'less')


# illustrating the use of ?base::subset
sLD = subset(MANDEXT, subset = (Group == 'LD')) # 'LD' means 'learning disabled'
dim(MANDEXT)
dim(sLD)
head(sLD)
# end of illustrating the use of ?base::subset

ids = c(sample(1:500, size = 10L, replace = FALSE), sample(501:1e3, size = 15L, replace = FALSE))
sp_MANDEXT = MANDEXT[ids, ]
dim(sp_MANDEXT)
t.test(SCORE ~ Group, data = sp_MANDEXT, alternative = 'less')


