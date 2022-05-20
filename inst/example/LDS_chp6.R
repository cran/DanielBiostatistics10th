library(DanielBiostatistics10th)
# To run a line of code, use shortcut
# Command + Enter: Mac and RStudio Cloud
# Control + Enter: Windows, Mac and RStudio Cloud
# To clear the console
# Control + L: Mac and RStudio Cloud



# Large Data Sets in Chapter 6, Page 210

# CHOLEST
head(CHOLEST <- read.csv(system.file('extdata', 'LDS_C06_CHOLEST.csv', 
  package = 'DanielBiostatistics10th')))
dim(CHOLEST)

(sp15 = sample(CHOLEST$CHOLEST, size = 15L)); t.test(sp15) # Question 2

(sp50 = sample(CHOLEST$CHOLEST, size = 50L))
prop_CLT(x = sum(sp50 > 225), n = length(sp50)) # Question 3





# BABYWGTS
head(BABYWGTS <- read.csv(system.file('extdata', 'LDS_C06_BABYWGTS.csv', 
                                      package = 'DanielBiostatistics10th')))
dim(BABYWGTS)

(sp20 = sample(BABYWGTS$WGT, size = 20L)); t.test(sp20) # Question 4

(sp35 = sample(BABYWGTS$WGT, size = 35L)); t.test(sp35) # Question 5



# BOYHGTS

head(BOYHGTS <- read.csv(system.file(
  'extdata', 'LDS_C06_BOYHGTS.csv', 
  package = 'DanielBiostatistics10th')))
dim(BOYHGTS)

(sp15 = sample(BOYHGTS$HGT, size = 15L)); t.test(sp15, conf.level = .99) # Question 6

(sp35 = sample(BOYHGTS$HGT, size = 35L)); t.test(sp35, conf.level = .99) # Question 7




