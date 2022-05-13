library(DanielBiostatistics10th)
# Put your blinking cursor at one line of your R code
# To run this line of code, use shortcut
# Command + Enter: Mac and RStudio Cloud
# Control + Enter: Windows, Mac and RStudio Cloud
# To clear the console
# Control + L: Mac and RStudio Cloud

# To view the help files of functions in Chapter 1
# ?Chapter01

# Page 8, Example 1.4.1
d141 = read.csv(system.file('extdata', 'EXA_C01_S04_01.csv', package = 'DanielBiostatistics10th'))
# to read in the data for Example 1.4.1, as provided by Publisher in .csv format
# ?read.csv # invoke the help files of an R 'function'
class(d141) # `d141` is a 'data.frame' (a specific class defined in R)
dim(d141) # dimension, number-row and number-column
head(d141, n = 8L) # first `n` rows of a 'data.frame'
names(d141) # column names of a 'data.frame'
d141$AGE # use `$` to obtain one column from a 'data.frame' 
sampleRow(d141, size = 10L, replace = FALSE) # to answer Example 1.4.1
if (FALSE) {
  # understand ?base::class of an object
  class(1) # 'numeric', not 'integer'
  class(1.2) # 'numeric', not 'integer'
  class(1L) # 'integer'
  class(TRUE) # 'logical'
  class(FALSE) # 'logical'
  tryCatch(class(false), error = identity) # error
}
if (FALSE) {
  # two ways of assigning a value to a variable
  ?`=` # casual assignment
  suppressWarnings(rm(x)); x = 1; x 
  ?`<-` # stringent assignment
  suppressWarnings(rm(y)); y <- 2; y 
  ?`==` # NOT assignment
  x <- 1; x == 2 # equal or not
}


# Page 11, Example 1.4.2 (systematic sample)
d141[seq.int(from = 4L, to = 166L, by = 18L), ]
