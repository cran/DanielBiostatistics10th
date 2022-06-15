library(DanielBiostatistics10th)

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

# Page 11, Example 1.4.2
d141[seq.int(from = 4L, to = 166L, by = 18L), ]
