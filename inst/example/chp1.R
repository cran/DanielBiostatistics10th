library(DanielBiostatistics10th)

# Page 8, Example 1.4.1
d141 = read.csv(system.file('extdata', 'EXA_C01_S04_01.csv', package = 'DanielBiostatistics10th'))
class(d141) # 'data.frame'; most used R object to store a 'data'
dim(d141)
head(d141, n = 8L) # first `n` rows of a 'data.frame'
names(d141) # column names of a 'data.frame'
d141$AGE
sampleRow(d141, size = 10L, replace = FALSE)

# Page 11, Example 1.4.2 (systematic sample)
d141[seq.int(from = 4L, to = 166L, by = 18L), ]

