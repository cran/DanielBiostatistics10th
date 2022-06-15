library(DanielBiostatistics10th)
# To run a line of code, use shortcut
# Command + Enter: Mac and RStudio Cloud
# Control + Enter: Windows, Mac and RStudio Cloud
# To clear the console
# Control + L: Mac and RStudio Cloud

library(BSDA)
library(robslopes)

# Page 673, Example 13.3.1
d1331 = c(4, 5, 8, 8, 9, 6, 10, 7, 6, 6)
BSDA::SIGN.test(d1331, md = 5)

# Page 677, Example 13.3.2
d1332 = read.csv(system.file('extdata', 'EXA_C13_S03_02.csv', package = 'DanielBiostatistics10th'))
head(d1332)
with(d1332, BSDA::SIGN.test(x = X, y = Y, alternative = 'less'))

# Page 683, Example 13.4.1
d1341 = c(4.91, 4.10, 6.74, 7.27, 7.42, 7.50, 6.56, 4.64, 5.98, 3.14, 3.23, 5.80, 6.17, 5.39, 5.77)
wilcox.test(d1341, mu = 5.05)

# Page 686, Example 13.5.1
d1351 = read.csv(system.file('extdata', 'EXA_C13_S05_01.csv', package = 'DanielBiostatistics10th'))
head(d1351)
(med1351 = median(unlist(d1351), na.rm = TRUE)) # common median
(t1351 = with(d1351, expr = {
  tmp <- cbind(Urban = table(URBAN < med1351), Rural = table(RURAL < med1351, useNA = 'no'))
  rownames(tmp) <- paste('Number of scores', c('above', 'below'), 'median')
  tmp
})) # Page 688, Table 13.5.2
chisq.test(t1351, correct = FALSE)

# Page 691, Example 13.6.1
d1361 = read.csv(system.file('extdata', 'EXA_C13_S06_01.csv', package = 'DanielBiostatistics10th'))
head(d1361)
with(d1361, wilcox.test(X, Y, exact = FALSE, alternative = 'less'))

# Page 699, Example 13.7.1
d1371 = read.csv(system.file('extdata', 'EXA_C13_S07_01.csv', package = 'DanielBiostatistics10th'))
head(d1371)
ks.test(d1371$GLUCOSE, y = pnorm, mean = 80, sd = 6)

# Page 705, Example 13.8.1
d1381 = data.frame(Eosinophil = c(5, 9, 8, 11, 14, 2, 3, 4, 6, 1, 13, 7, 15, 12, 10),
                   Exposure = rep(c('Air', 'Benzaldehyde', 'Acetaldehyde'), each = 5L))
with(d1381, kruskal.test(x = Eosinophil, g = Exposure))

# Page 708, Example 13.8.2
d1382 = read.csv(system.file('extdata', 'EXA_C13_S08_02.csv', package = 'DanielBiostatistics10th'))
head(d1382)
kruskal.test(x = unlist(d1382), g = rep(names(d1382), each = 10))

# Page 713, Example 13.9.1
d1391 = read.csv(system.file('extdata', 'EXA_C13_S09_01.csv', package = 'DanielBiostatistics10th'))
head(d1391)
(m1391 = matrix(as.matrix(d1391[LETTERS[1:3]]), ncol = 3L,
               dimnames = list(d1391$THERAPIST, LETTERS[1:3])))
friedman.test(m1391)

# Page 715, Example 13.9.2
d1392 = read.csv(system.file('extdata', 'EXA_C13_S09_02.csv', package = 'DanielBiostatistics10th'))
head(d1392)
(m1392 = matrix(as.matrix(d1392[LETTERS[1:4]]), ncol = 4L,
                dimnames = list(d1392$ANIMAL, LETTERS[1:4])))
friedman.test(m1392)

# Page 720, Example 13.10.1
d13101 = read.csv(system.file('extdata', 'EXA_C13_S10_01.csv', package = 'DanielBiostatistics10th'))
head(d13101)
with(d13101, cor.test(X, Y, method = 'spearman'))

# Page 722, Example 13.10.2
d13102 = read.csv(system.file('extdata', 'EXA_C13_S10_02.csv', package = 'DanielBiostatistics10th'))
head(d13102)
with(d13102, cor.test(V2, V3, method = 'spearman', exact = FALSE))

# Page 728, Example 13.11.1
# Page 729, Example 13.11.2
d13111 = data.frame(Testosterone = c(230, 175, 315, 290, 275, 150, 360, 425),
                    CitricAcid = c(421, 278, 618, 482, 465, 105, 550, 750))
with(d13111, robslopes::TheilSen(x = CitricAcid, y = Testosterone))
# textbook uses *central* median of ordered pairs, while ?robslopes::TheilSen uses *upper* median
