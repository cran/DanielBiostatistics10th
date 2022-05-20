library(DanielBiostatistics10th)
# To run a line of code, use shortcut
# Command + Enter: Mac and RStudio Cloud
# Control + Enter: Windows, Mac and RStudio Cloud
# To clear the console
# Control + L: Mac and RStudio Cloud

# Page 62, Large Data Set `NCBIRTH800`

NCBIRTH <- read.csv(system.file('extdata', 'LDS_C02_NCBIRTH800.csv', 
                                package = 'DanielBiostatistics10th'))
dim(NCBIRTH)
head(NCBIRTH)

NCBIRTHa = within(NCBIRTH, expr = {
  sex = structure(sex, levels = c('male', 'female'), class = 'factor')
  marital = structure(marital, levels = c('married', 'not married'), class = 'factor')
  smoke = structure(smoke + 1L, levels = c('not smoked', 'smoked'), class = 'factor')
  premie = structure(premie + 1L, levels = c('not premature', 'premature'), class = 'factor')
  low = structure(low + 1L, levels = c('not low', 'low'), class = 'factor')
  racemom = structure(racemom + 1L, levels = c('Other_non_white', 'White', 'Black', 'Indian', 'Chinese', 'Japanese', 'Hawaiian', 'Filipino', 'Other_Asian_PacificIs'), class = 'factor')
})

# the benefit of using 'factor' instead of 'integer'
table(NCBIRTH$sex)
table(NCBIRTHa$sex)

# For the variables of MAGE, WEEKS, GAINED, TOUNCES, and TGRAMS

# 1. Calculate the mean, median, standard deviation, IQR, and range.
# 2. For each, construct a histogram and comment on the shape of the distribution.
# 7. Calculate the skewness and kurtosis of the data set. What do they indicate?

# Learn how R function works
print_stats(NCBIRTHa$mage)
print_stats(NCBIRTHa$weeks)
print_stats(NCBIRTHa$gained)
print_stats(NCBIRTHa$tounces)
print_stats(NCBIRTHa$tgrams)

# 3. Do the histograms for TOUNCES and TGRAMS look strikingly similar? Why?
range(NCBIRTHa$tounces / NCBIRTHa$tgrams)
range(with(NCBIRTHa, tounces / tgrams)) # ?with

# 4. Construct box-and-whisker plots for all four variables.
boxplot(NCBIRTHa$mage, main = 'mage')
boxplot(NCBIRTHa$weeks, main = 'weeks')
boxplot(NCBIRTHa$gained, main = 'gained')
boxplot(NCBIRTHa$tounces, main = 'tounces')

# 5. Construct side-by-side box-and-whisker plots for the variable of TOUNCES for women 
# who admitted to smoking and women who did not admit to smoking. 
# Do you see a difference in birth weight in the two groups? Which group has more variability?
boxplot(tounces ~ smoke, data = NCBIRTHa)

# 6. Construct side-by-side box-and-whisker plots for the variable of MAGE for women 
# who are and are not married. Do you see a difference in ages in the two groups? 
# Which group has more variability? Are the results surprising?
boxplot(mage ~ marital, data = NCBIRTHa)



# Page 90, Chapter 3
addProbs(table(NCBIRTHa[c('premie', 'smoke')]))
addProbs(table(NCBIRTHa[c('low', 'marital')]))



# Page 210, Chapter 6

with(NCBIRTHa, prop_CLT(x = sum(sex == 'male'), n = length(sex))) # 1(a)
t.test(NCBIRTHa$mage) # 1(b)
t.test(NCBIRTHa$gained) # 1(c)
with(NCBIRTHa, prop_CLT(x = sum(smoke == 'smoked', na.rm = TRUE), n = length(smoke))) # 1(d)
t.test(gained ~ smoke, data = NCBIRTHa) # 1(e)
t.test(tounces ~ marital, data = NCBIRTHa) # 1(f)

with(NCBIRTHa, table(low, marital))
prop_CLT(x = 35L, n = c(537L, 263L)) # 1(g)



