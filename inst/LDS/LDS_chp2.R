
# Page 62, Large Data Set `NCBIRTH800`

head(NCBIRTH800_raw <- read.csv(system.file(
  'extdata', 'LDS_C02_NCBIRTH800.csv', 
  package = 'DanielBiostatistics10th')))
dim(NCBIRTH800_raw)

NCBIRTH800 = within(NCBIRTH800_raw, expr = {
  sex = structure(sex, levels = c('male', 'female'), class = 'factor')
  marital = structure(marital, levels = c('married', 'not married'), class = 'factor')
  smoke = structure(smoke + 1L, levels = c('not smoked', 'smoked'), class = 'factor')
  premie = structure(premie + 1L, levels = c('not premature', 'premature'), class = 'factor')
  low = structure(low + 1L, levels = c('not low', 'low'), class = 'factor')
  racemom = structure(racemom + 1L, levels = c('Other_non_white', 'White', 'Black', 'Indian', 'Chinese', 'Japanese', 'Hawaiian', 'Filipino', 'Other_Asian_PacificIs'), class = 'factor')
})

# the benefit of using 'factor' instead of 'integer'
table(NCBIRTH800_raw$sex)
table(NCBIRTH800$sex)

# For the variables of MAGE, WEEKS, GAINED, TOUNCES, and TGRAMS

# 1. Calculate the mean, median, standard deviation, IQR, and range.
# 2. For each, construct a histogram and comment on the shape of the distribution.
# 7. Calculate the skewness and kurtosis of the data set. What do they indicate?

# Learn how R function works
print_stats(NCBIRTH800$mage)
print_stats(NCBIRTH800$weeks)
print_stats(NCBIRTH800$gained)
print_stats(NCBIRTH800$tounces)
print_stats(NCBIRTH800$tgrams)

# 3. Do the histograms for TOUNCES and TGRAMS look strikingly similar? Why?
range(NCBIRTH800$tounces / NCBIRTH800$tgrams)
range(with(NCBIRTH800, tounces / tgrams)) # ?with

# 4. Construct box-and-whisker plots for all four variables.
boxplot(NCBIRTH800$mage, main = 'mage')
boxplot(NCBIRTH800$weeks, main = 'weeks')
boxplot(NCBIRTH800$gained, main = 'gained')
boxplot(NCBIRTH800$tounces, main = 'tounces')

# 5. Construct side-by-side box-and-whisker plots for the variable of TOUNCES for women 
# who admitted to smoking and women who did not admit to smoking. 
# Do you see a difference in birth weight in the two groups? Which group has more variability?
boxplot(tounces ~ smoke, data = NCBIRTH800)

# 6. Construct side-by-side box-and-whisker plots for the variable of MAGE for women 
# who are and are not married. Do you see a difference in ages in the two groups? 
# Which group has more variability? Are the results surprising?
boxplot(mage ~ marital, data = NCBIRTH800)



# Page 90, Chapter 3

addProbs(table(NCBIRTH800[c('premie', 'smoke')]))

addProbs(table(NCBIRTH800[c('low', 'marital')]))



# Page 210, Chapter 6

with(NCBIRTH800, prop_test_CLT(x = sum(sex == 'male'), n = length(sex))) # 1(a)
t.test(NCBIRTH800$mage) # 1(b)
t.test(NCBIRTH800$gained) # 1(c)
with(NCBIRTH800, prop_test_CLT(x = sum(smoke == 'smoked', na.rm = TRUE), n = length(smoke))) # 1(d)
t.test(gained ~ smoke, data = NCBIRTH800) # 1(e)
t.test(tounces ~ marital, data = NCBIRTH800) # 1(f)

with(NCBIRTH800, table(low, marital))
prop_test_CLT(x = 35L, n = c(537L, 263L)) # 1(g)



