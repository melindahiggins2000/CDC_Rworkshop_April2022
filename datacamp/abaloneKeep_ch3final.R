# ==================================================
# quick checks - after removing factor exercises
# moving them into ch 4
# ==================================================

# Load the abalone dataset using readr
library(readr)
abalone <- readr::read_csv("http://assets.datacamp.com/production/repositories/2299/datasets/5a80498a0bba2da70fd4af7764c3ab4e71e32dda/abalone.csv")

# Load dplyr package
library(dplyr)
library(ggplot2)

# Create abaloneMod from abalone and add new variable age equal to rings + 1.5
# Add 3 more variables to abaloneMod pctShucked pctViscera pctShell for each as percent of wholeWeight
# Add new character variable agecat to abaloneMod labeled as "< 10.5" and "10.5 and older"
# Add new logical variable adult to abaloneMod for sex not equal to "I"
# Add new variable sexAF as a factor of sex to abaloneMod
# Add new sexF variable as a factor to abaloneMod with levels "F" "I" "M" and labels "Female" "Infant" "Male"
# Add ordered factor sexOF variable to abaloneMod with levels in order "I" "F" "M"; labels "Infant" "Female" "Male"
abaloneMod <- abalone %>%
  mutate(age = rings + 1.5) %>%
  mutate(pctShucked = shuckedWeight * 100 / wholeWeight) %>%
  mutate(pctViscera = visceraWeight * 100 / wholeWeight) %>%
  mutate(pctShell = shellWeight * 100 / wholeWeight) %>%
  mutate(agecat = ifelse(test = age < 10.5, 
                         yes = "< 10.5", 
                         no = "10.5 and older")) %>%
  mutate(adult = sex != "I")

# Keep cases with height > 0
# Keep cases where shuckedWeight is less than wholeWeight
# Keep cases where length is > both height and diameter
abaloneKeep <- abaloneMod %>%
  filter(height > 0) %>%
  filter(shuckedWeight < wholeWeight) %>%
  filter((length > height) & (length > diameter))

# ==================================================
# davis dataset for examples
# ==================================================

## use davis dataset for ch3

library(car)
head(Davis)

# readr::write_csv(Davis, "davis.csv")

davismod <- Davis %>%
  mutate(bmi = weight / ((height/100))^2) %>%
  mutate(diffht = repht - height) %>%
  mutate(difflow = diffht <= -3) %>%
  mutate(bmicat = 
           ifelse(test = bmi < 25,
                  yes = "1. underwt/norm",
                  no = ifelse(bmi < 30,
                              "2. overwt",
                              "3. obese"))) %>%
  mutate(bmigt25 = ifelse(bmi > 25,
                          "2. overwt/obese",
                          "1. underwt/norm"))

daviskeep <- davismod %>% 
  filter(bmi < 100)

davismod %>% arrange(bmi) %>% tail()
daviskeep %>% arrange(bmi) %>% tail()

# ==================================================

# ==================================================
# summary stats
# ==================================================
# load the Hmisc package
library(Hmisc)

# run describe() function from Hmisc package to get descriptive stats for abalone sex, length, diameter, height
daviskeep %>% 
  select(sex, bmi) %>% 
  Hmisc::describe()

# load the psych package
library(psych)

# run describe() function from psych package to get descriptive stats for abalone sex, length, diameter, height
# leave out sex - only numeric vars for psych::describe()
daviskeep %>% 
  select(weight, height, bmi) %>% 
  psych::describe()

# use Hmisc::describe() notation to get descriptive stats for abalone wholeWeight and shuckedWeight
daviskeep %>% 
  select(sex, weight, height, bmi) %>% 
  Hmisc::describe()

detach("package:Hmisc", unload=TRUE)
detach("package:psych", unload=TRUE)

# load the Hmisc package
library(Hmisc)

# run describe() function from Hmisc package to get descriptive stats for abalone sex, length, diameter, height
abaloneKeep %>% 
  select(sex, length, diameter, height) %>% 
  describe()

# load the psych package
library(psych)

# run describe() function from psych package to get descriptive stats for abalone sex, length, diameter, height
abaloneKeep %>% 
  select(sex, length, diameter, height) %>% 
  describe()

# use Hmisc::describe() notation to get descriptive stats for abalone wholeWeight and shuckedWeight
abaloneKeep %>% 
  select(wholeWeight, shuckedWeight) %>% 
  Hmisc::describe()

# specific statistics
# davis
# run summary() for shuckedWeight and wholeWeight
daviskeep %>%
  select(weight, height, bmi) %>%
  summary()

# get mean and sd for length
daviskeep %>%
  summarise(nht = n(),
            medianht = median(height),
            pt05 = quantile(height, probs = 0.05),
            pt95 = quantile(height, probs = 0.95),
            minht = min(height),
            maxht = max(height))

# just get mean and sd for height and diameter
daviskeep %>%
  select(weight, height, bmi) %>%
  summarise_all(funs(mean, sd))

# examples
# run summary() for shuckedWeight and wholeWeight
abaloneKeep %>%
  select(shuckedWeight, wholeWeight) %>%
  summary()

# get mean and sd for length
abaloneKeep %>%
  summarise(meanlt = mean(length),
            sdlt = sd(length))

# just get mean and sd for height and diameter
abaloneKeep %>%
  select(height, diameter) %>%
  summarise_all(funs(mean, sd))

# summary stats by group
# daviskeep
# get min, mean, sd, median, and max for age
daviskeep %>% select(bmi) %>%
  summarise_all(funs(min, mean, sd, median, max))

# get min, mean, sd, median, and max for age by sex as group
daviskeep %>% group_by(sex) %>%
  select(bmi, sex) %>%
  summarise_all(funs(min, mean, sd, median, max))

daviskeep %>%
  group_by(sex) %>%
  select(sex, weight, height, bmi) %>%
  summarise_all(funs(mean, sd))

# get median, 5th and 95th percentiles for wholeWeight by adult
daviskeep %>% group_by(sex) %>%
  select(bmi) %>%
  summarise(medbmi = median(bmi),
            q1bmi = quantile(bmi, probs = 0.05),
            q3bmi = quantile(bmi, probs = 0.95))

# exercises
# get min, mean, sd, median, and max for age
abaloneKeep %>% select(age) %>%
  summarise_all(funs(min, mean, sd, median, max))

# get min, mean, sd, median, and max for age by sex as group
abaloneKeep %>% group_by(sex) %>%
  select(age) %>%
  summarise_all(funs(min, mean, sd, median, max))

# get median, 25th and 75th percentiles for wholeWeight by adult
abaloneKeep %>% group_by(adult) %>%
  select(wholeWeight) %>%
  summarise(medwwt = median(wholeWeight),
            q1wwt = quantile(wholeWeight, probs = 0.25),
            q3wwt = quantile(wholeWeight, probs = 0.75))

# ==================================================
# associations - part 2 of ch3
# ==================================================

# bivariate correlations
# correlation between age and shuckedWeight with cor()
daviskeep %>% 
  select(bmi, weight, height) %>%
  cor()

# correlation between age and 4 weight measurements with tests
corbmi <- daviskeep %>% 
  select(bmi, weight, height) %>%
  psych::corr.test()

# print results with 6 digits and confidence intervals shown
print(corbmi, digits = 6, short = FALSE)

# show p-values in detail
corbmi$p

# Hmisc rcorr function
hmcor <- daviskeep %>% 
  select(bmi, weight, height) %>%
  as.matrix() %>%
  Hmisc::rcorr()

# a scatterplotplot matrix
pairs(~bmi+weight+height, data=daviskeep)

daviskeep %>% 
  select(bmi, weight, height) %>%
  pairs()

daviskeep %>% 
  select(bmi, weight, height) %>%
  pairs(col = daviskeep$sex)

library(GGally)
ggpairs(iris, aes(colour = Species, alpha = 0.4))

daviskeep %>% 
  select(bmi, weight, height) %>%
  GGally::ggpairs()

daviskeep %>% 
  select(bmi, weight, height, sex) %>%
  GGally::ggpairs(aes(colour = sex))

daviskeep %>% 
  select(bmi, weight, height, sex) %>%
  GGally::ggpairs(aes(color = sex))

abaloneKeep %>% 
  select(age, wholeWeight, shuckedWeight, sex) %>%
  GGally::ggpairs(aes(colour = sex))

# exercises
# correlation between age and shuckedWeight with cor()
abaloneKeep %>% 
  select(age, shuckedWeight) %>%
  cor()

# correlation between age and 4 weight measurements with tests
corAgeWts <- abaloneKeep %>% 
  select(age, wholeWeight, shuckedWeight, 
         shellWeight, visceraWeight) %>%
  psych::corr.test()

# print results with 6 digits and confidence intervals shown
print(corAgeWts, digits = 6, short = FALSE)

abaloneKeep %>% 
  select(age, wholeWeight, shuckedWeight, 
         shellWeight, visceraWeight) %>%
  as.matrix() %>%
  Hmisc::rcorr()

# plots
abaloneKeep %>% 
  select(age, wholeWeight, shuckedWeight) %>%
  pairs()

# ttests

# get n mean sd for bmi by sex
daviskeep %>% 
  select(bmi, sex) %>%
  group_by(sex) %>%
  group_by(N = n(), add = TRUE) %>%
  summarise_all(funs(mean, sd))

daviskeep %>% 
  select(bmi, sex) %>%
  group_by(sex) %>%
  summarise_all(funs(mean, sd))

# perform equal variance test
var.test(bmi ~ sex, data = daviskeep)

# perform pooled and unpooled t-test bmi by sex
t.test(bmi ~ sex, data = daviskeep)
t.test(bmi ~ sex, data = daviskeep, 
       var.equal = TRUE)

# exercises
# get n mean sd for adult groups for length
abaloneKeep %>% 
  select(length, adult) %>%
  group_by(adult) %>%
  group_by(N = n(), add = TRUE) %>%
  summarise_all(funs(mean, sd))

# perform equal variance test
var.test(length ~ adult, data = abaloneKeep)

# perform pooled and unpooled t-test length by adult
t.test(length ~ adult, data = abaloneKeep)
t.test(length ~ adult, data = abaloneKeep, var.equal = TRUE)

# plot boxplot
ggplot(abaloneKeep, aes(x=adult, y=length)) +
  geom_boxplot()

ggplot(abaloneKeep, aes(x=adult, y=length)) +
  geom_boxplot(aes(fill = adult))

# plot overlaid histograms
ggplot(abaloneKeep, aes(length)) +
  geom_histogram(color = "blue", fill = "yellow") +
  facet_wrap(vars(adult), nrow=2)

# chi-square tests
# create table object of sex by agecat
tablebmisex <- daviskeep %>%
  with(table(bmigt25, sex))

tablebmisex

# use table object to run chisq.test
chisq.test(tablebmisex)

library(gmodels)
# create CrossTabs table, only show column percentages
daviskeep %>%
  with(gmodels::CrossTable(bmigt25, sex,
                           chisq=TRUE,
                           prop.r = FALSE,
                           prop.t = FALSE,
                           prop.chisq = FALSE,
                           expected=TRUE))
# add visualization
mosaicplot(bmigt25 ~ sex, data = daviskeep,
           color = c("light blue", "dark grey"),
           main = "BMI Categories by Sex")

# cs goodness of fit
# NHANES prevalences
# underwt/norm 29.8
# overwt 32.5
# obese 37.7
bmitable <- daviskeep %>%
  with(table(bmicat))

chisq.test(x = bmitable,
           p = c(0.298, 0.325, 0.377))


# skip vcd
library(vcd)

mosaic(bmigt25 ~ sex, data = daviskeep,
       highlighting_fill = c("light blue", "dark grey"))

#mosaic(bmigt25 ~ sex, data = daviskeep,
#       shade = TRUE)


# exercises
# create table object of sex by agecat
tableSexAgecat <- abaloneKeep %>%
  with(table(sex, agecat))

# use table object to run chisq.test
chisq.test(tableSexAgecat)

# create CrossTabs table, only show column percentages
abaloneKeep %>%
  with(gmodels::CrossTable(sex, agecat,
                           chisq=TRUE,
                           prop.c = FALSE,
                           prop.t = FALSE,
                           prop.chisq = FALSE,
                           expected = TRUE))

# abalone example
sextable <- abaloneKeep %>%
  with(table(sex))

sextable/4169

chisq.test(x = sextable,
           p = c(0.333, 0.333, 0.334))

# add visualizations
mosaicplot(sex ~ agecat, data = abaloneKeep,
           color = c("light blue", "dark grey"),
           main = "Abalone Sex by Age Categories")

mosaicplot(adult ~ agecat, data = abaloneKeep,
           color = c("light green", "purple"),
           main = "Abalone Adult by Age Categories")

## capstone for ch3
abaloneKeep %>%
  select(age, wholeWeight, shuckedWeight, 
         shellWeight, visceraWeight, length, 
         diameter, height) %>%
  cor()

abaloneKeep %>%
  filter(sex == "I") %>%
  select(age, wholeWeight, shuckedWeight, 
         shellWeight, visceraWeight, length, 
         diameter, height) %>%
  cor()

abaloneKeep %>%
  filter(sex == "F") %>%
  select(age, wholeWeight, shuckedWeight, 
         shellWeight, visceraWeight, length, 
         diameter, height) %>%
  cor()

abaloneKeep %>%
  filter(sex == "M") %>%
  select(age, wholeWeight, shuckedWeight, 
         shellWeight, visceraWeight, length, 
         diameter, height) %>%
  cor()

abaloneKeep %>%
  select(age, shellWeight) %>%
  cor()

abaloneKeep %>%
  filter(sex == "M") %>%
  select(age, shellWeight) %>%
  cor()

abaloneKeep %>%
  filter(sex == "F") %>%
  select(age, shellWeight) %>%
  cor()

abaloneKeep %>%
  filter(sex == "I") %>%
  select(age, shellWeight) %>%
  cor()

ggplot(abaloneKeep, aes(shellWeight)) +
  geom_histogram(color = "black", fill = "yellow")
median(abaloneKeep$shellWeight)

abaloneKeep2 <- abaloneKeep %>%
  mutate(shellSplit = shellWeight > 0.235)

tabshell <- abaloneKeep2 %>%
  with(table(agecat, shellSplit))
chisq.test(tabshell)

table(abaloneKeep$agecat)


# =======================================
# working with summary stats output
davissmry <- daviskeep %>%
  select(weight, height, bmi) %>%
  summary()

class(davissmry)
davissmry[,1:2]

davisbmisex <- daviskeep %>% group_by(sex) %>%
  select(bmi) %>%
  summarise(medbmi = median(bmi),
            q1bmi = quantile(bmi, probs = 0.05),
            q3bmi = quantile(bmi, probs = 0.95))

class(davisbmisex)
davisbmisex %>%
  filter(sex == "M")

# ===========================

davishmisc <- daviskeep %>%
  select(weight, height, bmi) %>%
  Hmisc::describe()

class(davishmisc)
is.list(davishmisc)
davishmisc$weight

davispsych <- daviskeep %>%
  select(weight, height, bmi) %>%
  psych::describe()

class(davispsych)
is.list(davispsych)
davispsych$median

# ===============================

corbmi <- daviskeep %>% 
  select(bmi, weight, height) %>%
  psych::corr.test()

class(corbmi)
is.list(corbmi)

corbmi$p

bmittest <- t.test(bmi ~ sex, data = daviskeep, 
                   var.equal = TRUE)
class(bmittest)
is.list(bmittest)

bmittest$statistic
bmittest$parameter
bmittest$p.value
bmittest$conf.int

tablebmisex <- daviskeep %>%
  with(table(bmigt25, sex))
csbmi <- chisq.test(tablebmisex)
class(csbmi)
is.list(csbmi)

csbmi$statistic
csbmi$parameter
csbmi$p.value
csbmi$expected

# ====================================
# repeat with abaloneKeep dataset

# =======================================
# working with summary stats output
absummary <- abaloneKeep %>%
  select(wholeWeight, shuckedWeight, shellWeight) %>%
  summary()

class(absummary)
absummary[,1:2]

absexdiam <- abaloneKeep %>% group_by(sex) %>%
  select(diameter) %>%
  summarise(meandiam = mean(diameter),
            sddiam = sd(diameter))

class(absexdiam)
absexdiam %>%
  filter(sex == "I")

# ===========================

abhmisc <- abaloneKeep %>%
  select(wholeWeight, shuckedWeight, shellWeight) %>%
  Hmisc::describe()

class(abhmisc)
is.list(abhmisc)
abhmisc$shuckedWeight

abpsych <- abaloneKeep %>%
  select(wholeWeight, shuckedWeight, shellWeight) %>%
  psych::describe()

class(abpsych)
is.list(abpsych)
abpsych$mad

# ===============================

abage <- abaloneKeep %>% 
  select(age, wholeWeight, shuckedWeight) %>%
  psych::corr.test()

class(abage)
is.list(abage)
names(abage)

abage$p
abage$r

abttest <- t.test(shellWeight ~ adult, data = abaloneKeep, 
                  var.equal = TRUE)
class(abttest)
is.list(abttest)

abttest$statistic
abttest$parameter
abttest$p.value
abttest$conf.int

abagesex <- abaloneKeep %>%
  with(table(agecat, sex))
csagesex <- chisq.test(abagesex)
class(csagesex)
is.list(csagesex)

csagesex$statistic
csagesex$parameter
csagesex$p.value
csagesex$expected

# ======================================
# placeholder codes
# ======================================

# save spearman's rho using cor() function as spearcor
spearcor <- abaloneKeep %>%
  select(age, length, height, diameter) %>%
  cor(method = "spearman")

# display class of spearcar
class(spearcor)

# save Mann Whitney test of diameter by adult as wcox
wcox <- wilcox.test(diameter ~ adult,
                    data = abaloneKeep)

# display wcox test statistic, parameter and p-value
wcox$statistic
wcox$p.value

# =====================================
# list slides and exercises

x <- TRUE
age <- c("child", "young", "old")
a <- c(5.0, 3.1, 2.4)
b <- c(4.1, 2.2, 5.4)
m <- matrix(c(a, b),
            nrow = 3,
            ncol = 2)
l <- list(x, age, m)
names(l) <- c("xval","agevec","mtx")

names(l)
class(l)
is.list(l)

l$agevec
l$mtx

# =========================================
# UPDATED working with stats output
# reuse code from earlier lessons
# in final lesson 4 - analogous to final
# exercises with abalones
# ========================================

# From lesson 1
# Summary statistics of weight, height, bmi
davissummary <- daviskeep %>%
  select(weight, height, bmi) %>%
  summary()

class(davissummary)
davissummary[1:3,]
davissummary[,1:2]
dim(davissummary)

# from slide 15, lesson 1 - by group summarise, summarise_all
davisbysex <- daviskeep %>%
  group_by(sex) %>%
  select(sex, weight, height, bmi) %>%
  summarise_all(funs(mean, sd))

# Tbl tibbles are special cases of data frames
# data frames are special cases of lists
class(davisbysex)
is.list(davisbysex)

davisbysex
names(davisbysex)
dim(davisbysex)

# pull out the stats row for males
davisbysex %>% filter(sex == "M")

davisbysex$height_mean


# from lesson 1 using Hmisc
# Run describe() function from Hmisc package for sex and bmi
davisHmisc <- daviskeep %>% 
  select(sex, bmi) %>% 
  Hmisc::describe()

class(davisHmisc)
is.list(davisHmisc)
names(davisHmisc)

davisHmisc$sex

class(davisHmisc$sex)
is.list(davisHmisc$sex)
names(davisHmisc$sex)

davisHmisc$sex$counts

# from lesson 1
# Run describe() function from psych package for weight, height and bmi.
# Leave off sex since psych::describe only works for numeric variables
davispsych <- daviskeep %>% 
  select(weight, height, bmi) %>% 
  psych::describe()

class(davispsych)
is.data.frame(davispsych)
is.list(davispsych)

names(davispsych)

davispsych$n

davispsych$median

davispsych %>%
  select(n, mean, sd)

# from lesson 2 - slide 6
# Correlation using psych::corr.test
# to get p-values
daviscorr <- daviskeep %>% 
  select(bmi, weight, height) %>%
  psych::corr.test()

class(daviscorr)
is.list(daviscorr)
is.data.frame(daviscorr)

names(daviscorr)

# View correlations
daviscorr$r

# View p-values
daviscorr$p

# View confidence intervals for r
daviscorr$ci

# t-test output
# ttests - pooled and unpooled
# slide 13 lesson 2

# UNPOOLED t-test bmi by sex
davisunpooled <- t.test(bmi ~ sex, 
       data = daviskeep)

# POOLED t-test bmi by sex
davispooled <- t.test(bmi ~ sex, data = daviskeep, 
       var.equal = TRUE)

class(davisunpooled)
is.list(davisunpooled)
names(davisunpooled)

names(davispooled)

# from lesson 3
#chisq test
#slide 5 lesson 3

# Create table object of bmigt25 by sex and view table
tablebmisex <- daviskeep %>%
  with(table(bmigt25, sex))
daviscs <- chisq.test(tablebmisex)
class(daviscs)

names(daviscs)

# t-test statistic, degrees of freedom, p-value
davisunpooled$statistic
davisunpooled$parameter
davisunpooled$p.value

# chi-square test statistic, df, p-value
daviscs$statistic
daviscs$parameter
daviscs$p.value

# chi-square test observed counts
daviscs$observed

# chi-square test expected values
daviscs$expected

