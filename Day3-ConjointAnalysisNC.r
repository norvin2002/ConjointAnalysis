pacman::p_load('tidyverse', 'conjoint', 'readxl', 'ggplot2', 'factoextra')
#https://youtu.be/8TwllD5jOPg
#https://medium.com/@aditigupta029/want-to-know-customers-preferences-for-new-product-s-features-here-is-a-simple-example-of-da25d6099137

setwd('C:/Users/nchandra/OneDrive - National University of Singapore/ComplexPredictiveMod/Day3/Workshop')

factor = expand.grid(
    package = c("A","B","C"),
    brand = c("K2R", "Glory", "Bissell"),
    price = c("$1.19", "$1.29", "$1.39"),
    seal = c("Yes", "No"),
    money = c("Yes", "No")
)

print(factor)

#1 Partial profile method
par_profile = caFactorialDesign(data = factor, type = 'fractional',cards = 22, seed = 123)
code = caEncodedDesign(par_profile)
encodedorthodesign <- data.frame(par_profile,code)

nrow(par_profile)

# all the attributes are independent from one another
cor(code)

#2 create a dataframe containing all the levels
levels = c("A", "B", "C", "K2R", "Glory", "Bissell", "$1.19", "$1.29", "$1.39","Yes", "No", "Yes", "No")
lev.df = data.frame(levels)

#3 Responses
survey = read_excel('Carpet_pref.xls')

head(survey)

dim(survey)

#Conjoint Analysis
Conjoint(y= survey[,2:ncol(survey)], x = par_profile, z = lev.df, y.type = 'score')

caPartUtilities(y = survey[,2:ncol(survey)], x = par_profile, z = lev.df)

segments = caSegmentation(y= survey[,2:ncol(survey)], x = code, c= 3)

segments


caImportance(y = survey[,2:ncol(survey)], x= par_profile)

clust = segments$segm

fviz_cluster(clust,segments$util)


