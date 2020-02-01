# Project-1: Real Estate
# Date: 16-01-2020

setwd("J:/Edvancer R/Project 1 Real Estate/")
getwd()

# read the train and test datasets
hg_train = read.csv("housing_train.csv", stringsAsFactors = F)
hg_test = read.csv("housing_test.csv", stringsAsFactors = F)

# find the variance of target variable
sum(is.na(hg_train$Price))
var_price = var(hg_train$Price, na.rm = T)
# Ans: 432958829214.684

# combine test & train data
library(dplyr)
hg_test$Price = NA
hg_train$data = 'train'
hg_test$data = 'test'
hg_all = rbind(hg_train, hg_test)
glimpse(hg_all)

# Find out how many observations have missing values for variable 'YearBuilt'?
num_NA = sum(is.na(hg_all$YearBuilt))
n = hg_all[is.na(hg_all$YearBuilt),]
unique(hg_all$YearBuilt)
table(is.na(hg_all$YearBuilt))
# Ans: 4660

# What is the difference in average price between house type h and t?
table(hg_train$Type)
unique(hg_train$Type)

hgPrice_h = hg_train$Price[hg_train$Type == "h"] 
hgPrice_t = hg_train$Price[hg_train$Type == "t"] 
diffInPrice = mean(hgPrice_h) - mean(hgPrice_t)
# Ans: 392384.197334447

# How many unique values variable postcode takes?
unq_pc = unique(hg_all$Postcode)
# Ans: 94

# how should you treat post code ? As a categorical variable or numeric variable 
# Ans: categorical

# Does distance follow a normal distribution?
library(ggplot2)
unique(hg_all$Distance)
ggplot(hg_all,aes(x = Distance)) + geom_histogram()
ggplot(hg_all,aes(x = Distance)) + geom_density()
# No

# Which seller has maximum value transactions? ( Sum of Price)
unique(hg_all$SellerG)
library(dplyr)
hg_all_Sllr = hg_all %>%
  group_by(SellerG) %>%
  summarise(total_trans = sum(Price, na.rm = T))

max_tran_S = hg_all_Sllr$SellerG[hg_all_Sllr$total_trans == max(hg_all_Sllr$total_trans)]
sum(hg_all$Price[hg_all$SellerG == "Jellis"], na.rm = T)
# Ans: Jellis

# Which CouncilArea has maximum average price?

hg_all_CA = hg_all %>%
  group_by(CouncilArea) %>%
  summarise(avg_price = mean(Price, na.rm = T), var_price = sd(Price, na.rm = T))

max_avg_CA = hg_all_CA$CouncilArea[hg_all_CA$avg_price == max(hg_all_CA$avg_price)]
mean(hg_all$Price[hg_all$CouncilArea == "Bayside"], na.rm = T)
# Ans: Bayside

# which CouncilArea has maximum variance(sd) in the price?

max_var_CA = hg_all_CA$CouncilArea[hg_all_CA$var_price == max(hg_all_CA$var_price)]
sd(hg_all$Price[hg_all$CouncilArea == "Stonnington"], na.rm = T)
var(hg_all$Price[hg_all$CouncilArea == "Stonnington"], na.rm = T)
# Ans: Stonnington

# Should we use Address as is in the modeling process?
(unique(hg_all$Address))
sum(is.na(hg_all$Address))
n_add = table(hg_all$Address)
sort(n_add, decreasing = T)

# categorical variables
glimpse(hg_all)
unique(hg_all$Suburb) # 142 unique categories
unique(hg_all$Address) # 9324 unique categories so we will not consider it in the model
unique(hg_all$Type) # 3 unique categories
unique(hg_all$Method) # 5 unique categories
unique(hg_all$SellerG) # 198 unique categories
unique(hg_all$CouncilArea) # 20 unique categories

# although post code is integer but it should be considered as categorial variable
unique(hg_all$Postcode) # 94 unique categories

# write function to create dummy for categorical variables
CreateDummies=function(data,var,freq_cutoff=10){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub(",","",name)
    name=gsub("\\+","",name)
    name=gsub("-","_",name)
    name=gsub("/","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

# create dummy for categorical variable for Suburb, Type, Method, SellerG, CouncilArea, Postcode
# check the value for each category and create dummies

# categories Suburb
t = table(hg_all$Suburb)
sort(t)
hg_all_d = CreateDummies(hg_all, "Suburb", 100)

# categories Type
t = table(hg_all$Type)
sort(t)
hg_all_d = CreateDummies(hg_all_d, "Type", 100)

# categories Method
t = table(hg_all$Method)
sort(t)
hg_all_d = CreateDummies(hg_all_d, "Method", 10)

# categories SellerG
t = table(hg_all$SellerG)
sort(t)
hg_all_d = CreateDummies(hg_all_d, "SellerG", 100)

# categories CouncilArea
t = table(hg_all$CouncilArea)
sort(t)
hg_all_d = CreateDummies(hg_all_d, "CouncilArea", 100)

# categories Postcode
t = table(hg_all$Postcode)
sort(t)
hg_all_d = CreateDummies(hg_all_d, "Postcode", 100)

# look at all the varibles in the dataframe
names(hg_all_d)

# remove variable Address
hg_all_d$Address = NULL

glimpse(hg_all_d)
# All variables are numeric except data which will be used to separate train and test

# Check for NA in all variables except dummy and data
sum(is.na(hg_all_d$Rooms)) # no NA
sum(is.na(hg_all_d$Price)) # no NA in train but 1885 NA in test
sum(is.na(hg_all_d$Distance)) # no NA

sum(is.na(hg_all_d$Bedroom2)) # 1978 NA replace by median
median(hg_all_d[hg_all_d$data == 'train', "Bedroom2"], na.rm =T)
sum(is.na(hg_all_d$Bathroom)) # 1978 NA replace by median
median(hg_all_d[hg_all_d$data == 'train', "Bathroom"], na.rm =T)
sum(is.na(hg_all_d$Car)) # 1978 NA replace by median
median(hg_all_d[hg_all_d$data == 'train', "Car"], na.rm =T)
sum(is.na(hg_all_d$YearBuilt)) # 4660 NA replace by median
median(hg_all_d[hg_all_d$data == 'train', "YearBuilt"], na.rm =T)

sum(is.na(hg_all_d$Landsize)) # 1985 NA replace by mean
mean(hg_all_d[hg_all_d$data == 'train', "Landsize"], na.rm =T)
sum(is.na(hg_all_d$BuildingArea)) # 5269 NA replace by mean
mean(hg_all_d[hg_all_d$data == 'train', "BuildingArea"], na.rm =T)

# variable where NA are to be replaced with mean
col_median = c("Bedroom2","Bathroom","Car","YearBuilt")
col_mean = c("Landsize","BuildingArea")

# function for replacing NA with mean
for( col in col_mean)
{
  hg_all_d[is.na(hg_all_d[,col]),col] = mean(hg_all_d[hg_all_d$data == 'train', col], na.rm =T)
}
# No NA in variable "Landsize" & "BuildingArea"

# function for replacing NA with median
for( col in col_median)
{
  hg_all_d[is.na(hg_all_d[,col]),col] = median(hg_all_d[hg_all_d$data == 'train', col], na.rm =T)
}
# No NA in variable "Bedroom2","Bathroom","Car","YearBuilt"

t = names(hg_all_d) %in% c("data", "Price")
sum(is.na(hg_all_d[,!t]))
# No NA in data set

# separate train and test data
hg_dp_train = hg_all_d %>% filter(data =="train") %>% select(-data)
hg_dp_test = hg_all_d %>% filter(data =="test") %>% select(-data,-Price)

# divide train data into two parts for validation
set.seed(2)
s = sample(1:nrow(hg_dp_train),0.8*nrow(hg_dp_train))
hg_dp_train1=hg_dp_train[s,]
hg_dp_train2=hg_dp_train[-s,]

# build the intial model 
fit_vif=lm(Price~., data = hg_dp_train1)

# drop those variables from the model whose coefficients are NA
formula(fit_vif)
fit_vif = lm(Price ~ .-Postcode_3039-Postcode_3071-Postcode_3013-Postcode_3103-Postcode_3124-Postcode_3207-
               Postcode_3044-Postcode_3187-Postcode_3122-Postcode_3104-Postcode_3070-Postcode_3101-
               Postcode_3186-Postcode_3146-Postcode_3056-Postcode_3141-Postcode_3182-Postcode_3072-
               Postcode_3165-Postcode_3073, data = hg_dp_train1)
# No coefficients of the predictor variables are NA

# remove those variables whose vif value is greater than 5 one by one
library(car)
sort(vif(fit_vif), decreasing = T)[1:5]
# remove Method_s
fit_vif = lm(Price ~ .-Postcode_3039-Postcode_3071-Postcode_3013-Postcode_3103-Postcode_3124-Postcode_3207-
               Postcode_3044-Postcode_3187-Postcode_3122-Postcode_3104-Postcode_3070-Postcode_3101-
               Postcode_3186-Postcode_3146-Postcode_3056-Postcode_3141-Postcode_3182-Postcode_3072-
               Postcode_3165-Postcode_3073-Method_S, data = hg_dp_train1)
sort(vif(fit_vif), decreasing = T)[1:5]
# remove Suburb_MalvernEast
fit_vif = lm(Price ~ .-Postcode_3039-Postcode_3071-Postcode_3013-Postcode_3103-Postcode_3124-Postcode_3207-
               Postcode_3044-Postcode_3187-Postcode_3122-Postcode_3104-Postcode_3070-Postcode_3101-
               Postcode_3186-Postcode_3146-Postcode_3056-Postcode_3141-Postcode_3182-Postcode_3072-
               Postcode_3165-Postcode_3073-Method_S-Suburb_MalvernEast, data = hg_dp_train1)
sort(vif(fit_vif), decreasing = T)[1:5]
# remove Postcode_3121
fit_vif = lm(Price ~ .-Postcode_3039-Postcode_3071-Postcode_3013-Postcode_3103-Postcode_3124-Postcode_3207-
               Postcode_3044-Postcode_3187-Postcode_3122-Postcode_3104-Postcode_3070-Postcode_3101-
               Postcode_3186-Postcode_3146-Postcode_3056-Postcode_3141-Postcode_3182-Postcode_3072-
               Postcode_3165-Postcode_3073-Method_S-Suburb_MalvernEast-Postcode_3121, data = hg_dp_train1)
sort(vif(fit_vif), decreasing = T)[1:5]
# remove CouncilArea_
fit_vif = lm(Price ~ .-Postcode_3039-Postcode_3071-Postcode_3013-Postcode_3103-Postcode_3124-Postcode_3207-
               Postcode_3044-Postcode_3187-Postcode_3122-Postcode_3104-Postcode_3070-Postcode_3101-
               Postcode_3186-Postcode_3146-Postcode_3056-Postcode_3141-Postcode_3182-Postcode_3072-
               Postcode_3165-Postcode_3073-Method_S-Suburb_MalvernEast-Postcode_3121-CouncilArea_, 
               data = hg_dp_train1)
sort(vif(fit_vif), decreasing = T)[1:5]
# remove Postcode_3011
fit_vif = lm(Price ~ .-Postcode_3039-Postcode_3071-Postcode_3013-Postcode_3103-Postcode_3124-Postcode_3207-
               Postcode_3044-Postcode_3187-Postcode_3122-Postcode_3104-Postcode_3070-Postcode_3101-
               Postcode_3186-Postcode_3146-Postcode_3056-Postcode_3141-Postcode_3182-Postcode_3072-
               Postcode_3165-Postcode_3073-Method_S-Suburb_MalvernEast-Postcode_3121-CouncilArea_- 
               Postcode_3011, data = hg_dp_train1)
sort(vif(fit_vif), decreasing = T)[1:5]
# all variables having redundant information have been removed and vif values are less than 5
# there is no multicolinearity

# now we have to drop variables based on p-values > 0.05
fit = lm(Price ~ .-Postcode_3039-Postcode_3071-Postcode_3013-Postcode_3103-Postcode_3124-Postcode_3207-
           Postcode_3044-Postcode_3187-Postcode_3122-Postcode_3104-Postcode_3070-Postcode_3101-
           Postcode_3186-Postcode_3146-Postcode_3056-Postcode_3141-Postcode_3182-Postcode_3072-
           Postcode_3165-Postcode_3073-Method_S-Suburb_MalvernEast-Postcode_3121-CouncilArea_- 
           Postcode_3011, data = hg_dp_train1)
summary(fit)
# instead of dropping variables one by one based on p-value, we can use step function
# step function drop variable based on AIC score swhich is similar to p-value
fit = step(fit)
summary(fit)
# still some variables have p-value greater than 0.5 which has to be removed step by step
formula(fit)

summary(fit)
# remove Suburb_StKilda
fit = lm(Price ~ Rooms + Distance + Bedroom2 + Bathroom + Car + Landsize + BuildingArea + YearBuilt + 
           Suburb_Doncaster + Suburb_Footscray + Suburb_Thornbury + Suburb_Hampton + Suburb_Balwyn + 
           Suburb_Camberwell + Suburb_PortMelbourne + Suburb_BrightonEast + Suburb_Hawthorn + 
           Suburb_BalwynNorth + Suburb_Kew + Suburb_Brighton + Suburb_GlenIris + Suburb_Brunswick + 
           Suburb_SouthYarra + Suburb_Preston + Suburb_Richmond + Suburb_BentleighEast + 
           Suburb_Reservoir + Type_u + Type_h + Method_VB + Method_SP + Method_PI + SellerG_Kay + 
           SellerG_Miles + SellerG_RT + SellerG_Marshall + SellerG_hockingstuart + SellerG_Jellis + 
           SellerG_Nelson + CouncilArea_Manningham + CouncilArea_Brimbank + CouncilArea_HobsonsBay + 
           CouncilArea_Bayside + CouncilArea_Melbourne + CouncilArea_Banyule + CouncilArea_Yarra + 
           CouncilArea_Maribyrnong + CouncilArea_Stonnington + CouncilArea_GlenEira + CouncilArea_Darebin + 
           CouncilArea_MooneeValley + CouncilArea_Moreland + Postcode_3147 + Postcode_3145 + 
           Postcode_3127 + Postcode_3081 + Postcode_3031 + Postcode_3181 + Postcode_3015 + Postcode_3188 + 
           Postcode_3012 + Postcode_3204 + Postcode_3058 + Postcode_3163 + Postcode_3040 + Postcode_3032 + 
           Postcode_3046 + Postcode_3020, data = hg_dp_train1)

summary(fit)
# remove SellerG_Nelson
fit = lm(Price ~ Rooms + Distance + Bedroom2 + Bathroom + Car + Landsize + BuildingArea + YearBuilt + 
           Suburb_Doncaster + Suburb_Footscray + Suburb_Thornbury + Suburb_Hampton + Suburb_Balwyn + 
           Suburb_Camberwell + Suburb_PortMelbourne + Suburb_BrightonEast + Suburb_Hawthorn + 
           Suburb_BalwynNorth + Suburb_Kew + Suburb_Brighton + Suburb_GlenIris + Suburb_Brunswick + 
           Suburb_SouthYarra + Suburb_Preston + Suburb_Richmond + Suburb_BentleighEast + 
           Suburb_Reservoir + Type_u + Type_h + Method_VB + Method_SP + Method_PI + SellerG_Kay + 
           SellerG_Miles + SellerG_RT + SellerG_Marshall + SellerG_hockingstuart + SellerG_Jellis + 
           CouncilArea_Manningham + CouncilArea_Brimbank + CouncilArea_HobsonsBay + 
           CouncilArea_Bayside + CouncilArea_Melbourne + CouncilArea_Banyule + CouncilArea_Yarra + 
           CouncilArea_Maribyrnong + CouncilArea_Stonnington + CouncilArea_GlenEira + CouncilArea_Darebin + 
           CouncilArea_MooneeValley + CouncilArea_Moreland + Postcode_3147 + Postcode_3145 + 
           Postcode_3127 + Postcode_3081 + Postcode_3031 + Postcode_3181 + Postcode_3015 + Postcode_3188 + 
           Postcode_3012 + Postcode_3204 + Postcode_3058 + Postcode_3163 + Postcode_3040 + Postcode_3032 + 
           Postcode_3046 + Postcode_3020, data = hg_dp_train1)

summary(fit)
# remove Suburb_Richmond
fit = lm(Price ~ Rooms + Distance + Bedroom2 + Bathroom + Car + Landsize + BuildingArea + YearBuilt + 
           Suburb_Doncaster + Suburb_Footscray + Suburb_Thornbury + Suburb_Hampton + Suburb_Balwyn + 
           Suburb_Camberwell + Suburb_PortMelbourne + Suburb_BrightonEast + Suburb_Hawthorn + 
           Suburb_BalwynNorth + Suburb_Kew + Suburb_Brighton + Suburb_GlenIris + Suburb_Brunswick + 
           Suburb_SouthYarra + Suburb_Preston + Suburb_BentleighEast + Suburb_Reservoir + Type_u + 
           Type_h + Method_VB + Method_SP + Method_PI + SellerG_Kay + SellerG_Miles + SellerG_RT + 
           SellerG_Marshall + SellerG_hockingstuart + SellerG_Jellis + CouncilArea_Manningham + 
           CouncilArea_Brimbank + CouncilArea_HobsonsBay + CouncilArea_Bayside + CouncilArea_Melbourne + 
           CouncilArea_Banyule + CouncilArea_Yarra + CouncilArea_Maribyrnong + CouncilArea_Stonnington + 
           CouncilArea_GlenEira + CouncilArea_Darebin + CouncilArea_MooneeValley + CouncilArea_Moreland + 
           Postcode_3147 + Postcode_3145 + Postcode_3127 + Postcode_3081 + Postcode_3031 + Postcode_3181 + 
           Postcode_3015 + Postcode_3188 + Postcode_3012 + Postcode_3204 + Postcode_3058 + Postcode_3163 + 
           Postcode_3040 + Postcode_3032 + Postcode_3046 + Postcode_3020, data = hg_dp_train1)

summary(fit)
# remove Postcode_3145
fit = lm(Price ~ Rooms + Distance + Bedroom2 + Bathroom + Car + Landsize + BuildingArea + YearBuilt + 
           Suburb_Doncaster + Suburb_Footscray + Suburb_Thornbury + Suburb_Hampton + Suburb_Balwyn + 
           Suburb_Camberwell + Suburb_PortMelbourne + Suburb_BrightonEast + Suburb_Hawthorn + 
           Suburb_BalwynNorth + Suburb_Kew + Suburb_Brighton + Suburb_GlenIris + Suburb_Brunswick + 
           Suburb_SouthYarra + Suburb_Preston + Suburb_BentleighEast + Suburb_Reservoir + Type_u + 
           Type_h + Method_VB + Method_SP + Method_PI + SellerG_Kay + SellerG_Miles + SellerG_RT + 
           SellerG_Marshall + SellerG_hockingstuart + SellerG_Jellis + CouncilArea_Manningham + 
           CouncilArea_Brimbank + CouncilArea_HobsonsBay + CouncilArea_Bayside + CouncilArea_Melbourne + 
           CouncilArea_Banyule + CouncilArea_Yarra + CouncilArea_Maribyrnong + CouncilArea_Stonnington + 
           CouncilArea_GlenEira + CouncilArea_Darebin + CouncilArea_MooneeValley + CouncilArea_Moreland + 
           Postcode_3147 + Postcode_3127 + Postcode_3081 + Postcode_3031 + Postcode_3181 + 
           Postcode_3015 + Postcode_3188 + Postcode_3012 + Postcode_3204 + Postcode_3058 + Postcode_3163 + 
           Postcode_3040 + Postcode_3032 + Postcode_3046 + Postcode_3020, data = hg_dp_train1)

summary(fit)
# remove Postcode_3012
fit = lm(Price ~ Rooms + Distance + Bedroom2 + Bathroom + Car + Landsize + BuildingArea + YearBuilt + 
           Suburb_Doncaster + Suburb_Footscray + Suburb_Thornbury + Suburb_Hampton + Suburb_Balwyn + 
           Suburb_Camberwell + Suburb_PortMelbourne + Suburb_BrightonEast + Suburb_Hawthorn + 
           Suburb_BalwynNorth + Suburb_Kew + Suburb_Brighton + Suburb_GlenIris + Suburb_Brunswick + 
           Suburb_SouthYarra + Suburb_Preston + Suburb_BentleighEast + Suburb_Reservoir + Type_u + 
           Type_h + Method_VB + Method_SP + Method_PI + SellerG_Kay + SellerG_Miles + SellerG_RT + 
           SellerG_Marshall + SellerG_hockingstuart + SellerG_Jellis + CouncilArea_Manningham + 
           CouncilArea_Brimbank + CouncilArea_HobsonsBay + CouncilArea_Bayside + CouncilArea_Melbourne + 
           CouncilArea_Banyule + CouncilArea_Yarra + CouncilArea_Maribyrnong + CouncilArea_Stonnington + 
           CouncilArea_GlenEira + CouncilArea_Darebin + CouncilArea_MooneeValley + CouncilArea_Moreland + 
           Postcode_3147 + Postcode_3127 + Postcode_3081 + Postcode_3031 + Postcode_3181 + 
           Postcode_3015 + Postcode_3188 + Postcode_3204 + Postcode_3058 + Postcode_3163 + 
           Postcode_3040 + Postcode_3032 + Postcode_3046 + Postcode_3020, data = hg_dp_train1)

summary(fit)
# remove Suburb_Footscray
fit = lm(Price ~ Rooms + Distance + Bedroom2 + Bathroom + Car + Landsize + BuildingArea + YearBuilt + 
           Suburb_Doncaster + Suburb_Thornbury + Suburb_Hampton + Suburb_Balwyn + Suburb_Camberwell + 
           Suburb_PortMelbourne + Suburb_BrightonEast + Suburb_Hawthorn + Suburb_BalwynNorth + 
           Suburb_Kew + Suburb_Brighton + Suburb_GlenIris + Suburb_Brunswick + Suburb_SouthYarra + 
           Suburb_Preston + Suburb_BentleighEast + Suburb_Reservoir + Type_u + Type_h + Method_VB + 
           Method_SP + Method_PI + SellerG_Kay + SellerG_Miles + SellerG_RT + SellerG_Marshall + 
           SellerG_hockingstuart + SellerG_Jellis + CouncilArea_Manningham + CouncilArea_Brimbank + 
           CouncilArea_HobsonsBay + CouncilArea_Bayside + CouncilArea_Melbourne + CouncilArea_Banyule + 
           CouncilArea_Yarra + CouncilArea_Maribyrnong + CouncilArea_Stonnington + CouncilArea_GlenEira + 
           CouncilArea_Darebin + CouncilArea_MooneeValley + CouncilArea_Moreland + Postcode_3147 + 
           Postcode_3127 + Postcode_3081 + Postcode_3031 + Postcode_3181 + Postcode_3015 + Postcode_3188 + 
           Postcode_3204 + Postcode_3058 + Postcode_3163 + Postcode_3040 + Postcode_3032 + Postcode_3046 +
           Postcode_3020, data = hg_dp_train1)

summary(fit)
# remove Suburb_Brunswick
fit = lm(Price ~ Rooms + Distance + Bedroom2 + Bathroom + Car + Landsize + BuildingArea + YearBuilt + 
           Suburb_Doncaster + Suburb_Thornbury + Suburb_Hampton + Suburb_Balwyn + Suburb_Camberwell + 
           Suburb_PortMelbourne + Suburb_BrightonEast + Suburb_Hawthorn + Suburb_BalwynNorth + 
           Suburb_Kew + Suburb_Brighton + Suburb_GlenIris + Suburb_SouthYarra + 
           Suburb_Preston + Suburb_BentleighEast + Suburb_Reservoir + Type_u + Type_h + Method_VB + 
           Method_SP + Method_PI + SellerG_Kay + SellerG_Miles + SellerG_RT + SellerG_Marshall + 
           SellerG_hockingstuart + SellerG_Jellis + CouncilArea_Manningham + CouncilArea_Brimbank + 
           CouncilArea_HobsonsBay + CouncilArea_Bayside + CouncilArea_Melbourne + CouncilArea_Banyule + 
           CouncilArea_Yarra + CouncilArea_Maribyrnong + CouncilArea_Stonnington + CouncilArea_GlenEira + 
           CouncilArea_Darebin + CouncilArea_MooneeValley + CouncilArea_Moreland + Postcode_3147 + 
           Postcode_3127 + Postcode_3081 + Postcode_3031 + Postcode_3181 + Postcode_3015 + Postcode_3188 + 
           Postcode_3204 + Postcode_3058 + Postcode_3163 + Postcode_3040 + Postcode_3032 + Postcode_3046 +
           Postcode_3020, data = hg_dp_train1)

summary(fit)
# remove CouncilArea_GlenEira
fit = lm(Price ~ Rooms + Distance + Bedroom2 + Bathroom + Car + Landsize + BuildingArea + YearBuilt + 
           Suburb_Doncaster + Suburb_Thornbury + Suburb_Hampton + Suburb_Balwyn + Suburb_Camberwell + 
           Suburb_PortMelbourne + Suburb_BrightonEast + Suburb_Hawthorn + Suburb_BalwynNorth + 
           Suburb_Kew + Suburb_Brighton + Suburb_GlenIris + Suburb_SouthYarra + 
           Suburb_Preston + Suburb_BentleighEast + Suburb_Reservoir + Type_u + Type_h + Method_VB + 
           Method_SP + Method_PI + SellerG_Kay + SellerG_Miles + SellerG_RT + SellerG_Marshall + 
           SellerG_hockingstuart + SellerG_Jellis + CouncilArea_Manningham + CouncilArea_Brimbank + 
           CouncilArea_HobsonsBay + CouncilArea_Bayside + CouncilArea_Melbourne + CouncilArea_Banyule + 
           CouncilArea_Yarra + CouncilArea_Maribyrnong + CouncilArea_Stonnington + 
           CouncilArea_Darebin + CouncilArea_MooneeValley + CouncilArea_Moreland + Postcode_3147 + 
           Postcode_3127 + Postcode_3081 + Postcode_3031 + Postcode_3181 + Postcode_3015 + Postcode_3188 + 
           Postcode_3204 + Postcode_3058 + Postcode_3163 + Postcode_3040 + Postcode_3032 + Postcode_3046 +
           Postcode_3020, data = hg_dp_train1)

summary(fit)
# all p-values are less than 0.05

# check the performance of model on validation data (train2)
val.pred = predict(fit, newdata = hg_dp_train2)
# error
err = hg_dp_train2$Price - val.pred
# RMSE
err ** 2 %>% mean() %>% sqrt() 
# score
212467 / err ** 2 %>% mean() %>% sqrt() # score on val data is 0.564 > 0.51

## build the model for complete train data
fit.fnl = lm(Price ~ Rooms + Distance + Bedroom2 + Bathroom + Car + Landsize + BuildingArea + YearBuilt + 
              Suburb_Doncaster + Suburb_Thornbury + Suburb_Hampton + Suburb_Balwyn + Suburb_Camberwell + 
              Suburb_PortMelbourne + Suburb_BrightonEast + Suburb_Hawthorn + Suburb_BalwynNorth + 
              Suburb_Kew + Suburb_Brighton + Suburb_GlenIris + Suburb_SouthYarra + Suburb_Preston + 
              Suburb_BentleighEast + Suburb_Reservoir + Type_u + Type_h + Method_VB + Method_SP + 
              Method_PI + SellerG_Kay + SellerG_Miles + SellerG_RT + SellerG_Marshall + 
              SellerG_hockingstuart + SellerG_Jellis + CouncilArea_Manningham + CouncilArea_Brimbank + 
              CouncilArea_HobsonsBay + CouncilArea_Bayside + CouncilArea_Melbourne + CouncilArea_Banyule + 
              CouncilArea_Yarra + CouncilArea_Maribyrnong + CouncilArea_Stonnington + CouncilArea_Darebin + 
              CouncilArea_MooneeValley + CouncilArea_Moreland + Postcode_3147 + Postcode_3127 + 
              Postcode_3081 + Postcode_3031 + Postcode_3181 + Postcode_3015 + Postcode_3188 + 
              Postcode_3204 + Postcode_3058 + Postcode_3163 + Postcode_3040 + Postcode_3032 + 
              Postcode_3046 + Postcode_3020, data = hg_dp_train)

# vif value which should be less than 5 to remove multi-colinearity
sort(vif(fit_vif), decreasing = T)[1:5]
# vif values are less than 5

# p-values should be less than 0.05 to ensure coefficients are significant
summary(fit.fnl)

# predict for the test data
test.pred = predict(fit.fnl, newdata = hg_dp_test)
# ensure no NA values in test.pred
sum(is.na(test.pred)) # no NA

# write into the folder
getwd()
write.csv(test.pred, "P.Akash_Pattanaik_P1_part2.csv", row.names = F)

#####
