install.packages("tidyverse")
library(dplyr)
library(readr)

# I added 'Title' columns to the train set with passenger's titles extracted (using Excel's flash fill)
# load 13 columns, ignore any other entries in the file
# 1 sposob
train = read_csv("train.csv")[, 1:13]
test = read_csv("test.csv")

# 2 sposob 
# train_set = read_csv("train.csv", col_types = "iiiccniicncc")

check_missing = function(df) {
  # look for missing values per column
  obs = nrow(df)
  col_names = names(df)
  # get number of missing values per column - result is named vector
  na = sapply( df, function(x) sum(is.na(x)) )
  # get index of cols with NAs
  col_index = which(na > 0)
  # keep only col names with NAs
  col_names = col_names[col_index]
  # add new col with %
  na = data.frame(na)
  na = na %>%
    mutate('%NA' = round(na / obs * 100, 2)) %>%
    filter(na > 0)
  
  # add back row names
  rownames(na) = col_names
  colnames(na) = c("#NA", "%NA")
  na
}

check_missing2 = function(df) {
  temp = colSums(is.na(df))
  print("% of observations missing")
  round(temp[temp > 0] / nrow(df) * 100, 2)
}

# check missing values per column
check_missing(train)

# calc family size and add to data set
train$Family_size = train$SibSp + train$Parch
test$Family_size = test$SibSp + test$Parch

# Fare per person
train$Fare_pp = train$Fare / (train$Family_size + 1)
test$Fare_pp = test$Fare / (test$Family_size + 1)

# check title frequencies and recode them to 4 main categories - this will be age proxy
recode_titles = function(df) {
  table(df$Title)
  df$new_titles = recode(df$Title, Capt. = "Mr.", Col. = "Mr.", Don. = "Mr.", Rev. = "Mr.", Dr. = "Mr.", Mme. = "Mrs.", Ms. = "Mrs.", Major. = "Mr.", 
                            Lady. = "Mrs.", Sir. = "Mr.", Mlle. = "Mr.", Col. = "Mr.", Capt. = "Mr.", "the Countess." = "Mrs.", Jonkheer. = "Mr.")
  table(df$new_titles)
  return(df)
}

train = recode_titles(train)
test = recode_titles(test)



# estimate missing age based on median of respective title
# 1. calculate median age per title
title_age_median = train %>%
    group_by(new_titles) %>%
    summarise(group_median = median(Age, na.rm = T))

#2. Impute missing age values
impute_age = function(df) {
  # create list of median values for every title based on lookup against title_age_median
  medians = title_age_median[match(df$new_titles, title_age_median$new_titles), 2]
  # 1 sposob
  df$age_all = ifelse(is.na(df$Age), as.numeric(unlist(medians)), df$Age)
  
  # 2 sposob
  # df$age_all = df$Age
  # df$age_all[is.na(df$Age)] = medians[is.na(df$Age)]
  
  return(df)
}

train = impute_age(train)
test = impute_age(test)

logit_formula = Survived ~ I(age_all^2) + Sex + Pclass + Family_size + Pclass:Sex + new_titles + Fare_pp
model = glm(lm_formula, family = "binomial", data = train)
test$Survived = predict(model, newdata = test, type = "response")
test$Survived= ifelse(test$Survived >= 0.5, 1, 0)

write.csv(file = "TitanicPred-logit.csv", x = select(test, PassengerId, Survived))

