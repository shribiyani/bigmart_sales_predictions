###### Loading Packages #####

library(data.table) # used for reading and manipulation of data
library(dplyr) # used for data manipulation and joining
library(ggplot2) # used for plotting
library(caret) # used for modelling
library(corrplot) # used for making correlation plot
library(xgboost) # used for building XGBoost model
library(cowplot) # used for combining multiple plots

#######  Reading Data  ########

getwd() # to get file location
?setwd()

train = fread("C:/Users/sharmila_biyani/Documents/Analytics_Vidya/Big_Mart Project - AnalyticsVidya/BigMart_train.csv")
test = fread("C:/Users/sharmila_biyani/Documents/Analytics_Vidya/Big_Mart Project - AnalyticsVidya/BigMart_test.csv")
submission = fread("C:/Users/sharmila_biyani/Documents/Analytics_Vidya/Big_Mart Project - AnalyticsVidya/BigMart_sample_submission.csv")

###### Understanding Data #### 
# Dimensions Of Data i.e., rows and columns

View(train)
View(test)

dim(train); dim(test)

# Features of data i.e. quick glace over feature names of dataset train & test
names(train); names(test)

# Structure of Data which gives a short summary of all the features present in dataframe.
str(train)
summary(train)
str(test)
summary(test)

# Combine train and test >

test[,Item_Outlet_Sales := NA]
combi = rbind(train, test) # combine train & test dataframe
dim(combi)

###### EXPLORATORY DATA ANALYSIS (EDA) ######

### UNIVARIATE EDA ANALYSIS

## TARGET VARIABLE 
  # target variable is continuous and can visualize it by plotting its histogram

ggplot(train)+
  geom_histogram(aes(train$Item_Outlet_Sales), binwidth = 100, fill="darkgreen")+
  xlab("Item_Outlet_Sales")

# As you see, the plot is Right Skewed Variable

# Independent Variables (Numaeric Variables)

p1 = ggplot(combi)+
  geom_histogram(aes(Item_Weight), binwidth = 0.5, fill = "blue")
p2 = ggplot(combi)+
  geom_histogram(aes(Item_Visibility), binwidth = 0.005, fill = "green")
p3 = ggplot(combi)+
  geom_histogram(aes(Item_MRP), binwidth = 1, fill = "red")
plot_grid(p1,p2,p3, nrow = 1) # plot_grid() from cowplot package

# Observations -
  # 1. there is no clear-cut pattern in Item_Weight.
  # 2. Item_Visibility is right skewed and should be transformed to curb its skewness
  # 3. their is clearly see 4 different distributions for Item_MRP. (an interesting insights)

# Independent Variable (Categorical Variables)

# Plot for Item_Fat_Contents

ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n()))+
  geom_bar(aes(Item_Fat_Content,  Count), stat = "identity", fill = "coral1")

# in above plot, 'LF', 'low fat' & "low fat" are same category
# and "reg" and "Regular" are same. So correcting these issues as-

combi$Item_Fat_Content[combi$Item_Fat_Content == "LF"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] = "Regular"

ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n()))+
  geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")


# Checking other categorical Variables as

# plot for Item_Type

p4 = ggplot(combi %>% group_by(Item_Type) %>% summarise(Count = n()))+
  geom_bar(aes(Item_Type, Count), stat = "identity", fill = "coral1")+
  xlab("")+
  geom_label(aes(Item_Type, Count, label = Count), vjust = 0.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Item_Type")

# Plot for Outlet_Identifier

p5 = ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count = n()))+
  geom_bar(aes(Outlet_Identifier, Count), stat = "identity", fill = "blue")+
  xlab("")+
  geom_label(aes(Outlet_Identifier, Count, label = Count), vjust = 0.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Outlet_Identifier")

# Plot for Outlet_Size

p6 = ggplot(combi %>% group_by(Outlet_Size) %>% summarise(Count = n()))+
  geom_bar(aes(Outlet_Size, Count), stat = "identity", fill = "yellow")+
  xlab("")+
  geom_label(aes(Outlet_Size, Count, label = Count), vjust = 0.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Outlet_Size")

second_row = plot_grid(p5, p6, nrow = 1 )
plot_grid(p4, second_row, ncol = 1)

# in Outlet_Size, their is 4016 which is blank or missing. we check for this in Bivariate Analysis

# Plot for Outlet_Establishment_Year

p7 = ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n()))+
  geom_bar(aes(Outlet_Establishment_Year, Count), stat = "identity", fill = "orange")+
  geom_label(aes(Outlet_Establishment_Year, Count, label = Count), vjust = 0.5)+
  theme(axis.text.x = element_text(size = 8.5))+
  ggtitle("Outlet_Estabalish_Year")

# Plot for Outlet_Type

p8 = ggplot(combi %>% group_by(Outlet_Type) %>% summarise(Count = n()))+
  geom_bar(aes(Outlet_Type, Count),  stat = "identity", fill = "green")+
  geom_label(aes(Outlet_Type, Count, label = Count), vjust = 0.5)+
  theme(axis.text.x = element_text(size = 8.5))+
  ggtitle("Outlet_Type")

plot_grid(p7, p8, ncol = 2) # Plotting both together

# Observations as -
  # 1. Lesser number of observations in the data for outlet established in the year 1998 as compared to the other years.
  # 2. SuperMarket Type 1 seems more popular category in Outlet_Type


##### BIVARIATE ANALYSIS ####

# Extracting train data from combined data

train = combi[1:nrow(train)]

# TARGET VARIABLE vs INDEPENDENT NUMERICAL VARIABLE (CONTINUOUS VARIABLE)

# Item_Weight vs Item_Outlet_Sales

p9 = ggplot(train)+
  geom_point(aes(Item_Weight, Item_Outlet_Sales), colour = "violet", alpha = 0.3)+
  theme(axis.title = element_text(size = 8.5))

# Warning message: Removed 1463 rows containing missing values (geom_point).

# Item_Visibility vs Item_Outlet_Sales

p10 = ggplot(train)+
  geom_point(aes(Item_Visibility, Item_Outlet_Sales), colour = "blue", alpha = 0.3)+
  theme(axis.title = element_text(size = 8.5))

# Item_MRP vs Item_Outlet_Sales

p11 = ggplot(train)+
  geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "pink", alpha = 0.3)+
  theme(axis.title = element_text(size = 8.5))

second_row_2 = plot_grid(p10, p11, ncol = 2)
plot_grid(p9, second_row_2, nrow = 2)

# Observation -
  # Item_Outlet_Sales is spread well across the entire range of the Item_Weight without any obvious pattern.
  # in Item_Visibility vs Item_outlet_Sales, there is string of points at Item_Visibility = 0.0, which seems
    # strange as item_visibility cannot completely  zero # note this point for later processing
  # In Item_MRP vs Item_Outlet_Sales, we see clearly 4 segment of prices that can be used in featuring engineering to create new variable


# TARGET VARIABLE vs CATEGORICAL VARIABLE

# we using violin plot instead boxplot- because violin plot show the full distribution of the data.
#   the width of the violin plot at a particular level indicates the concentration or density of data at that level
#   the height of the violin plot tell us the range of the target variable values.

# Item_Type vs Item_Outlet_Sales

p12 = ggplot(train)+
  geom_violin(aes(Item_Type, Item_Outlet_Sales), fill = "magenta")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text = element_text(size = 6),
        axis.title = element_text(size = 8.5))

# Item_Fat_Content vs Item_Outlet_Sales

p13 = ggplot(train)+
  geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales), fill = "purple" )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text = element_text(size = 6),
        axis.title = element_text(size = 8.5))

# Outlet_Identifier vs Item_Outlet_Sales

p14 = ggplot(train)+
  geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales), fill = "grey")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text = element_text(size = 6),
        axis.title = element_text(size = 8.5))

second_row_3 = plot_grid(p13,p14,ncol= 2)
plot_grid(p12, second_row_3, ncol = 1)

# Observation -
  # 1. Distribution of Item_Outlet_Sales across the categories of Item_type is not very distinct
        # and same in the case with Item_Fat_Content.
  # 2. The distribution for OUT010  & OUT019 categories of Outlet_Identifier are quit similar and
        # very much different from the rest of the categories of Outlet_Identifier.

# Outlet_Size vs Item_Outlet_Sales

# In Univariate analysis, we came to know about empty values in Outlet_Size variable. Let's check
      # the distribution of the target variable across Outlet_Size -

ggplot(train)+
  geom_violin(aes(Outlet_Size, Item_Outlet_Sales), fill = "magenta")

  # Observation -
    # 1.  the distribution of "Small" Outlet_Size is almost identical to the distribution of blank
        #  category (first violin) of Outlet_Size. so we substitute the blank with "Small"
    # this in not the only way to impute missing values.

# Outlet_Location_Type vs Item_Outlet_Sales

p15 = ggplot(train)+
  geom_violin(aes(Outlet_Location_Type, Item_Outlet_Sales), fill = 'pink')

# Outlet_Type vs Item_Outlet_Sales

p16 = ggplot(train)+
  geom_violin(aes(Outlet_Type, Item_Outlet_Sales), fill = "green")

plot_grid(p15, p16, ncol = 1)


  # Observations -
    # 1. Tier 1 & Tier 3 locations of Outlet_Location_Type look similar
    # 2. in Outlet_Type plot, Grocery Store has most of its data points around the lower sales values
          # as compared to other categories

##### MISSING VALUE TREATEMENT ####

# Missing data can have a severe impact on building predictive models because missing values might contain
  # some vital information which could help in making better predictions.
# Common Techniques are as -
#   1. Deletion of Rows - in train dataset having missing values in any variable are deleted.
#         in this method we loss of information & drop in prediction model.

#   2. Mean / Median / Mode Imputaion - in continuous / numerical variable, missing vakues can replaced with 
#         mean / median of all known values of that variable. in categorical variable it is replaced by mode.

#   3. Building Prediction Model - can even make a prediction model to impute missing values in variable. Here
#         we treat the variable with missing value as Target VAriable & other variable as Predictors.
#         divid data into 2 datasets => 1. without any missing value (Training Set for prediction model) 
#           & 2. with Missing variable - to predict missing values

# Code to find missing value in a variable

sum(is.na(combi$Item_Weight)) # 2439

# Imputing Missing value in Item_Weight

missing_index = which(is.na(combi$Item_Weight))
for (i in missing_index) {
  item = combi$Item_Identifier[i]
  combi$Item_Weight[i] = mean(combi$Item_Weight[combi$Item_Identifier == item], na.rm = TRUE)
}

sum(is.na(combi$Item_Weight))

# Replacing 0 in Item_Visibility Variable
  # Similary, 0's in Item_Visibility can be replaced with Item_Identifier wise mean values of Item_Visibility

# Visualized in plot as =>

ggplot(combi)+
  geom_histogram(aes(Item_Visibility), bins = 100)

# Replacing 0's with mean

zero_index = which(combi$Item_Visibility == 0)
for (i in zero_index) {
  item = combi$Item_Identifier[i]
  combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier == item], na.rm = TRUE)
}

# After replacing zero we plot Histogram for Item_Visibility as -

ggplot(combi)+
  geom_histogram(aes(Item_Visibility), bins = 100)

##### FEATURE ENGINEERING ####

# Mostly given features in a dataset are not sufficient to give satisfactory predictions.
# So in such case \, we have to create new features which might help in improving the model performance.

# in given dataset, we are creating followin new features - 
# 1. Item_Type_new > broader categories for the variable Item_Type.
# 2. Item_category > categorical variable derived from Item_Identifier
# 3. Outlet_Years > years of operation for outlets
# 4. price_per_unit_wt > Item_MRP / Item_Weight
# 5. Item_MRP_clusters > binning for Item_MRP


# Item_Type to Item_Type_new
  # classify the categories into perishable and non_perishable

table(combi$Item_Type)
perishable = c('Breads', 'Breakfast', 'Dairy', 'Fruits and Vegetables', 'Meat','Seafood')
non_perishable = c('Backing Goods', 'Canned', 'Frozen Foods', 'Hard Drinks', 'Health & Hygiene', 'Household', 'Soft Drinks')

# compare Item_Type with first 2 character of Item_Identifier i.e., 'DR'>drinks , "FD" > food , & "NC" > non-consumable

table(combi$Item_Type, substr(combi$Item_Identifier, 1, 2))

# Based on above table we create a new feature i.e., "Item_category

combi[, Item_category := substr(combi$Item_Identifier, 1, 2)]

# Change values of Item_Fat_Content where Item_category "NC" > due to non-consumable items cannot have any fat content

combi$Item_Fat_Content[combi$Item_category == 'NC'] = "Non-Edible"

# Year of Operation for outlets

combi[, Outlet_Years := 2013 - Outlet_Establishment_Year]
combi$Outlet_Establishment_Year = as.factor(combi$Outlet_Establishment_Year)

# Price per unit per weight

combi[, price_per_unit_wt := Item_MRP/Item_Weight]

# Creating Item_MRP_clusters as new variable

# Earlier in Item_MRP vs Item_Outlet_Sales plot, we saw Item_MRP was spread across in 4 chunks
    # Now assign a label to each of these chunks & use these label as a new variable

combi[, Item_MRP_clusters := ifelse(Item_MRP < 69, '1st',
                                    ifelse(Item_MRP >= 69 & Item_MRP < 136, "2nd",
                                           ifelse(Item_MRP >= 136 & Item_MRP < 203, '3rd', '4th')))]

##### ENCODING CATEGORICAL VARIABLES ####

# Why it is essential?
    # most of the machine learning algorithms produce better result with numerical variables only.
    # so it is essential to treat the categorical variables present in data.
    # this can be done by completely remove categorical variables, but results to loss of enormous information.
  # in this stage, we will convert our categorical into numerical ones.
      # 1. Label Encoding > means converting each category in a variable to number. 
                    # suitable for ordinal variables - categorical variable with some order.
      # 2. One Hot Encoding > each category of a categorical variable is converted into a new binary column (0 / 1)

## LABEL ENCODING FOR THE CATEGORICAL VARIABLES
  # Outlet_Size & Outlet_Location_Type are ordinal variables

combi[, Outlet_Size_num := ifelse(Outlet_Size == "Small",0,
                                  ifelse(Outlet_Size == "Medium", 1, 2))]

combi[,Outlet_Location_Type_num := ifelse(Outlet_Location_Type == "Tier 3", 0,
                                          ifelse(Outlet_Location_Type == "Tier 2", 1, 2))]

# removing categorical variables after label encoding

combi[, c("Outlet_Size", "Outlet_Location_Type") := NULL]

## ONE HOT ENCODING FOR THE CATEGORICAL VARIABLE

ohe = dummyVars("~.", data = combi[, -c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")], fullRank = TRUE)
ohe_df = data.table(predict(ohe, combi[, -c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")]))
combi = cbind(combi[, "Item_Identifier"], ohe_df)


##### DATA PREPROCESSING ####

# WHAT IS DATA PREPROCESSING?
      # it is a pre-processing refers to the transformations applied to your data before feeding to the machine (algorithm).
      # it involves further cleaning of data, data transformation, data scaling and many more things.
# we are dealing with skewness and scale the numerical variables

## REMOVING SKEWNESS -
      # SKEWNESS in variables is undesirables for predictive modeling.
      # some ML methods assume normally distributed data and skewed variable can be transformed by taking its log / square root / cube root
          # to make its distribution as close to normal distribution as possible.

# in our data, variables Item_Visibility & price_per_unit_wt are highly skewed. so treat their skewness by log transformation.

combi[, Item_Visibility := log(Item_Visibility +1)] # log+1 to avoid division by zero
combi[, price_per_unit_wt := log(price_per_unit_wt +1)]


## SCALING NUMERICAL PREDICTORS
      # scale & center the numeric variables to make them have a mean of zero, standard deviation of one & scale of 0 to 1.
      # SCALING & CENTERING is required for linear regression models.

num_vars = which(sapply(combi, is.numeric)) # index of numeric features
num_vars_names = names(num_vars)
combi_numeric = combi[, setdiff(num_vars_names, "Item_Outlet_Sales"), with = F]
prep_num = preProcess(combi_numeric, method = c("center", "scale"))
combi_numeric_norm = predict(prep_num, combi_numeric)

combi[, setdiff(num_vars_names, "Item_Outlet_Sales") := NULL] # removing numeric independent variable
combi = cbind(combi, combi_numeric_norm)

# Splitting the combined data (combi) back to train & test set

train = combi[1:nrow(train)]
test = combi[(nrow(train) + 1):nrow(combi)]
test[, Item_Outlet_Sales := NULL] # removing Item_Outlet_Sales as it contains only NA for test dataset

## CORRELATED VARIABLES
    # examine the correlated features of train dataset. 
    # CORRELATION varies from -1 to +1
          # 1. Negative Correlation =   < 0 & >= -1
          # 2. No Correlation       =     0
          # 3. Positive Correlation =   > 0 & <= +1
# it is not desirable to have correlated features if we using linear regression.

cor_train = cor(train[, -c("Item_Identifier")])
corrplot(cor_train, method = "square", type = "lower", tl.cex = 0.9)
?corrplot

# Observation of Corrplot as -
    # 1. the plot shows correlation between all the possible pairs of variable in data.
    # 2. Corr between any two variable is represented by pie.
    # 3. Blueish pie indicates positive correlation and Reddish pie is negative correlation
    # 4. magnitude of the correlation is denoted by area covered by the pie.
    # 5. variables price_per_unit_wt and Item_Weight are highly correlated as former was created from  latter one.
    # 6. Similarly price_per_unit_wt and Item_MRP are highly correlated for some reason.

##### MODEL BUILDING ####

### LINEAR REGRESSION ###
## Building Model

linear_reg_mod = lm(Item_Outlet_Sales ~., data = train[, -c("Item_Identifier")])
print(linear_reg_mod)

## Making Predictions on test Data
# preparing dataframe for submission and writing it in a csv file

submission$Item_Outlet_Sales = predict(linear_reg_mod, test[, -c("Item_Identifier")])
write.csv(submission, "Linear_Reg_submit.csv", row.names = F)

### REGULARIZED LINEAR REGRESSION ###

  # Regularized Regression models can handel the correlated independent variables well and helps in overcoming overfitting.
  # Ridge penalty shrinks the coefficients of correlated predictors towards each other.
  # Lasso tends to pick one of the pair of correlated features and discard the other.
  # The TUNING parameter LAMBDA controls the strength of the penalty

# LASSO REGREASSION

set.seed(1235)
my_control = trainControl(method = "cv", number = 5)
Grid = expand.grid(alpha = 1, lambda = seq(0.001, 0.1, by = 0.0002))
lasso_linear_reg_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")],
                             y = train$Item_Outlet_Sales,
                             method = "glmnet", trControl = my_control, tuneGrid = Grid)
print(lasso_linear_reg_mod)

# 1 > install.packages("glmnet") > library(glmnet)
library(glmnet)

lasso_linear_reg_mod
# Mean Validation 1130.02 & Leaderboard 1202.26


# RIDGE REGRESSION

set.seed(1234)
my_control_2 =trainControl(method = "cv", number = 5)
Grid_2 = expand.grid(alpha = 0, lambda = seq(0.001, 0.1, by = 0.0002))
ridge_linear_reg_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")],
                             y = train$Item_Outlet_Sales, method = "glmnet", trControl = my_control_2, tuneGrid = Grid_2)

ridge_linear_reg_mod

# RMSE (Mean) 1135.784 Leaderboard 1219.08


### RANDOM FOREST MODEL ###
# RANDOM FOREST is tree based bootstrapping algorithm wherein a certain number of weak learners(decision trees) are combined 
        # to make a powerful prediction model.
# for every individual learner, a random sample of rows and a randomly chosen variables are used to build a Decision Tree Model.
# final prediction can be a function of all the predictions made by the individual learners.
# in case of regression problem, the final prediction can be mean of all the prediction.

# another tuning parameters used here are mtry - no. of predictor variables randomly sampled at each split, and
        # min.node.size - minimum size of terminal nodes (setting this number causes smaller trees and reduces overfitting)

set.seed(1238)
my_control_3 = trainControl(method = "cv", number = 4) # 5 fold CV
tgrid = expand.grid(
  .mtry = c(3:10),
  .splitrule = 'variance',
  .min.node.size = c(10,15,20)
)

# package requiered for followin model = "ranger"
rf_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")],
               y = train$Item_Outlet_Sales,
               method = "ranger",
               trControl = my_control_3,
               tuneGrid = tgrid,
               num.trees = 200,
               importance = "permutation")

rf_mod
# Mean Validation score - 1090.05
# Leaderboard validation score = 1157.52

library(ranger)

# Model Parameters

plot(rf_mod)

# Variable Importantce > plot feature importance based on Random Forrest Model

plot(varImp(rf_mod))

# as expected Item_MRP is the most important variable in predicting the target variable.
# new features created by us like, price_per_unit_wt, Outlet_Years, Item_MRP_clusters, are also among the top most important variables.
# this is why feature engineering plays such a crucial role in predictive modeling

### XGBoost Model ###

# a fast and efficient algorithm & used by many data scientist.
# it is a boosting algorithm
# it works only with numeric variables & replaced categorical variables into numeric variables
# Tuning parameters in XGBoost as -
    # 1. General Parameters - refer to booster using to do boosting. commonly used in tree or linear model.
    # 2. Booster Parameters - depend on which booster you have chosen
    # 3. Learning Task Parameters - which decid on the learning scenario. 
            # ex LT parameter - regression tasks may use different parameters with ranking tasks.

# parameters we going to use in our model as -
  # 1. eta - also known as Leraning Rate / Shrinking Factor. it actually shrinks the feature weights to make the 
          # boosting process more conservative. Then Range is 0 to 1.
          # Low eta value means the model is more robust to overfitting.
  
  # 2. gamma -the range is 0 to infinity(). Larger the gamma more conservative the algorithm.

  # 3. max_depth - we specify maximum depth of a tree using parameter.

  # 4. subsmple - the proportion of rows that the model will randomly select to grow trees.

  # 5. colsample_bytree - the ration of variables randomly chosen to build each tree in the model.

para_list = list(objective = 'reg:linear', eta = 0.01, gamma = 1,
                 max_depth = 6, subsample = 0.8, colsample_bytree = 0.5)

dtrain = xgb.DMatrix(data = as.matrix(train[, -c("Item_Identifier", "Item_Outlet_Sales")]),
                     label = train$Item_Outlet_Sales)
dtrain
dtest = xgb.DMatrix(data = as.matrix(test[, -c("Item_Identifier")]))
dtest

# CROSS VALIDATION

# using xgb.cv() function for cross validation > this function comes with the "xgboost" package.
# here we are using cross validation for finding the optimal value of nrounds.

set.seed(100)
xgbcv = xgb.cv(params = para_list, data = dtrain, nrounds = 500, nfold = 5,
               print_every_n = 10, early_stopping_rounds = 30, maximize = F)

# MODEL TRAINING

# from above, we got best validation / test score at the 430th iteration.
# Hence, we will use nrounds = 430 for building XGBoost model.

xgb_model = xgb.train(data = dtrain, params = para_list, nrounds = 430)
xgb_model

# Leaderboard score = 1154.70

# VARIABLE IMPORTANCE

var_imp = xgb.importance(feature_names = setdiff(names(train), c("Item_Identifier", "Item_Outlet_Sales")), model = xgb_model)
xgb.plot.importance(var_imp)

# again features created by us like, price_per_unit_wt, Outlet_Years, Item_MRP_Clusters, are amonge top most imp. variables








