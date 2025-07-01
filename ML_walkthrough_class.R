require(tidyverse)
require(caret) # for machine learning
require(recipes) # For preprocessing your data
require(rsample) # for train test splits
require(rattle) # For nice tree plots
require(yardstick) # for performance metrics of different ML models. 

# For parallelization (to run the models across many cores) -- speeds up computation!
# install.packages("doMC")
#doMC::registerDoMC()


# Data

#For this exercise, let's examine historical data of bike sharing in London downloaded from Kaggle. The data contains information on rider usage given a common set of climate and time predictors. 
# Assume we work for the company that runs the ride sharing venture: our aim is to build a model that best predicts the number of riders (`cnt`) at any given moment in time so that we can distribute resources appropriately. Please see the Kaggle site for a description of the variables: https://www.kaggle.com/hmavrodiev/london-bike-sharing-dataset#london_merged.csv

setwd("~/Downloads/School/POLI 178")
dat = read_csv("london_bike_sharing.csv")
glimpse(dat)

#Let's look at the temporal coverage: Looks like the data roughly covers a two year time span.

dat %>% 
  summarize(min_date = min(timestamp),
            max_date = max(timestamp))

# Workflow

#+ Wrangle (assumed)
#+ Split sample.
#+ Inspect
#+ Pre-process. 
#+ Select model.
#+ run model.
#+ iterate/validate/compete. 



# Split the Sample: Training and test data

#Before event looking at the data, let's split the sample up into a training and test dataset. We'll completely hold off on viewing the test data, so as not to bias our development of the learning model. 

set.seed(123)
splits = initial_split(dat,prop = .8)
train_data = training(splits) # Use 80% of the data as training data 
test_data = testing(splits) # holdout 20% as test data 

dim(train_data)
dim(test_data) 


# Inspect the Data

summary(train_data)


#Visualize the distribution for each variable.

#First, let's look at the numerical variables.

train_data %>% 
  select_if(is.numeric) %>% 
  gather(var,val) %>% # equivalent to pivot_longer
  ggplot(aes(val,group=var)) +
  geom_histogram(bins = 30) +
  facet_wrap(~var,scales="free",ncol=2)

#Most of the predictor variables are continuous values. Some variables appear to be dummy (`is_weekend`) or ordered (`season`) variables. For the seasons, we may want to consider changing this variable to a dummy variable (where each season acts as a dummy variable) since the "order" in the seasons isn't implied (i.e. winter isn't necessarily worse than Fall, etc.).


#We want to pay special attention to the  **outcome variable**. Notice it is a left-skewed count, so we may want to log transform it so that it's less skewed.
                                                                                                                                                                                                                                                                                                                          
#Now let's see if the predictors are correlated:

train_data %>% select(-c(cnt,timestamp)) %>%
  cor()

#Most of the variables are pretty good. However, `t1` (actual temp), and `t2` (temp feeling) are very correlated. We may want to think about dropping one of them. 

# This may cause us to think about what the other variables are about. The variable `weather_code` is not well-defined. This variable could be a composite measure of the other weather variables.  If that's true, we can't include it and the other weather variables. Since we just don't know what it means, we may want to drop it. 
                                                                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                                                              
                                                                                                                                                                                                                                                                                                                          Turning to missingness:
# Pre-process the Data 

#If we are running run model, we can simply pre-process the data. We want to run **many** ML models and pick the best one. 

#Our goal: Write a script that ensures the data are well processed for every model. You can sort of think about this as a step in the loop. 


#What do we need to do?

#- Seasons variable to dummies 
#- Scale all the variables so that they fall into a 0 to 1 range.
#- No need to impute missing values. All the data is there.




# First, turn the season variable into a categorical variable
clean_steps = function(.data){
  .data %>% 
  mutate(season = as.factor(season)) %>% 
  select(-timestamp,-weather_code)
} 

# Generate our recipe to preprocess the data 
rcp <- 
  recipe(cnt~.,data = train_data %>% clean_steps) %>% 
  step_dummy(all_nominal(),-cnt) %>% # log the dependent variable so it's more normal, offset the count by 1 for instances when cnt==0
step_log(cnt,offset=1) %>% 
step_range(all_numeric(),-cnt) %>%  # Normalize scale
prep()

# Apply the recipe to the training and test data
train_data2 <- bake(rcp,train_data %>% clean_steps)
test_data2 <- bake(rcp,test_data%>% clean_steps) # Need to transform the seasons data here as well. 

#Let's examine the post-processed data.

train_data2 %>% glimpse()


train_data2 %>% 
  select(cnt,hum,wind_speed,t1,t2) %>% 
  gather(var,val) %>% 
  ggplot(aes(val,group=var)) +
  geom_histogram(bins = 30) +
  facet_wrap(~var,scales="free",ncol=3)

# Cross-validation

#When comparing different machine learning models, we want to make sure we're making a fair and equal comparison. One way that random chance can sneak into our assessments of model fit is through cross-validation. 

#Remember, cross-validation requires that we
# + Build a ML model on one sample
# + Test the predictive accuracy of that model on a different sample.
# + Pick the model that has the best predictive accuracy. 
 
#We want to make sure that we're cross-validating the data on the _exact same data partitions_. 

#`caret` makes this ease to do. Let's use the k-fold cross-validation method with 10 folds in the data. 

set.seed(1988) # set a seed for replication purposes 
folds <- createFolds(train_data2$cnt, k = 10) # Partition the data into 10 equal folds
sapply(folds,length)

#Now, let's use the `trainControl()` function from `caret` to set up our validation conditions


control_conditions <- 
  trainControl(method='cv', # K-fold cross validation
               index = folds # The indices for our folds (so they are always the same)
  )


#We'll now use this same cross-validation object for everything that we do. 
                                                                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                                                                          # Models
## Linear Regression


mod_lm <-
  train(cnt ~ .,          # Equation (outcome and everything else)
        data=train_data2, # Training data 
        method = "lm",    # linear model
        metric = "RMSE",   # mean squared error
        trControl = control_conditions # Cross validation conditions
  )


#Let's look at the performance plots. What do we see?

mod_lm

## K-Nearest Neighbors
# Remember: these algorithms are computationally demanding, so they can take a little time to run depending on the power of your machine

mod_knn <- train(cnt ~ .,           # Equation (outcome and everything else)
data=train_data2,  # Training data 
method = "knn",    # K-Nearest Neighbors Algorithm
metric = "RMSE",   # mean squared error
trControl = control_conditions # Cross validation conditions
)

#Let's look at the performance plots. What do we see?

mod_knn


#- `caret` has default settings that auto explore different tunning parameters for the model. 
#- We can easily plot these tuning features using the standard plot functions (these function have been over-ridden in R)


plot(mod_knn)

# Draw out the best model (the one with the best performance)
mod_knn$finalModel

#Now, let's say we wanted to adjust the tun\ning parameters. We can explore different model tunings by using the `tuneGrid` argument. 
                                                                                                                                                                                                                                                                                                                          
# Different values of the tuning parameter that I want to try.

knn_tune = expand.grid(k = c(1,3,10,50))

mod_knn <-
train(cnt ~ .,           # Equation (outcome and everything else)
data=train_data2,  # Training data 
method = "knn",    # K-Nearest Neighbors Algorithm
metric = "RMSE",   # mean squared error
trControl = control_conditions, # Cross validation conditions
tuneGrid = knn_tune # Vary the tuning parameter K 
)
                                                                                                                                                                                                                                                                                                                          ```
#Look at performance: it doesn't look that we get much of a boost when going all the way to K-Neighbors.

plot(mod_knn)

################################
## Decision Trees
##########################

mod_cart <-
   train(cnt ~ .,            # Equation (outcome and everything else)
        data=train_data2,    # Training data 
        method = "rpart",    # Regression tree
        metric = "RMSE",     # mean squared error
        trControl = control_conditions # Cross validation conditions
  )


mod_cart

#Note that the main tuning parameter here is the complexity parameter (`cp`): this reflect how deep we let the tree grow. It looks like the more complex models perform worse on out-of-sample data. 

plot(mod_cart)

#Let's visualize the tree we just grew. It looks like the humidity level is an important factor when predicting othe number of riders. 

# This tree goes really deep
fancyRpartPlot(mod_cart$finalModel)

#We can actually print out the larger decision tree to look at it as a print out.
                                                                                                                                                                                                                                                                                                                          print(mod_cart$finalModel)
#Let's free the CART to grow deeper.
tune_cart <- expand.grid(cp = c(0.0010281)) # Complexity Parameter (how "deep" our trees should grow)
mod_cart2 <-
  train(cnt ~ ., # Equation (outcome and everything else)
        data=train_data2, # Training data 
        method = "rpart", # Classification Tree
        metric = "RMSE",     # mean squared error
        tuneGrid = tune_cart, # Tuning parameters
        trControl = control_conditions
  )


#Doesn't do a whole lot better on out of sample data. 

print(mod_cart2)

#Let's look at the tree itself: much deeper!
print(mod_cart2$finalModel)
fancyRpartPlot(mod_cart2$finalModel)


########################
## Random Forest
###############################
mod_rf <-
  train(cnt ~ ., # Equation (outcome and everything else)
        data=train_data2, # Training data 
        method = "ranger", # random forest (ranger is much faster than rf)
        metric = "RMSE",     # mean squared error
        trControl = control_conditions
  )


#There are three tuning parameters in this model:

#- `mtry`: the number of predictors that we'll randomly select.
#- `splitrule`: the way we determine how the nodes should be split
 #- `variance` = takes the split that maximizes the variance (minimizes the error).
#- `extratrees` = doesn't bag, randomly select vars, but just randomly split, accept the best split. (Short for "Extremely Randomize Trees")
#- 'min.node.size': the minimum number of observations that can be in each terminal node (default fixes this at 1)



mod_rf


plot(mod_rf)

mod_rf$bestTune


# Model Comparison

#How did the different methods perform? Which one did the best?


# Organize all model inputs as a list.
mod_list <-
  list(
    lm = mod_lm,
    knn = mod_knn,
    cart = mod_cart,
    cart_deep = mod_cart2,
    rf = mod_rf 
  )

# Resamples allows us to compare model output
resamples(mod_list)


#Examine the error across each of the models.

dotplot(resamples(mod_list),metric = "RMSE")


Examine the fit across each of the models.

dotplot(resamples(mod_list),metric = "Rsquared")


# Test the Predictive Accuracy of the Best Model 

#Of the models we ran, it looks like the Random Forest model did the best. Let's now see how well it does on predicting the test data.

pred <- predict(mod_rf,newdata = test_data2)
mse = sum((test_data2$cnt-pred)^2)/nrow(test_data2)
rmse_score = sqrt(mse)
rmse_score

#We can also use the `yardstick` package to calculate performance metrics.
# Generate a prediction on our test data. 
pred <- predict(mod_rf,newdata = test_data2)

# Organize as a data frame
performance = tibble(truth=test_data2$cnt,estimate = pred)

# Calculate performance metrics
bind_rows(
performance %>% rmse(truth,estimate), # Root Mean Squared Error
performance %>% rsq(truth,estimate) # R Squared
)

