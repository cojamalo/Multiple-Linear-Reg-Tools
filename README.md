Title: Multiple Linear Regression Tools

Date: 2017-04-28

Author: Connor Lenio

Dependencies: dplyr, DAAG, AICcmodavg

Enhances: leaps::regsubsets, stats::AIC, stats::step, DAAG::cv.lm, AICcmodavg::AICc

Description: Functions to evalute linear fit for the purposes of multiple regression and predictor selection

Warning: These functions were constructed to assist in learning about linear regression and evaluating linear models
with the mtcars data set and may or may not be helpful to actual use outside of simple regression using data like mtcars. Currently, they only work with fully numerical variables only. Categorical variables must be converted to numerical variables for the functions to work.

### Please note ###
Depending on your specific case, it will be better to use alternative methods of model selection such as:
1) leaps::resubsets

   best_subsets<- regsubsets(mpg ~ am + ., data = new_mtcars)
   plot(best_subsets, scale = "adjr2")
   plot(best_subsets, scale = "bic")
   plot(best_subsets, scale = "Cp")
   
2) stats::step

   null <- lm(mpg~am, data=new_mtcars)
   full<- lm(mpg ~ ., new_mtcars)   
   best_step <- step(null, scope=list(lower=null, upper=full))

* * *

* find_fit <- function(response, explanatory, data, type, model)

Helper function that takes input such as the following: 
find_fit(mpg, wt, mtcars, "Log", lm(mpg~log(wt), mtcars))
Returns: single row data frame with scores for each model analysis metric
type terms adj_R_2 BIC AICc LOOCV KFOLD
      Log     1   0.804 162  158  7.64    NA

* compare_fit <- function(response, explanatory, data)

Function that takes input such as the following: 
 compare_fit(mpg, disp, mtcars)
 Returns: multiple row data frame for each transformation type ranked by LOOCV score 

* find_best_trans <- function(response, data)

 Function that takes input such as the following: 
 find_best_trans(mpg, mtcars)
 Returns: multiple-row data frame with the "best" linear transformation for each variable in the data 

* add_best_trans <- function(best_trans_df, data)

 Function that takes output dataframe from find_best_trans function as well as data: 
 find_best_trans(mpg, mtcars)
 Return: Appends the best linear transformation for each explanatory variable  
 to the supplied data frame, will ignore adding an explanatory variable if "linear" is top type

* model_eval <- function(model, data, kfold = FALSE)

 Function that takes a model and the data such as: 
 model_eval(fit.BIC, new_mtcars)
 Note: 10x10 K Fold CV is optional to save computation time
 Set kfold = True to include 10x10 K Fold CV
 Return: A single row with scores for each analysis metric 
                           model terms adj_R_2 BIC AICc LOOCV KFOLD
  mpg ~ am + recip_hp + log_wt     3   0.882   151  145  5.31  5.81
  
* tree_lm <- function(base_function, data, target = c("LOOCV","BIC", "AICc", "KFOLD"), top_n = 3, kfold = FALSE)

 Note: Please install DAAG bug fix to use tree_lm with: install_github("gokceneraslan/DAAG")
 Controller Function that takes a base function as a string and the data such as: 
 tree_lm("mpg ~ am", new_mtcars), or
 target argument is which analysis to optimize for
 tree_lm("mpg ~ am", new_mtcars, target = "AICc"), or
 Top_n is the number of top options to further explore at each run of check_models
 tree_lm("mpg ~ am", new_mtcars, top_n = 3), or
 Note: 10x10 K Fold CV is optional to save computation time
 Set kfold = True to include 10x10 K Fold CV
 tree_lm("mpg ~ am", new_mtcars, kfold = TRUE)
 Please use 1 instead of a period in your formula if you want to begin the search with no predictors such as: "mpg ~ 1"
 Return: A data frame with the "best model" found for each number of predictors
 Warning: Optimization is the root of all evil i.e. know why you are optimizing and its costs!

* check_models <- function(base_function, data, top_n, best, target, kfold = FALSE)

 Function run inside tree_lm that uses recursion to search a tree of possible predictor combinations to find the "best" options 
 for a given model quality analysis

* my_fn <- function(data, mapping, ...)

 Function for ggpairs that compares linear fit with loess smooth
 Helps one visually spot if a linear transformation is useful
 An example call to ggpairs employing my_fn is:
 g <- new_mtcars %>% select(am, recip_hp, log_wt, mpg) %>% ggpairs(lower = list(continuous = my_fn))

Project coded by Connor Lenio ©2017.
