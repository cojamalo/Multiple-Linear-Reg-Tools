
# Title: Multiple Linear Regression Tools
# Date: 2017-04-28
# Author: Connor Lenio
# Dependencies: dplyr, DAAG, AICcmodavg
# Enhances: leaps::regsubsets, stats::AIC, stats::step, DAAG::cv.lm, AICcmodavg::AICc
# Description: Functions to evalute linear fit for the purposes of multiple regression and predictor selection

# Warning: These functions were constructed to assist in learning about linear regression and evaluating linear
# models with the mtcars data set and may or may not be helpful to actual use outside of simple regression using 
# data like mtcars. Currently, they only work with fully numerical variables only. Categorical variables must be 
# converted to numerical variables for the functions to work.

### Please note ###
# Depending on your specific case, it will be better to use alternative methods of model selection such as:
# 1) leaps::resubsets
# best_subsets<- regsubsets(mpg ~ am + ., data = new_mtcars)
# plot(best_subsets, scale = "adjr2")
# plot(best_subsets, scale = "bic")
# plot(best_subsets, scale = "Cp")
# 2) stats::step
# null <- lm(mpg~am, data=new_mtcars)
# full<- lm(mpg ~ ., new_mtcars)   
# best_step <- step(null, scope=list(lower=null, upper=full))
###


## Helper function that takes input such as the following: 
# find_fit(mpg, wt, mtcars, "Log", lm(mpg~log(wt), mtcars))
# Returns: single row data frame with scores for each model analysis metric
#   type terms adj_R_2 BIC AICc LOOCV KFOLD
#    Log     1   0.804 162  158  7.64    NA
find_fit <- function(response, explanatory, data, type, model) {
    library(dplyr)
    data$response <- eval(substitute(response), data)
    data$explanatory <- eval(substitute(explanatory), data)
    out <- tryCatch(model, error = function(e) e)
    if(any(class(out) == "error")) {
        output <- data.frame(type = type, terms = NA, adj_R_2 = NA,BIC=NA,AICc = NA, LOOCV = NA, KFOLD = NA)
    }
    else {
        output <- cbind(type = type, select(model_eval(model, data),-model))
    }
    return(output)
}

## Function that takes input such as the following: 
# compare_fit(mpg, disp, mtcars)
# Returns: multiple row data frame for each transformation type ranked by LOOCV score 
compare_fit <- function(response, explanatory, data) {
    library(dplyr)
    if (class(response) == "character") {
        response <- as.name(response)
        explanatory <- as.name(explanatory)
    }
    data$response <- eval(substitute(response), data)
    data$explanatory <- eval(substitute(explanatory), data)
    output <- rbind(find_fit(response, explanatory, data, "Linear", lm(response~explanatory, data)),
                    find_fit(response, explanatory, data, "Log", lm(response~log(explanatory), data)),
                    find_fit(response, explanatory, data, "Log10", lm(response~log10(explanatory), data)),
                    find_fit(response, explanatory, data, "Log2", lm(response~log2(explanatory), data)),
                    find_fit(response, explanatory, data, "Exponential", lm(response~exp(explanatory), data)),
                    find_fit(response, explanatory, data, "Exp10", lm(response~I(10^explanatory), data)),
                    find_fit(response, explanatory, data, "Exp2", lm(response~I(2^explanatory), data)),
                    find_fit(response, explanatory, data, "Reciprical", lm(response~I(1/explanatory), data)),
                    find_fit(response, explanatory, data, "Square", lm(response~I(explanatory^2), data)),
                    find_fit(response, explanatory, data, "Cube", lm(response~I(explanatory^3), data)),
                    find_fit(response, explanatory, data, "Square Root", lm(response~sqrt(explanatory), data)),
                    find_fit(response, explanatory, data, "Cubic Root", lm(response~I(explanatory^(1/3)), data)))
    output <- output %>% arrange(LOOCV)
    output <- output[complete.cases(output[,-7]),]
    if(any(output$type %in% "Exponential") & round(filter(output, type == "Exponential")$LOOCV[1], digits = 4) == round(output$LOOCV[1], digits = 4)) {
        output <- rbind(filter(output, type == "Exponential"), filter(output, type != "Exponential"))
    }
    if(any(output$type %in% "Log") & round(filter(output, type == "Log")$LOOCV[1], digits = 4) == round(output$LOOCV[1], digits = 4)) {
        output <- rbind(filter(output, type == "Log"), filter(output, type != "Log"))
    }
    if(any(output$type %in% "Linear") & round(filter(output, type == "Linear")$LOOCV[1], digits = 4) == round(output$LOOCV[1], digits = 4)) {
        output <- rbind(filter(output, type == "Linear"), filter(output, type != "Linear"))
    }
    return(output)
}

## Function that takes input such as the following: 
# find_best_trans(mpg, mtcars)
# Returns: multiple-row data frame with the "best" linear transformation for each variable in the data 
find_best_trans <- function(response, data) {
    library(dplyr)
    print("Processing, please wait.....")
    output <- data.frame()
    for (i in 1:ncol(data)) {
            response <- as.character(substitute(response))
            explan <- names(data)[i]
            row <- compare_fit(response, explan, data)[1,]
            row <- cbind(data.frame(variable = explan), row)
            output <- rbind(output, row)
    }
    output <- output[-1,] %>% arrange(LOOCV, AICc) %>% select(-KFOLD)
    return(output)
   
}

## Function that takes output dataframe from find_best_trans function as well as data: 
# find_best_trans(mpg, mtcars)
# Return: Appends the best linear transformation for each explanatory variable  
# to the supplied data frame, will ignore adding an explanatory variable if "linear" is top type
add_best_trans <- function(best_trans_df, data) {
    library(dplyr)
    for (i in 1:nrow(best_trans_df)) {
        row <- best_trans_df[i,]
        var_name <- as.character(row$variable)
        raw_vector <- data[[var_name]]
        switch(as.character(row$type),
               "Log" = {new_col <- log(raw_vector);data <- cbind(data, new_col); names(data)[length(names(data))] <- paste0("log_",var_name)},
               "Log10" = {new_col <- log10(raw_vector); data <- cbind(data, new_col); names(data)[length(names(data))] <- paste0("log10_",var_name)},
               "Log2" = {new_col <- log2(raw_vector); data <- cbind(data, new_col); names(data)[length(names(data))] <- paste0("log2_",var_name)},
               "Exponential" = {new_col <- exp(raw_vector); data <- cbind(data, new_col); names(data)[length(names(data))] <- paste0("exp_",var_name)},
               "Exp10" = {new_col <- 10^(raw_vector); data <- cbind(data, new_col); names(data)[length(names(data))] <- paste0("10^",var_name)},
               "Exp2" = {new_col <- 2^raw_vector; data <- cbind(data, new_col); names(data)[length(names(data))] <- paste0("2^",var_name)},
               "Reciprical" = {new_col <- 1/(raw_vector); data <- cbind(data, new_col); names(data)[length(names(data))] <- paste0("recip_",var_name)},
               "Square" = {new_col <- raw_vector^2; data <- cbind(data, new_col); names(data)[length(names(data))] <- paste0(var_name, "^2")},
               "Cube" = {new_col <- raw_vector^3; data <- cbind(data, new_col); names(data)[length(names(data))] <- paste0(var_name, "^3")},
               "Square Root" = {new_col <- sqrt(raw_vector); data <- cbind(data, new_col); names(data)[length(names(data))] <- paste0("sqrt_",var_name)},
               "Cubic Root" = {new_col <- raw_vector^(1/3); data <- cbind(data, new_col); names(data)[length(names(data))] <- paste0(var_name, "^1/3")}
               ) 
    }
   
    return(data)
}

## Function that takes a model and the data such as: 
# model_eval(fit.BIC, new_mtcars)
# Note: 10x10 K Fold CV is optional to save computation time
# Set kfold = True to include 10x10 K Fold CV
# Return: A single row with scores for each analysis metric 
#                           model terms adj_R_2 BIC AICc LOOCV KFOLD
#  mpg ~ am + recip_hp + log_wt     3   0.882   151  145  5.31  5.81
model_eval <- function(model, data, kfold = FALSE) {
    options(warn=-1)
    # For kfold, m = k for LOOCV
    one_run <- CVlm(data, model, m = nrow(data), printit = FALSE, plotit = FALSE)
    MSE = NA
    if (kfold) {
        # 10x10 K-Fold Cross Validation
        ms <- c()
        for (i in 1:10) {
            ms_run <- CVlm(data, model, m = 10, seed = i, printit = FALSE, plotit = FALSE)
            ms_run <- attributes(ms_run)$ms 
            ms <- c(ms, ms_run) 
        }
        MSE <- mean(ms)
        MSE_hi <- round(1.96 * sqrt(MSE),digits=2)
        MSE_lo <- round(-1.96 * sqrt(MSE),digits=2)
    }
    options(warn=0)
    # Model code
    model_name <- as.character(model)[11]
    # Adjusted R^2
    R_2 <- summary(model)$adj.r.squared
    #Parsimony
    predictors <- length(attributes(summary(model)$terms)$term.labels)
    # AICc Analysis
    AICc <- AICc(model) 
    # BIC Analysis
    BIC <- BIC(model)
    # 5 x 2 Fold Mean Square
    LOOCV <- attributes(one_run)$ms
    # MSE <- mean(ms)
    output <- data.frame(model = model_name, terms = predictors, adj_R_2 = R_2, BIC=BIC, AICc = AICc,LOOCV = LOOCV, KFOLD = MSE) 
    return(output)
}





# Note: Please install DAAG bug fix to use tree_lm with: install_github("gokceneraslan/DAAG")
## Controller Function that takes a base function as a string and the data such as: 
# tree_lm("mpg ~ am", new_mtcars), or
# target argument is which analysis to optimize for
# tree_lm("mpg ~ am", new_mtcars, target = "AICc"), or
# Top_n is the number of top options to further explore at each run of check_models
# tree_lm("mpg ~ am", new_mtcars, top_n = 3), or
# Note: 10x10 K Fold CV is optional to save computation time
# Set kfold = True to include 10x10 K Fold CV
# tree_lm("mpg ~ am", new_mtcars, kfold = TRUE)
# Please use 1 instead of a period in your formula if you want to begin the search with no predictors such as: "mpg ~ 1"
# Return: A data frame with the "best model" found for each number of predictors
# Warning: Optimization is the root of all evil i.e. know why you are optimizing and its costs!
tree_lm <- function(base_function, data, target = c("LOOCV","BIC", "AICc", "KFOLD"), top_n = 3, kfold = FALSE) {
    options(warn=-1)
    library(dplyr)
    library(AICcmodavg)
    library(DAAG)
    print("Processing, please wait....")
    null_mod <- lm(base_function, data)
    #Construct the formulas from a given base and possible variables
    if (target == "KFOLD") {
        kfold = TRUE  
    }
    output <- model_eval(null_mod, data, kfold=kfold)
    best <- output[[target]] 
    output <- check_models(base_function, data, top_n, best, target, kfold)
    output <- arrange(output, output[,target])
    # This checks for any rows that are duplicates of each other:
    duplicates <- duplicated(apply(apply( output, 1, sort), 2 , paste , collapse = ""))
    output <- output[!duplicates,]
    output <- output %>% mutate(rank = rank(output[[target]])) %>% group_by(terms) %>% filter(rank == min(rank))
    output <- output[!duplicated(output$terms),] %>% select(-rank)
    options(warn=0)
    return(output)
}

# Function run inside tree_lm that uses recursion to search a tree of possible predictor combinations to find the "best" options 
# for a given model quality analysis
check_models <- function(base_function, data, top_n, best, target, kfold = FALSE) {
    if (target == "KFOLD") {
      kfold = TRUE  
    }
    null_mod <- lm(base_function, data)
    #Construct the formulas from a given base and possible variables
    output <- model_eval(null_mod, data, kfold = kfold)
    # extract response variable
    params <- trimws(strsplit(base_function, split = "~")[[1]])
    variables <- names(data)
    variables <- variables[!grepl(params[1], variables)]
    # Construct base vector from 
    base <- params[2]
    if (grepl("[+]", params[2])) {
    base <- trimws(strsplit(params[2], split = "[+]")[[1]])     
    }
    # Stop conditional 1 for recursion
    if (length(base) == length(variables)) {
         return(output)  
    }
    # Construct expand_args from base
    expand_args <- c()
    for (item in base) {
        expand_args <- c(expand_args, list(item))
    }
    expand_args <- c(expand_args, list(variables))
    # Construct the combination data frame
    current <- expand.grid(expand_args, stringsAsFactors = FALSE)
    # This checks for any rows that are duplicates of each other:
    duplicates <- duplicated(apply(apply( current, 1, sort), 2 , paste , collapse = ""))
    current <- current[!duplicates,]
    # This removes any rows with the same variable in more than one column
    key <- colSums(apply(apply( current, 1, as.character), 2 , duplicated))
    current <- current[key == 0,] 
    # Construct formulas vector   
    formulas <- apply(current, 1 , paste , collapse = " + ")    
    formulas <- paste0(params[1], " ~ ", formulas)
   # Runs model eval for each of a vector of formula strings
    for (formula in formulas)   { 
        new_mod <- lm(formula, data)
        # Run model_eval on constructed model
        new_row <- model_eval(new_mod, data, kfold=kfold)
        output <- rbind(output, new_row)
    }
    # # Check for duplicates
    # duplicates <- duplicated(apply(apply(output, 1, sort), 2 , paste , collapse = ""))
    # output <- output[!duplicates,]
    # Select top five models
    output <- arrange(output, output[,target])
    output <- filter(output, output[,target] < best)
    best <- output[,target][1]   
    form_length <- top_n - (top_n - nrow(output))
    if (form_length > top_n) {form_length = top_n}
    formulas <- as.character(output[1:form_length,1])
    if (is.na(formulas)) {
     return(output)   
    }
    if (base_function %in% formulas) {
           formulas <- formulas[-which(base_function == formulas)]
    } 
    for (formula in formulas) {
        
          output <- rbind(output, check_models(formula, data, top_n, best, target, kfold))  
    }
    return(output)
}


# Function for ggpairs that compares linear fit with loess smooth
# Helps one visually spot if a linear transformation is useful
# An example call to ggpairs employing my_fn is:
# g <- new_mtcars %>% select(am, recip_hp, log_wt, mpg) %>% ggpairs(lower = list(continuous = my_fn))
my_fn <- function(data, mapping, ...){
    p <- ggplot(data = data, mapping = mapping) + 
        geom_point() + 
        geom_smooth(method=loess, fill="red", color="red", ...) +
        geom_smooth(method=lm, fill="blue", color="blue", ...)
    p
}

## Function timing blocks
#### Troubleshoot ####
start <- Sys.time()
#### Troubleshoot ####

#### Troubleshoot ####     
end <- Sys.time()
print(end-start)
#### Troubleshoot ####
