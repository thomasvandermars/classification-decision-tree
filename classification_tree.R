# load libraries
library(here)
library(readxl)
library(rlang)
library(rpart)
library(rpart.plot)

# source files
source(here("utils.R"))
source(here("eval.R"))

######### IMPORT DATA #########
data <- read_excel(here("data","Loan Data.xlsx"))

######### REMOVE UNWANTED VARIABLES #########
#data[c("name1, name2")] <- NULL

######### REMOVE SPACES FROM VARIABLE NAMES #########
names(data) <- sub(" ", "_", names(data))

######### MISSING DATA #########
data <- missing_data_handling(data, "omit")

######### RESPONSE VARIABLE #########
target_variable = 'Personal_Loan'

data[target_variable] <- factor(unname(unlist(data[target_variable])), 
                         levels = c(0, 1), 
                         labels = c('Declined', 'Accepted') )

######### FORMULA #########
f <- as.formula(paste(colnames(data[target_variable]), 
                      " ~ ", 
                      paste(colnames(data[-which(colnames(data) == target_variable)]), 
                            collapse = "+")))

######### set seed for reproducible results #########
set.seed(12345)

######### partition #########
part <- partition(x = data, fraction_training = 0.6)

######### full model #########
fm <- rpart(formula = as.formula(f), method = "class", data = part$train)
fm_train <- score(x = part$train, formula = as.formula(f), model = fm)
fm_test  <- score(x = part$test, formula = as.formula(f), model = fm)

######### pruned model #########
pm <- prune(fm, fm$cptable[ which.min( fm$cptable[,"xerror"] ), "CP" ])
pm_train <- score(x = part$train, formula = as.formula(f), model = pm)
pm_test  <- score(x = part$test, formula = as.formula(f), model = pm)


######### Cross Validating - Full Tree vs. Pruned Tree #########
fm_forest <- CV(data = data, formula = as.formula(f), train_frac = 0.6, n = 50, prune_tree = FALSE, seed = 500)
pm_forest <- CV(data = data, formula = as.formula(f), train_frac = 0.6, n = 50, prune_tree = TRUE, seed = 500)


######### calculate and plot cross validation on test data - classification accuracy ######### 
plot_acc <- data.frame(ClassAcc = c(fm_forest, pm_forest), 
                       Model = c(rep(paste("Full (",length(fm_forest),"/",length(fm_forest),")", sep=""),length(fm_forest)),
                                 rep(paste("Pruned (", sum(fm_forest != pm_forest),"/",length(fm_forest),")", sep=""),length(pm_forest))) )

boxplot(ClassAcc~Model, 
        data = plot_acc, 
        xlab = paste("Model (",length(fm_forest),")", sep=""), 
        ylab = 'Test Data Classification Accuracy', 
        col = 'cyan', 
        border = 'blue', 
        main = 'Test Data Classification Accuracy')

######### calculate and plot cross validation on test data - error rates ######### 
plot_err <- data.frame(ClassErr = 1 - c(fm_forest, pm_forest), 
                       Model = c(rep(paste("Full (",length(fm_forest),"/",length(fm_forest),")", sep=""),length(fm_forest)),
                                 rep(paste("Pruned (", sum(fm_forest != pm_forest),"/",length(fm_forest),")", sep=""),length(pm_forest))) )
boxplot(ClassErr~Model, 
        data = plot_err, 
        xlab = paste("Model (",length(fm_forest),")", sep=""), 
        ylab = 'Test Data Misclassification Rate', 
        col = "orange", 
        border = "brown", 
        main = 'Test Data Misclassification')


######### Plot Full Tree - complete information ######### 
rpart.plot(fm, # rpart object 
           type = 2, # default is 2 (most info) -- easiest to read is 5
           extra = 102, # default is 102 
           tweak = 1.1, # makes text larger or smaller
           main = "New Loan Requests - Classification Tree\nFull Tree (98.35% test accuracy) [Complete Information]",
           box.palette = c("red1", "red3", "green3", "green1"),
           shadow.col = "lightgray")

######### Plot Full Tree - easiest to read ######### 
rpart.plot(fm, # rpart object 
           type = 5, # default is 2 (most info) -- easiest to read is 5
           extra = 8, # default is 102 
           tweak = 1.1, # makes text larger or smaller
           main = "New Loan Requests - Classification Tree\nFull Tree (98.35% test accuracy) [Readability Focus]",
           box.palette = c("red1", "red3", "green3", "green1"),
           shadow.col = "lightgray")

######### Plot Pruned Tree - complete information ######### 
rpart.plot(pm, # rpart object 
           type = 2, # default is 2 (most info) -- easiest to read is 5
           extra = 102, # default is 102 
           tweak = 1.1, # makes text larger or smaller
           main = "New Loan Requests - Classification Tree\nPruned Tree (98.25% test accuracy) [Complete Information]",
           box.palette = c("red1", "red3", "green3", "green1"),
           shadow.col = "lightgray")

######### Plot Pruned Tree - easiest to read ######### 
rpart.plot(pm, # rpart object 
           type = 5, # default is 2 (most info) -- easiest to read is 5
           extra = 8, # default is 102 
           tweak = 1.2, # makes text larger or smaller
           main = "New Loan Requests - Classification Tree\nPruned Tree (98.25% test accuracy) [Readability Focus]",
           box.palette = c("red1", "red3", "green3", "green1"),
           shadow.col = "lightgray")

######### New Loan Requests Prediction ######### 

# import the data of the new applicants
data_new <- read_excel(here("data","New Loan Requests.xlsx"))

# predict using the pruned classification tree (since it has almost the same accuracy as the full tree)
data_new <- cbind(y_hat = as.character(predict(object = pm, newdata = data_new, type = "class")), data_new)
