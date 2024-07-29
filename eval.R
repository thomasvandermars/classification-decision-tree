# Function to score model against data set
score <- function(x, formula, model) {
  
  # model predictions
  predictions <- as.character(predict(model, x, type = "class"))
  
  # actual classes in response variable
  actuals <- unlist(unname(x[all.vars(formula[[2]])]))
  
  return(list(score = mean(predictions == actuals), confusion_matrix = table(predictions = predictions, actuals = actuals)))
}

# Cross Validation 
CV <- function(data, formula, train_frac, n, prune_tree = FALSE, seed = 12345){
  
  # vector to store classification accuracy scores (fractions)
  classification_accuracy <- rep(NA,n) 
  
  # set seed for replicable results
  set.seed(seed)
  
  for (i in 1:n){
    
    # re-partition the data
    data_part <- partition(x = data, fraction_training = train_frac)
    
    # actual classes in response variable
    actuals <- unlist(unname(data_part$test[all.vars(formula[[2]])]))
    
    # create model
    model <- rpart(formula = as.formula(formula), method = "class", data = data_part$train)
    
    # IF tree is unpruned (full model)
    if(prune_tree == FALSE) { 
      
      predictions <- as.character(predict(model, data_part$test, type = "class")) # predict response classes
      classification_accuracy[i] <- mean(predictions == actuals) # mean classification accuracy
      
    } else { # ELSE IF tree is pruned
      
      # find (sub) tree with lowest cross-validation error
      opt <- which.min( model$cptable[,"xerror"] )
      cp  <- model$cptable[opt, "CP"]
      
      # prune the tree
      pruned_model <- prune(model, cp)
      
      predictions <- as.character(predict(pruned_model, data_part$test, type = "class"))  # predict response classes
      classification_accuracy[i] <- mean(predictions == actuals) # mean classification accuracy
      
    }
  }
  return(classification_accuracy)
} # END CV