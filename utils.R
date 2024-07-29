# Normalization is scaling the variables on a range from 0 to 1
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }

# Standardization means that the data set will have a zero mean and a unit variance
standardize <- function(x) { return ( (x - mean(x) ) / ( sd(x) ) ) }

# function to partition the data
partition <- function(x, fraction_training) {
  
  # Training & test fractions
  fractionTraining <- fraction_training
  fractionTest     <- (1 - fractionTraining)
  
  # Compute sample sizes.
  sampleSizeTraining <- floor(fractionTraining * nrow(x))
  sampleSizeTest     <- floor(fractionTest * nrow(x))
  
  # Create the randomly-sampled indices for the data frame. Use setdiff() to avoid overlapping subsets of indices.
  indicesTraining <- sort( sample( seq_len(nrow(x)), size = sampleSizeTraining ) )
  indicesTest     <- setdiff( seq_len(nrow(x)), indicesTraining )
  
  # Finally, output the three data frames for training and test.
  return(list(train = x[indicesTraining, ], test = x[indicesTest, ]))
  
} # END partition FUNCTION

# function to handle missing data
missing_data_handling <- function(x, method = c("omit", "mean")) {
  
  # IF any records with missing values exists...
  if( sum(apply(x, 1, function(y){ any(is.na(y)) }) > 0) ) { 
    
    if( method == "omit" ) {
      
      # omit records with missing (NA) values
      x <- na.omit(x)
      
    } else if ( method == "mean" ) {
      
      # replace missing values with variable (column) mean
      for(i in 1:ncol(x)){ 
        if(is.numeric(unlist(x[,i]))) {
          var_miss_row_inds <- is.na(x[,i])
          x[var_miss_row_inds, i] <- mean(unlist(x[,i]), na.rm = TRUE)  
        }
      }
    }
  }
  return(x)
} # END missing_data_handling FUNCTION

compute_accuracy <- function(model, x, y) {
  # COMPUTE CLASSIFICATION ACCURACY
  y_hat <- predict(nn, x)
  y_hat <- max.col(y_hat)
  y <- max.col(y)
  return(mean(y == y_hat))
}
