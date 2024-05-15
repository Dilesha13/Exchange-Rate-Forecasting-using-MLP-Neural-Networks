# import libraries
library(readxl)
library(dplyr) 
library(neuralnet)
library(Metrics)
library(grid)
library(gridExtra)

# import dataset
exchange_rate_data <- read_excel("C:/Users/Tharushi/IIT/IIT/2nd yr/2nd semester/ML/CW/R/ML_CW/ExchangeUSD (2).xlsx")
#View(data)

# Select numeric columns from exchange_rate_data
exchange_rate_data <- select_if(exchange_rate_data, is.numeric)

# Create a boxplot of the data before outlier removal
boxplot(exchange_rate_data, main = "Before Outlier Removal", outcol="red")

# Before removing the outliers, count the rows and columns and print the results.

rows_of_dataset <- nrow(exchange_rate_data)
columns_of_dataset <- ncol(exchange_rate_data)

# print the results
cat("Rows to delete before outliers   :", rows_of_dataset, "\n")
cat("columns to delete before outliers:", columns_of_dataset, "\n")

# Create a function to remove outliers from a single column using the boxplot method.

outliers_remove <- function(x) {
  bp <- boxplot.stats(x)$stats
  x[x < bp[1] | x > bp[5]] <- NA
  return(x)
}

# Apply the function to each data frame column.

whithout_outliers_in_dataset <- apply(exchange_rate_data, 2, outliers_remove)

# Remove any rows with missing values

whithout_outliers_in_dataset <- na.omit(whithout_outliers_in_dataset)

# After removing the outliers, count the rows and columns and print the results.

rows_of_dataset_after_delete_outliers <- nrow(whithout_outliers_in_dataset)
columns_of_dataset_after_delete_outliers <- ncol(whithout_outliers_in_dataset)

# print the results
cat("After deleting outliers, Number of Rows   :", rows_of_dataset_after_delete_outliers, "\n")
cat("After deleting outliers, Number of Columns:", columns_of_dataset_after_delete_outliers, "\n")

# Create a boxplot of the data after outlier removal
boxplot(whithout_outliers_in_dataset,main = "After Outlier Removal")

# Convert the vector whithout_outliers_in_dataset to a dataframe
whithout_outliers_df <- as.data.frame(whithout_outliers_in_dataset)

# Access the column USD/EUR
exchange_rate <- whithout_outliers_df$'USD/EUR'
plot(exchange_rate, type = "l")

# Split data into training and testing sets (400 for training)
train_size <- 400
training_data <- exchange_rate[1:train_size]
testing_data <- exchange_rate[(train_size + 1):length(exchange_rate)]

# create a I/O  matrix
time_delayed_matrix <- bind_cols(
  t4 = lag(exchange_rate,5),
  t3 = lag(exchange_rate,4),
  t2 = lag(exchange_rate,3),
  t1 = lag(exchange_rate,2),
  exchange_rateRate = exchange_rate) 
delayed_matrix <- na.omit(time_delayed_matrix)
#View(delayed_matrix)

# Separating dataset into test and training sets.
train_dataset <- delayed_matrix[1:400,]
test_dataset <- delayed_matrix[401:nrow(delayed_matrix),]

# Determining the minimum and maximum values in the training set
first_min_value <- min(train_dataset)
first_max_value <- max(train_dataset)

# Extracting the output data from the testing dataset
first_data_output <- test_dataset$exchange_rateRate

# normalization function
normalization <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}

# Un-normalization function
un_normalization <- function(x, min, max) {
  return( (max - min)*x + min )
}

# The normalization function is used to normalise the data.
time_delayedNormaliz <- as.data.frame(lapply(delayed_matrix[1:ncol(delayed_matrix)], normalization))

# Separating the normalised data into test and train sets
train_datasetNormaliz <- time_delayedNormaliz[1:400,]
test_datasetNormaliz <- time_delayedNormaliz[401:nrow(delayed_matrix),]

# See how the data looks like before and after normalisation with a Boxplot
boxplot(delayed_matrix, main="before normalizing data")
boxplot(time_delayedNormaliz, main="After normalizing data")

# Producing Test Information for Each and EveryDelay
t1_testDataSet <- as.data.frame(test_datasetNormaliz[, c("t1")])
t2_testDataSet <- test_datasetNormaliz[, c("t1", "t2")]
t3_testDataSet <- test_datasetNormaliz[, c("t1", "t2", "t3")]
t4_testDataSet <- test_datasetNormaliz[, c("t1", "t2", "t3", "t4")]

#Function to train an AR model
ModelTrain <- function(formula, hiddenVal, isLinear, actFunc,inputs,hidden){
  
  # Defining a title for the plot
  my_title <- paste(inputs,"inputs and",length(hidden),"hidden layers","(",paste(hidden, collapse=","),") \n")
  
  # Establishing a reproducible seed
  set.seed(1234)
  
  # The neuralnet method is used to train a model of a neural network.
  nural <- neuralnet(formula, data = train_datasetNormaliz, hidden = hiddenVal, act.fct = actFunc, linear.output = isLinear)
  #pdf("neural_network_plot.pdf")
  # Using a neural network to generate a plot
  plot(nural)
  
  # Keeping the plot in a variable
  plot_panel <- grid.grab(wrap = TRUE)
  
  # Creating a title grob
  plot_title <- textGrob(my_title,
                         x = .5, y = .20,
                         gp = gpar(lineheight_hours = 2,
                                   fontsize = 15, col = 'red',
                                   adj = c(1, 0)
                         )
  )
  # Stacking the title and main panel, and plotting
  grid.arrange(
    grobs = list(plot_title,
                 plot_panel),
    height_hourss = unit(c(.15, .85), units = "npc"),
    width = unit(1, "npc")
  )
  dev.new()
  dev.off()
  # Return the trained neural network model
  return(nural)
}

# Define function ModelTest with input arguments nuralModel, testing_df, inputs, hidden
ModelTest <- function(nuralModel, testing_df, inputs, hidden){
  
  # Print the number of inputs and hidden layers specified
  cat("There are", inputs, "inputs and", length(hidden), "hidden layers", "(", paste(hidden, collapse=","), ") \n")
  
  # Create a title for the plots based on the inputs and hidden layers specified
  my_title <- paste(inputs, "inputs and", length(hidden), "hidden layers", "(", paste(hidden, collapse=","), ") \n")
  
  # Use the test data to calculate the output of the neural network.
  nnResults <- compute(nuralModel, testing_df)
  
  # Extract the predicted values and un-normalize them using the min and max values of the original data
  predict <- nnResults$net.result
  unuralormalised_predict <- un_normalization(predict, first_min_value, first_max_value)
  
  # Find the difference between your expected results and the actual ones.
  devia = ((first_data_output - unuralormalised_predict) / first_data_output)
  
  # Calculate the model accuracy as 1 minus the absolute mean deviation
  modelAccu = 1 - abs(mean(devia))
  accuracy = round(modelAccu * 100, digits = 2)
  
  # Plot the predicted vs. actual output values with the un-normalized data
  plot(first_data_output, unuralormalised_predict, col = 'green', main = "Un-normalized Prediction Graph", pch = 18, cex = 0.7)
  mtext(my_title,  side = 3, line = 2, cex = 0.8)
  abline(0,1,lwd=2)
  legend("bottomright", legend = 'neural', pch = 18, col = 'green')
  
  # Create a new plot showing the original output values vs. the predicted values with un-normalized data
  x = 1:length(first_data_output)
  plot(x, first_data_output, col = "red", type = "l", lwd=2, main = "Concrete Strength Prediction")
  mtext(my_title,  side = 3, line = 2, cex = 0.8)
  lines(x, unuralormalised_predict, col = "blue", lwd=2)
  legend("topright", legend = c("original-strength", "predicted-strength"), fill = c("red", "blue"), col = 2:3, adj = c(0, 0.6))
  grid()
  
  # Calculate the RMSE, MAE, MAPE, and sMAPE metrics for the model
  rmse = rmse(first_data_output, unuralormalised_predict)
  mae = mae(first_data_output, unuralormalised_predict)
  mape = mape(first_data_output, unuralormalised_predict)
  smape = smape(first_data_output, unuralormalised_predict)
  
  # Get the model's RMSE, MAE, MAPE, and sMAPE values.
  cat("Model Accuracy:", accuracy, "%\n")
  cat("RMSE:", rmse, "\n")
  cat("MAE:", mae, "\n")
  cat("MAPE:", mape, "\n")
  cat("sMAPE:", smape, "\n")
  cat("\n\n")
  
  # Return the un-normalized predicted output values
  return(unuralormalised_predict)
}



# t1 with different hidden layer sizes
hidden_layers <- list( c(6),c(5, 3),c(5,7))

# loop through different hidden layer sizes
for (i in seq_along(hidden_layers)) {
  # train model using t1 as input and current hidden layer size
  model <- ModelTrain(exchange_rateRate ~ t1, hidden_layers[[i]], isLinear = FALSE, "tanh",1,hidden_layers[[i]])
  # test model on t1_testDataSet
  pred <- ModelTest(model, t1_testDataSet,1,hidden_layers[[i]])
}



# t2 with different hidden layer sizes
hidden_layers <- list( c(10),c(4,3),c(6,4))
for (i in seq_along(hidden_layers)) {
# train model using t1 and t2 as input and hidden layer size of 10
model <- ModelTrain(exchange_rateRate ~ t1 + t2, hidden_layers[[i]],isLinear = TRUE, "logistic",2,hidden_layers[[i]])
# test model on t2_testDataSet
pred <- ModelTest(t2_train, t2_testDataSet,2,hidden_layers[[i]])
}


# t3 with different hidden layer sizes
hidden_layers <- list( c(5),c(5,4),c(10))

# loop through different hidden layer sizes
for (i in seq_along(hidden_layers)) {
  # train model using t1, t2, and t3 as input and current hidden layer size
  model <- ModelTrain(exchange_rateRate ~ t1 + t2 + t3 ,hidden_layers[[i]],isLinear = TRUE, "logistic",3,hidden_layers[[i]])
  # test model on t3_testDataSet
  pred <- ModelTest(model, t3_testDataSet,3,hidden_layers[[i]])
}


# t4 with different hidden layer sizes
hidden_layers <- list( c(8),c(15),c(5,2),c(10,5))

# loop through different hidden layer sizes
for (i in seq_along(hidden_layers)) {
  # train model using t1, t2, t3, and t4 as input and current hidden layer size
  model <- ModelTrain(exchange_rateRate ~ t1 + t2 + t3 + t4,hidden_layers[[i]],isLinear = TRUE, "logistic",4,hidden_layers[[i]])
  # test model on t4_testDataSet
  pred <- ModelTest(model, t4_testDataSet,4,hidden_layers[[i]])
}

# Identify the best MLP network configuration based on the provided information
best_config <- list(
  input_count = 3,
  hidden_layers = c(10),
  neurons = 10
)

# Train the best MLP network
best_model <- ModelTrain(exchange_rateRate ~ t1 + t2 + t3, best_config$hidden_layers, isLinear = TRUE, "logistic", best_config$input_count, best_config$hidden_layers)

# Test the best MLP network on the testing dataset
predictions <- ModelTest(best_model, t3_testDataSet, best_config$input_count, best_config$hidden_layers)

# Graphical representation - Scatter plot or line chart
plot(first_data_output, predictions, col = 'blue', main = "Predicted vs. Actual Output", xlab = "Actual Output", ylab = "Predicted Output")
abline(0, 1, col = "red", lwd = 2)  # Reference line for perfect predictions
legend("bottomright", legend = 'Predictions', col = 'blue', pch = 1)  # Add legend
grid()

# Statistical indices
rmse <- rmse(first_data_output, predictions)
mae <- mae(first_data_output, predictions)
mape <- mape(first_data_output, predictions)
smape <- smape(first_data_output, predictions)

# Print statistical indices
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
cat("MAPE:", mape, "\n")
cat("sMAPE:", smape, "\n")
