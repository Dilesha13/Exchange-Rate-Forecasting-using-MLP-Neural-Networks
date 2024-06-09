# Exchange-Rate-Forecasting-using-MLP-Neural-Networks

1. Introduction

      *Objective: The main goal was to develop a predictive model for the USD/EUR exchange rate using historical data.
   
      *Methods Used: Employed a neural network approach, specifically a Multi-Layer Perceptron (MLP), to forecast future exchange rates.
   
3. Data Preparation

      *Dataset: Mention the source and type of the data (exchange rates over time).

      *Initial Steps: Imported the dataset and selected numeric columns for analysis.

      *Outlier Removal: Used boxplots to identify and remove outliers, reducing noise and improving model performance.

        outliers_remove <- function(x) {
          bp <- boxplot.stats(x)$stats
          x[x < bp[1] | x > bp[5]] <- NA
          return(x)
        }
   
3. Feature Engineering

      *Time-Delayed Matrix: Created input features with lagged values to capture temporal dependencies.

        time_delayed_matrix <- bind_cols(
          t4 = lag(exchange_rate,5),
          t3 = lag(exchange_rate,4),
          t2 = lag(exchange_rate,3),
          t1 = lag(exchange_rate,2),
          exchange_rateRate = exchange_rate) 
        delayed_matrix <- na.omit(time_delayed_matrix)
   
5. Data Normalization

      *Purpose: Normalization helps in improving the convergence speed of neural networks and ensures that each input feature contributes equally to the learning process.

        normalization <- function(x){
          return ((x - min(x)) / (max(x) - min(x)))
        }
   
5. Model Training

      *Neural Network Architecture: Mention experimenting with different numbers of hidden layers and neurons to find the best configuration.

        ModelTrain <- function(formula, hiddenVal, isLinear, actFunc, inputs, hidden) {
          nural <- neuralnet(formula, data = train_datasetNormaliz, hidden = hiddenVal, act.fct = actFunc, linear.output = isLinear)
          # Plotting and other steps
          return(nural)
        }
   
      *Model Testing: Explain how you evaluated the model performance on the test data.

        ModelTest <- function(nuralModel, testing_df, inputs, hidden) {
          nnResults <- compute(nuralModel, testing_df)
          predict <- nnResults$net.result
          unuralormalised_predict <- un_normalization(predict, first_min_value, first_max_value)
          # Accuracy and error calculations
          return(unuralormalised_predict)
        }
   
7. Model Evaluation

      *Metrics: Root Mean Squared Error (RMSE), Mean Absolute Error (MAE), Mean Absolute Percentage Error (MAPE), and Symmetric Mean Absolute Percentage Error (sMAPE).

        rmse <- rmse(first_data_output, predictions)
        mae <- mae(first_data_output, predictions)
        mape <- mape(first_data_output, predictions)
        smape <- smape(first_data_output, predictions)
   
7. Results and Interpretation

      *Best Model: The model with the configuration of 3 inputs and 10 neurons in the hidden layer performed the best.

        best_config <- list(
          input_count = 3,
          hidden_layers = c(10),
          neurons = 10
        )
        best_model <- ModelTrain(exchange_rateRate ~ t1 + t2 + t3, best_config$hidden_layers, isLinear = TRUE, "logistic", best_config$input_count, best_config$hidden_layers)
        predictions <- ModelTest(best_model, t3_testDataSet, best_config$input_count, best_config$hidden_layers)
   
9. Visualization

      *Prediction Graphs: Show plots comparing predicted vs. actual values to visually assess the model performance.
