


# Eval model function

eval_model <- function(mod, df_train, df_test){

  # add predictions to the data frames
  df_train <- df_train |>
    drop_na()
  df_train$fitted <- predict(mod, newdata = df_train)

  df_test <- df_test |>
    drop_na()
  df_test$fitted <- predict(mod, newdata = df_test)

  # get metrics tables
  metrics_train <- df_train |>
    yardstick::metrics(GPP_NT_VUT_REF, fitted)

  metrics_test <- df_test |>
    yardstick::metrics(GPP_NT_VUT_REF, fitted)

  # extract values from metrics tables
  rmse_train <- metrics_train |>
    filter(.metric == "rmse") |>
    pull(.estimate)
  rsq_train <- metrics_train |>
    filter(.metric == "rsq") |>
    pull(.estimate)

  rmse_test <- metrics_test |>
    filter(.metric == "rmse") |>
    pull(.estimate)
  rsq_test <- metrics_test |>
    filter(.metric == "rsq") |>
    pull(.estimate)

  # visualise as a scatterplot
  # adding information of metrics as sub-titles
  plot_1 <- ggplot(data = df_train, aes(GPP_NT_VUT_REF, fitted)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    labs(subtitle = bquote( italic(R)^2 == .(format(rsq_train, digits = 2)) ~~
                              RMSE == .(format(rmse_train, digits = 3))),
         title = "Training set") +
    theme_classic()

  plot_2 <- ggplot(data = df_test, aes(GPP_NT_VUT_REF, fitted)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    labs(subtitle = bquote( italic(R)^2 == .(format(rsq_test, digits = 2)) ~~
                              RMSE == .(format(rmse_test, digits = 3))),
         title = "Test set") +
    theme_classic()

  out <- cowplot::plot_grid(plot_1, plot_2)

  return(out)
}



# prediction model that outputs merged and ordered predictions
# of combined train and test sets
pred_model <- function(mod, df, df_train, df_test) {


  # column name for predictions with model type
  fitted_col <- paste0("fitted_", mod$method)

  # remove missing data
  df_train_clean <- df_train |> drop_na()
  df_test_clean  <- df_test |> drop_na()

  # predict for train and tes
  df_train_clean[[fitted_col]] <- predict(mod, newdata = df_train_clean)
  df_test_clean[[fitted_col]]  <- predict(mod, newdata = df_test_clean)

  # rbind prediction data
  df_combined <- bind_rows(df_train_clean, df_test_clean)

  # merge with original timeseries of daily_data because of missing dates in train/test
  df_merged <- left_join(df, df_combined, by = "TIMESTAMP") |> arrange(TIMESTAMP)

  return(df_merged[[fitted_col]])
}



