### Supervised Machine Learning 1


# libraries
rm(list = ls())

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(caret)
library(recipes)
library(yardstick)

# data
daily_fluxes <- read_csv("./data/FLX_CH-Dav_FLUXNET2015_FULLSET_DD_1997-2014_1-3.csv") |>

  # select only the variables we are interested in
  dplyr::select(TIMESTAMP,
                GPP_NT_VUT_REF,    # the target
                ends_with("_QC"),  # quality control info
                ends_with("_F"),   # includes all all meteorological covariates
                -contains("JSB")   # weird useless variable
  ) |>

  # convert to a nice date object
  dplyr::mutate(TIMESTAMP = lubridate::ymd(TIMESTAMP)) |>

  # set all -9999 to NA
  mutate(across(where(is.numeric), ~na_if(., -9999))) |>

  # retain only data based on >=80% good-quality measurements
  # overwrite bad data with NA (not dropping rows)
  dplyr::mutate(GPP_NT_VUT_REF = ifelse(NEE_VUT_REF_QC < 0.8, NA, GPP_NT_VUT_REF),
                TA_F           = ifelse(TA_F_QC        < 0.8, NA, TA_F),
                SW_IN_F        = ifelse(SW_IN_F_QC     < 0.8, NA, SW_IN_F),
                LW_IN_F        = ifelse(LW_IN_F_QC     < 0.8, NA, LW_IN_F),
                VPD_F          = ifelse(VPD_F_QC       < 0.8, NA, VPD_F),
                PA_F           = ifelse(PA_F_QC        < 0.8, NA, PA_F),
                P_F            = ifelse(P_F_QC         < 0.8, NA, P_F),
                WS_F           = ifelse(WS_F_QC        < 0.8, NA, WS_F)) |>

  # drop QC variables (no longer needed)
  dplyr::select(-ends_with("_QC"))




set.seed(123)  # for reproducibility

# split data into training and testing
split <- rsample::initial_split(daily_fluxes, prop = 0.7, strata = "VPD_F")
daily_fluxes_train <- rsample::training(split)
daily_fluxes_test <- rsample::testing(split)


plot_data <- daily_fluxes_train |>
  dplyr::mutate(split = "train") |>
  dplyr::bind_rows(daily_fluxes_test |>
                     dplyr::mutate(split = "test")) |>
  tidyr::pivot_longer(cols = 2:9, names_to = "variable", values_to = "value")

plot_data |>
  ggplot(aes(x = value, y = ..density.., color = split)) +
  geom_density() +
  facet_wrap(~variable, scales = "free")


# recipe = anleitung for scale and centering the data, its a methods
# scale and center (only) the training data! to achieve equal wieghing of variables
pp <- recipes::recipe(GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + TA_F, data = daily_fluxes_train) |>
  recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) |>
  recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes())


# train the model on the training data
caret::train(
  pp,                            # scale/center method
  data = daily_fluxes_train,     # og data set
  method = "knn",                # type of model
  trControl = caret::trainControl(method = "none") # no iterations
)


# prepare the recipe (data specific (1))
pp_prep <- recipes::prep(pp, training = daily_fluxes_train)

# transform the data
daily_fluxes_juiced <- recipes::juice(pp_prep)

# if we have a data specific recipe like at (1)
# we need to bake it
daily_fluxes_baked <- recipes::bake(pp_prep, new_data = daily_fluxes_train)

# confirm that juice and bake return identical objects when given the same data
all_equal(daily_fluxes_juiced, daily_fluxes_baked)


# visualize missing data
visdat::vis_miss(
  daily_fluxes,
  cluster = FALSE,
  warn_large_data = FALSE
)


# gap filling with the median
pp |> step_impute_median(all_predictors())


# with KNN with the five neighbours
pp |> step_impute_knn(all_predictors(), neighbors = 5)


# dealing with categorical variables that are then encoded to 0 and 1
recipe(GPP_NT_VUT_REF ~ ., data = daily_fluxes) |>
  step_dummy(all_nominal(), one_hot = TRUE)

# (near) zero variance variables need to be removed
# you can check for them like this:
caret::nearZeroVar(daily_fluxes, saveMetrics = TRUE)


# we add this to our recipe
pp |>
  step_zv(all_predictors())




# THE WHOLE PROCEDURE

# Data cleaning: looks ok, no obviously bad data
# no long tail, therefore no further target engineering
daily_fluxes |>
  ggplot(aes(x = GPP_NT_VUT_REF, y = ..count..)) +
  geom_histogram()

# Data splitting
set.seed(1982)  # for reproducibility
split <- rsample::initial_split(daily_fluxes, prop = 0.7, strata = "VPD_F")
daily_fluxes_train <- rsample::training(split)
daily_fluxes_test <- rsample::testing(split)

# Model and pre-processing formulation, use all variables but LW_IN_F
pp <- recipes::recipe(GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + TA_F,
                      data = daily_fluxes_train |> drop_na()) |>
  recipes::step_BoxCox(recipes::all_predictors()) |>
  recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) |>
  recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes())

# Fit linear regression model
mod_lm <- caret::train(
  pp,
  data = daily_fluxes_train |> drop_na(),
  method = "lm",
  trControl = caret::trainControl(method = "none"),
  metric = "RMSE"
)

# Fit KNN model
mod_knn <- caret::train(
  pp,
  data = daily_fluxes_train |> drop_na(),
  method = "knn",
  trControl = caret::trainControl(method = "none"),
  tuneGrid = data.frame(k = 8),
  metric = "RMSE"
)



# make model evaluation into a function to reuse code
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

# linear regression model
eval_model(mod = mod_lm, df_train = daily_fluxes_train, df_test = daily_fluxes_test)


# KNN
eval_model(mod = mod_knn, df_train = daily_fluxes_train, df_test = daily_fluxes_test)





