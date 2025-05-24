### Regression and Classification


# libraries
library(ggplot2)
library(dplyr)
rm(list = ls())


# read and format data from Ch 3
half_hourly_fluxes <- readr::read_csv("./data/FLX_CH-Lae_FLUXNET2015_FULLSET_HH_2004-2006_CLEAN.csv")

set.seed(2023)
plot_1 <- half_hourly_fluxes |>
  sample_n(2000) |>  # to reduce the dataset
  ggplot(aes(x = SW_IN_F, y = GPP_NT_VUT_REF)) +
  geom_point(size = 0.75, alpha = 0.4) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(x = expression(paste("Shortwave radiation (W m"^-2, ")")),
       y = expression(paste("GPP (gC m"^-2, "s"^-1, ")"))) +
  theme_classic()

segment_points <- data.frame(x0 = 332, y0 = 3.65, y_regr = 8.77)

plot_1 +
  geom_segment(aes(x = x0, y = y0, xend = x0, yend = y_regr),
               data = segment_points,
               color = "blue", lwd = 1.2, alpha = 0.8)


# numerical variables only, remove NA
df <- half_hourly_fluxes |>
  dplyr::select(-starts_with("TIMESTAMP")) |>
  tidyr::drop_na()

# fit univariate linear regression
linmod1 <- lm(GPP_NT_VUT_REF ~ SW_IN_F, data = df)
# fit multivariate linear regression
linmod2 <- lm(GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + TA_F, data = df)
# all possible predictors in the data set
linmod3 <- lm(GPP_NT_VUT_REF ~ ., data = df)



# create month category
df_cat <- half_hourly_fluxes |>
  mutate(MONTH = lubridate::month(TIMESTAMP_START)) |>
  tidyr::drop_na() |>
  dplyr::select(MONTH, GPP_NT_VUT_REF, SW_IN_F)


# fix class of categorical variables
df_cat <- df_cat |>
  mutate(MONTH = as.factor(MONTH))


linmod_cat <- lm(GPP_NT_VUT_REF ~ MONTH + SW_IN_F, data = df_cat)
summary(linmod_cat)

df_cat |>
  mutate(MONTH_NAME = lubridate::month(as.integer(MONTH), label = TRUE)) |>
  ggplot(aes(x = SW_IN_F, y = GPP_NT_VUT_REF)) +
  geom_point(alpha = 0.2) +
  geom_smooth(formula = y ~ x + 0, method = "lm", color = "red", se = FALSE) +
  labs(x = "SW", y = "GPP") +
  facet_wrap(~MONTH_NAME) +
  theme_classic()

# interaction between month and SW
linmod_inter <- lm(GPP_NT_VUT_REF ~ MONTH + SW_IN_F + MONTH:SW_IN_F, data = df_cat)
# equivalently: lm(GPP_NT_VUT_REF ~ MONTH * SW_IN_F, data = df_cat)
summary(linmod_inter)


# polynomial models for nonlinear relationships
quadmod <- lm(GPP_NT_VUT_REF ~ poly(SW_IN_F, 2),
              data = df_cat |>
                filter(MONTH == 8))
summary(quadmod)

df_cat |>
  filter(MONTH == 8) |>
  ggplot(aes(x = SW_IN_F, y = GPP_NT_VUT_REF)) +
  geom_point(alpha = 0.4) +
  geom_smooth(formula = y ~ x, method = "lm", aes(color = "lm"), se = FALSE) +
  geom_smooth(formula = y ~ poly(x, 2), method = "lm",
              aes(color = "poly2"), se = FALSE) +
  geom_smooth(formula = y ~ poly(x, 3), method = "lm",
              aes(color = "poly3"), se = FALSE) +
  labs(x = "SW", y = "GPP", color = "Regression") +
  theme_classic()



# generate correlated random data
set.seed(1982)
df <- tibble(x = rnorm(100)) |>
  mutate(y = x + rnorm(100)) |>
  mutate(y_fitted = lm(y ~ x)$fitted.values)

# implementations using Pearson's correlation
summary(lm(y ~ x, data = df))$r.squared
cor(df$y, df$x)^2 # remember: location and scale invariant

#slope
coef(lm(y ~ y_fitted, data = df))[2]



# regression fit metrics
compute_regr_metrics <- function(mod){

  p <- length(mod$coefficients)
  n <- length(mod$residuals)

  tibble(
    mse = mean(mod$residuals^2),
    R2 = summary(mod)$r.squared,
    R2_adj = summary(mod)$adj.r.squared,
    AIC = extractAIC(mod)[2],
    AIC_adj = extractAIC(mod)[2] + 2*(p+2)*(p+3)/(n-p-3),
    BIC = BIC(mod) # this implementation is based on log-likelihood
  )
}

list_metrics <- purrr::map(
  list(linmod1, linmod2, linmod_cat, quadmod),
  ~compute_regr_metrics(.))
names(list_metrics) <- c("Linear model",
                         "Linear model 2",
                         "Linear + categories",
                         "Quadratic model")
bind_rows(list_metrics, .id = "type")

set.seed(2023)
half_hourly_fluxes_small <- half_hourly_fluxes |>
  sample_n(100) |> # reduce dataset
  select(SW_IN_F, GPP_NT_VUT_REF)

plot_3 <- half_hourly_fluxes_small |>
  ggplot(aes(x = SW_IN_F, y = GPP_NT_VUT_REF)) +
  geom_point(size = 0.75) +
  geom_smooth(method = "lm", color = "red", fullrange = TRUE) +
  labs(x = expression(paste("Shortwave radiation (W m"^-2, ")")),
       y = expression(paste("GPP (", mu,"mol CO"[2], " m"^-2, "s"^-1, ")"))) +
  theme_classic() +
  ylim(-20, 40) +
  xlim(0, 1100)

plot_4 <- half_hourly_fluxes_small |>
  add_row(SW_IN_F = 1100, GPP_NT_VUT_REF = -20) |> # add outlier
  ggplot(aes(x = SW_IN_F, y = GPP_NT_VUT_REF)) +
  geom_point(size = 0.75) +
  geom_smooth(method = "lm", color = "red", fullrange = TRUE) +
  labs(x = expression(paste("Shortwave radiation (W m"^-2, ")")),
       y = expression(paste("GPP (", mu,"mol CO"[2], " m"^-2, "s"^-1, ")"))) +
  theme_classic() +
  geom_point(aes(x = 1100, y = -20), colour = 'red', shape = 1, size = 3) +
  ylim(-20, 40) +
  xlim(0, 1100)

cowplot::plot_grid(plot_3, plot_4)


# create an outlier for demonstration purposes
half_hourly_fluxes_outlier <- half_hourly_fluxes_small |>
  add_row(SW_IN_F = 1100, GPP_NT_VUT_REF = -20)

# Various ways to identify the outlier using graphs
plot_5 <- ggplot(
  data = half_hourly_fluxes_outlier,
  aes(x = GPP_NT_VUT_REF, y = after_stat(density))) +
  geom_histogram(fill = "grey70", color = "black") +
  geom_density(color = 'red')+
  labs(title = 'Histogram, density and boxplot',
       x = expression(paste("GPP (gC m"^-2, "s"^-1, ")"))) +
  theme_classic()

plot_6 <- ggplot(
  data = half_hourly_fluxes_outlier,
  aes(x = "", y = GPP_NT_VUT_REF)) +
  geom_boxplot(fill = "grey70", color = "black") +
  coord_flip() +
  theme_classic() +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(y = expression(paste("GPP (gC m"^-2, "s"^-1, ")")))

plot_7 <- ggplot(
  data = half_hourly_fluxes_outlier,
  aes(x = SW_IN_F, y = after_stat(density))) +
  geom_histogram(fill = "grey70", color = "black") +
  geom_density(color = 'red')+
  labs(title = 'Histogram, density and boxplot',
       x = expression(paste("Shortwave radiation (W m"^-2, ")"))) +
  theme_classic()

plot_8 <- ggplot(
  data = half_hourly_fluxes_outlier,
  aes(x = "", y = SW_IN_F)) +
  geom_boxplot(fill = "grey70", color = "black") +
  coord_flip() +
  theme_classic() +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(y = expression(paste("Shortwave radiation (W m"^-2, ")")))

cowplot::plot_grid(plot_5, plot_7, plot_6, plot_8,
                   ncol = 2, rel_heights = c(2,1),
                   align = 'v', axis = 'lr')



# QQ plots to investigate normal distribution of predictors (what we want)
plot_9 <- ggplot(
  data = half_hourly_fluxes_outlier,
  aes(sample = GPP_NT_VUT_REF)) +
  geom_qq() +
  geom_qq_line() +
  labs(y = expression(paste("GPP (gC m"^-2, "s"^-1, ")")),
       x = "Theoretical normal quantiles") +
  theme_classic()

plot_10 <- ggplot(
  data = half_hourly_fluxes_outlier,
  aes(sample = SW_IN_F)) +
  geom_qq() +
  geom_qq_line() +
  labs(y = expression(paste("Shortwave radiation (W m"^-2, ")")),
       x = "Theoretical normal quantiles") +
  theme_classic()

cowplot::plot_grid(plot_9, plot_10, ncol = 2)

# we can investigate the leverage of outliers using leverage plots
# Fit regression with outlier
linmod_outlier <- lm(GPP_NT_VUT_REF ~ SW_IN_F,
                     data = add_row(half_hourly_fluxes_small,
                                    SW_IN_F = 1100,
                                    GPP_NT_VUT_REF = -20))

plot(linmod_outlier, 5)



# Classification
# often based on multiple continous predictor
datasets::CO2 |>
  ggplot(aes(x = uptake, y = Type, color = Type)) +
  geom_point(size = 3, alpha = 0.5) +
  theme_classic() +
  labs(x = expression(paste("Uptake (", mu,"mol CO"[2], " m"^-2, "s"^-1, ")"))) +
  theme(legend.position = "none")


# logistical linear regression
logmod <- glm(Type ~ uptake,
              family = binomial(link = logit),
              data = datasets::CO2)
summary(logmod)

beta <- coef(logmod)

# reuse previous plot with classification line
datasets::CO2 |>
  ggplot(aes(x = uptake, y = as.numeric(Type)-1, color = Type)) +
  geom_point(size = 3, alpha = 0.5) +
  labs(x = expression(paste("Uptake (", mu,"mol CO"[2], " m"^-2, "s"^-1, ")")),
       y = "") +
  theme_classic() +

  # call glm model fit as part of the plotting
  stat_smooth(method = "glm", color = "grey", se = FALSE,
              method.args = list(family = binomial),
              size = 3) +

  # manually plot of logit function with fitted coefficients
  geom_function(fun = function(x) exp(beta[1] + beta[2] * x)/(1 + exp(beta[1] + beta[2] * x)),
                color = "black", size = 0.5) +

  # visualise threshold
  geom_vline(xintercept = -beta[1] / beta[2], lty = 2, linetype = "dotted") +
  xlim(0, 60)


# confusion matrix to detect false positives and negatives
# Make classification predictions
Y <- logmod$data$Type
x <- as.factor(round(logmod$fitted.values)) # Use 0.5 as threshold

# Change class names
levels(Y) <- levels(x) <- c("Quebec", "Mississippi")

# plot confusion matrix
conf_matrix <- caret::confusionMatrix(data = x, reference = Y)
conf_matrix
mosaicplot(conf_matrix$table,
           main = "Confusion matrix")
