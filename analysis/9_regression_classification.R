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


# stopped at 9.3.2.5 Metrics for regression model comparison
