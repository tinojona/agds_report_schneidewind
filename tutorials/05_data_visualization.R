### Data Visualization

### Exercises


# libraries
library(ggplot2);library(readr);library(lubridate);library(dplyr);library(slider)

# data
rm(list =  ls())
half_hourly_fluxes <- readr::read_csv("../bigdata/FLX_CH-Lae_FLUXNET2015_FULLSET_HH_2004-2006.csv")


## Spurious Data
# Certain values of GPP_NT_VUT_REF in half_hourly_fluxes are repeated with a spuriously high frequency.
# Determine all values of GPP_NT_VUT_REF that appear more than once in half_hourly_fluxes
# and label them as being “spurious”. Visualise the time series of the first year of half-hourly GPP,
# mapping the information whether the data is spurious or not to the color aesthetic.

half_hourly_fluxes <- half_hourly_fluxes |>
  mutate(TIMESTAMP_START = ymd_hm(TIMESTAMP_START)) |>
  group_by(GPP_NT_VUT_REF) |>
  mutate(index = case_when(n() > 1 ~ "spurious", TRUE ~ "non-spurious")) |>
  ungroup()

ggplot(half_hourly_fluxes[1:365,], aes(x = TIMESTAMP_START, y = GPP_NT_VUT_REF)) +
  geom_line() +
  geom_point(aes(color = index)) +
  theme_minimal() +
  labs(x = "Time",
       y = "GPP")



# Then aggregate half-hourly to daily data, taking the mean of GPP_NT_VUT_REF and recording the proportion
# of underlying half-hourly data points that are “spurious”. Visualise the time series of daily GPP_NT_VUT_REF
# with the color scale indicating the proportion of spurious half-hourly data that was used for determining the
# respective date’s mean GPP.

daily_GPP <- half_hourly_fluxes |>
  mutate(date = as_date(TIMESTAMP_START)) |>
  group_by(date) |>
  summarize(
    GPP = mean(GPP_NT_VUT_REF, na.rm = TRUE),
    spur_percentage = (sum(index == "spurious") / n()) *100
    )

ggplot(daily_GPP, aes(x = date, y = GPP)) +
  geom_line() +
  geom_point(aes(colour = spur_percentage), size = 1.2) +
  theme_minimal() +
  labs(
    x = "time",
    y = "GPP") +
  scale_color_viridis_c()


## Identifying Outliers
# First, using the half-hourly fluxes data, determine “outliers” as those values of GPP_NT_VUT_REF that fall outside to.
# Plot GPP_NT_VUT_REF versus shortwave radiation and highlight outliers in red.

half_hourly_fluxes <- half_hourly_fluxes |>
  mutate(
    IQR_GPP = IQR(GPP_DT_VUT_REF, na.rm = TRUE),
    Q1 = quantile(GPP_DT_VUT_REF, 0.25, na.rm = TRUE),
    Q3 = quantile(GPP_DT_VUT_REF, 0.75, na.rm = TRUE),
    outliers = if_else(GPP_NT_VUT_REF < (Q1 - 1.5 * IQR_GPP) |
                         GPP_NT_VUT_REF > (Q3 + 1.5 * IQR_GPP), "Outlier", "Normal"),
  )

ggplot(half_hourly_fluxes, aes(x = GPP_DT_VUT_REF, y = SW_IN_POT, color = outliers)) +
  geom_point() +
  scale_color_manual(values = c("Normal" = "black", "Outlier" = "red")) +
  labs(color = "Outlier Status") +
  theme_minimal()

# Now, we want to “control” for the influence of shortwave radiation on GPP and define outliers
# with respect to the distribution of residuals of the linear regression between the two variables.
# Relax the definition of what is considered an outlier by setting adjusting their definition to falling outside
# to. Again, plot GPP_NT_VUT_REF versus shortwave radiation and highlight outliers in red.

half_hourly_fluxes <- half_hourly_fluxes |>
  mutate(
    IQR_GPP_relaxed = IQR(lm(GPP_NT_VUT_REF ~ SW_IN_F, data = half_hourly_fluxes)$residuals, na.rm = TRUE),
    Q1_relaxed = quantile(lm(GPP_NT_VUT_REF ~ SW_IN_F, data = half_hourly_fluxes)$residuals, 0.25, na.rm = TRUE),
    Q3_relaxed = quantile(lm(GPP_NT_VUT_REF ~ SW_IN_F, data = half_hourly_fluxes)$residuals, 0.75, na.rm = TRUE),
    outliers_relaxed = if_else(GPP_NT_VUT_REF < (Q1_relaxed - 5 * IQR_GPP_relaxed) |
                         GPP_NT_VUT_REF > (Q3_relaxed + 5 * IQR_GPP_relaxed), "Outlier", "Normal"),
  )


ggplot(half_hourly_fluxes, aes(x = GPP_DT_VUT_REF, y = SW_IN_POT, color = outliers_relaxed)) +
  geom_point() +
  scale_color_manual(values = c("Normal" = "black", "Outlier" = "red")) +
  labs(color = "Outlier Status") +
  theme_minimal()



## Visualising diurnal and seasonal cycles of GPP
# As explored in the previous Chapter’s exercises, GPP varies over diurnal and seasonal cycles.
# Create a publication-ready figure that visualises the mean diurnal cycle of GPP for each day-of-year
# (mean across multiple years). Make sure that the figure is properly labelled,
# and legible for readers with a color vision deficiency.


GPP_matrix <- half_hourly_fluxes |>
  mutate(dayofyear = yday(TIMESTAMP_START),
         hour = hour(TIMESTAMP_START)) |>
  group_by(dayofyear, hour) |>
  summarize(GPP = mean(GPP_DT_VUT_REF, na.rm = TRUE), .groups = "drop")



GPP_matrix_long <- GPP_matrix |>
  pivot_longer()


ggplot(GPP_matrix, aes(x = hour, y = dayofyear, fill = GPP)) +
  geom_tile() +
  scale_fill_viridis_c(option = "viridis", na.value = "white") +
  labs(
    x = "Hour of the Day",
    y = "Day of the Year",
    fill = "GPP",
    title = "GPP Across Days and Hours"
  ) +
  theme_minimal()


## Trend in Carbon Dioxide Concentrations
CO2_data <- read_csv("./data/co2_mm_mlo.csv")

moving_average <- function(data, column_name, before, after){
  data <- data |>
    mutate(MA = slide_dbl(!!sym(column_name), mean,
                          .before = before,
                          .after = after,
                          .complete = TRUE))
  return(data)
}

CO2_data <- moving_average(CO2_data, "average", 12, 12)



ggplot(CO2_data, aes(x = `decimal date`, y = average)) +
  geom_line(color = "red") +
  geom_line(aes(y = MA), color = "blue") +
  labs(x = "Time",
       y = "CO2 concentration") +
  theme_minimal()
