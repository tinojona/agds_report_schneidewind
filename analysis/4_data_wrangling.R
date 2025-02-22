### Data Wrangling

### Exercises


# libraries
library(dplyr); library(lubridate); library(tidyr); library(readr); library(stringr); library(purrr)


## Star Wars
sw <- dplyr::starwars
View(sw)

# how many pale characters come from the planets Ryloth and Naboo?
print(paste("There are ",
            nrow(dplyr::filter(.data = sw , skin_color == "pale" & (homeworld == "Ryloth" | homeworld == "Naboo"))),
            " pale characters that come from either Ryloth or Naboo"))

# who is the oldest among the tallest thirty characters?
print(paste0("The oldest among the thirty tallest characters is: ",
             dplyr::arrange(.data = sw, height) |>
               dplyr::slice_tail(n = 30) |>
               dplyr::slice_max(birth_year) |>
               dplyr::select(name)
             ))

# what is the name of the smallest character and their starship in “Return of the Jedi”
small <- sw |>
  dplyr::arrange(height) |>
  dplyr::slice_head(n=1) |>
  dplyr::select(name, films, starships)

print(paste0("The name of the smallest character is ", small$name, " and their starship was ", small$starships))


## Aggregating
# Reuse the code in the tutorial to read, reduce, and aggregate the half_hourly_fluxes
# dataset to the daily scale calculating the following metrics across half-hourly VPD_F values
# within each day: mean, 25% quantile, and 75% quantile.

half_hourly_fluxes <- readr::read_csv("../bigdata/FLX_CH-Lae_FLUXNET2015_FULLSET_HH_2004-2006.csv")

half_hourly_fluxes <- dplyr::select(
  half_hourly_fluxes,
  starts_with("TIMESTAMP"),
  ends_with("_F"),
  GPP_NT_VUT_REF,
  NEE_VUT_REF_QC,
  starts_with("SWC_F_MDS_"),
  -contains("JSB"),
  NIGHT
)

half_hourly_fluxes <- half_hourly_fluxes |>
  dplyr::mutate(across(starts_with("TIMESTAMP_"), lubridate::ymd_hm))



daily_fluxes_VPD_F <- half_hourly_fluxes |>
  mutate(date = as_date(TIMESTAMP_START)) |>
  group_by(date) |>
  summarise(mit = mean(VPD_F),
            q25 = quantile(VPD_F, .25),
            q75 = quantile(VPD_F, .75))


# Retain only the daily data for which the daily mean VPD is among the upper or the lower 10% quantiles.

daily_fluxes_VPD_F_extreme <- daily_fluxes_VPD_F |>
  dplyr::filter(mit < quantile(mit, .1) | mit > quantile(mit, .9))

# Calculate the mean of the 25% and the mean of the 75% quantiles of half-hourly VPD within the
# upper and lower 10% quantiles of mean daily VPD.

mean(daily_fluxes_VPD_F_extreme$q25);mean(daily_fluxes_VPD_F_extreme$q75)


## Patterns in data quality
# Investigate whether NEE data quality is randomly spread across hours in a day by calculating the proportion of
# (i) actually measured data, (ii) good quality gap-filled data,
# (iii) medium quality data, and (iv) poor quality data within each hour-of-day

NEE_hourly_quality <- half_hourly_fluxes |>
  mutate(hour = lubridate::hour(TIMESTAMP_START)) |>
  group_by(hour) |>
  summarise(prop_total = 1 - (sum(is.na(NEE_VUT_REF_QC)) /n()),
            prop_0 = sum(NEE_VUT_REF_QC == 0) /n(),
            prop_1 = sum(NEE_VUT_REF_QC == 1) /n(),
            prop_2 = sum(NEE_VUT_REF_QC == 2) /n(),
            prop_3 = sum(NEE_VUT_REF_QC == 3) /n())

plot(NEE_hourly_quality$hour, NEE_hourly_quality$prop_total, lty = 2, ylim = c(0,1), col = 5)
lines(NEE_hourly_quality$hour, NEE_hourly_quality$prop_0, lty = 1, col = 1)
lines(NEE_hourly_quality$hour, NEE_hourly_quality$prop_1, lty = 1, col = 2)
lines(NEE_hourly_quality$hour, NEE_hourly_quality$prop_2, lty = 1, col = 3)
lines(NEE_hourly_quality$hour, NEE_hourly_quality$prop_3, lty = 1, col = 4)
legend("top", ncol = 1, legend = c("all", "0", "1", "2", "3"), col = c(5,1,2,3,4), bty = "n", lty = 1)

# interpret your findings: they are differently/inhomogeniously spread on average over the day

# aggregate the half-hourly GPP data (variable GPP_NT_VUT_REF) to daily means of the unmodified data (0-3)
# and from cleaned data where only measured (not gap-filled) data is kept. (only 3)
# Calculate the overall mean GPP for the two data frames
# Are the overall mean GPP values equal? If not, why?

half_hourly_fluxes <- readr::read_csv("../bigdata/FLX_CH-Lae_FLUXNET2015_FULLSET_HH_2004-2006.csv")

daily_GPP <- half_hourly_fluxes |>
  mutate(date = as_date(TIMESTAMP_START)) |>
  filter(NEE_VUT_REF_QC == 1) |>
  group_by(date) |>
  summarise(mit_all = mean(GPP_NT_VUT_REF , na.rm=T))



# data cleaning routine

half_hourly_fluxes <- select(
  half_hourly_fluxes,
  starts_with("TIMESTAMP"),
  ends_with("_F"),
  GPP_NT_VUT_REF,
  NEE_VUT_REF_QC,
  starts_with("SWC_F_MDS_"),
  -contains("JSB"),
  NIGHT
)

half_hourly_fluxes$TIMESTAMP_START <- ymd_hm(half_hourly_fluxes$TIMESTAMP_START)

half_hourly_fluxes |>
  mutate(year = year(TIMESTAMP_START),
         month = month(TIMESTAMP_START),
         doy = yday(TIMESTAMP_START)     # day of year
  ) |>
  select(TIMESTAMP_START, TIMESTAMP_END, year, month, doy)  # for displaying

half_hourly_fluxes <- half_hourly_fluxes |>
  mutate(across(where(is.numeric), ~na_if(., -9999)))

half_hourly_fluxes |>
  filter(NEE_VUT_REF_QC %in% c(0,1))

half_hourly_fluxes |>
  mutate(GPP_NT_VUT_REF = ifelse(NEE_VUT_REF_QC %in% c(0,1), GPP_NT_VUT_REF, NA))


half_hourly_fluxes |>
  drop_na()


write_csv(half_hourly_fluxes, file = "../bigdata/FLX_CH-Lae_FLUXNET2015_FULLSET_HH_2004-2006_CLEAN.csv")


