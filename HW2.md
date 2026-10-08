p8105_hw2_hca2124
================
Helene Apollon
2026-10-08

\#Question 1

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.2.1     ✔ readr     2.2.0
    ## ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ## ✔ ggplot2   4.0.3     ✔ tibble    3.3.1
    ## ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
    ## ✔ purrr     1.2.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
NYC_transit_df =
  read_csv("./NYC_Transit_Subway_Entrance_And_Exit_Data.csv",
           na = c ("NA", ".", ""))|>
janitor::clean_names() |>
  select(line, station_name, station_latitude, station_longitude, route1:route11, entry, vending, entrance_type, ada) |>
mutate(
  entry = 
         case_match(
           entry,
           "YES" ~ TRUE,
           "NO" ~ FALSE
         ))
```

    ## Rows: 1868 Columns: 32
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (22): Division, Line, Station Name, Route1, Route2, Route3, Route4, Rout...
    ## dbl  (8): Station Latitude, Station Longitude, Route8, Route9, Route10, Rout...
    ## lgl  (2): ADA, Free Crossover
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `entry = case_match(entry, "YES" ~ TRUE, "NO" ~ FALSE)`.
    ## Caused by warning:
    ## ! `case_match()` was deprecated in dplyr 1.2.0.
    ## ℹ Please use `recode_values()` instead.

This data set contains information on the entrances and exits for each
station in NYC. It contains 19 variables. From the original data, I
cleaned the names, and kept only the variables of interest using the
select line followed by the variable names. Then I converted the entry
variable from a character (Yes vs No), to a logical variable. The final
dataset has 1868 rows and 19 columns. The data is not tidy because the
Route variable is spread across 11 variable columns from route1:route11.

``` r
NYC_transit_df |> 
  distinct(line, station_name) |> 
  nrow()
```

    ## [1] 465

There are 465 distinct stations.

``` r
NYC_transit_df |>
  filter(ada) |>
  distinct(line, station_name) |>
  nrow()
```

    ## [1] 84

There are 84 ADA compliant stations.

``` r
NYC_transit_df |>
  filter(vending == "NO") |>
  pull(entry) |>
  mean()
```

    ## [1] 0.3770492

0.38 of the station entrances/exits without vending allow entrance

``` r
NYC_transit_reformat_df =
  NYC_transit_df |>
  mutate(route8 = as.character(route8),
         route9 = as.character(route9),
         route10 = as.character(route10),
         route11 = as.character(route11)) |>
  pivot_longer(
    route1:route11,
    names_to = "route_number",
    values_to = "route_name",
    names_prefix = "route",
    values_drop_na = TRUE
  )

# Distinct stations that serve the A
A_train =
  NYC_transit_reformat_df |>
  filter(route_name == "A") |>
  distinct(station_name, line, .keep_all = TRUE) |>
  nrow()

# A stations that are ADA compliant
ADA_A_stations = 
  NYC_transit_reformat_df |>
  filter(route_name == "A", ada) |>
  distinct(line, station_name) |>
  nrow()
```

There are 60 distinct stations that serve the A train. Of the stations
that serve the A train, 17 are ADA compliant.

\#Question 2

``` r
library(readxl)
mrtrash_df = 
  read_excel("./202610 Trash Wheel Collection Data.xlsx",
             sheet = "Mr. Trash Wheel",
             range = "A2:N742") |>
  janitor::clean_names() |>
  drop_na(dumpster) |>
  mutate(sports_balls = as.integer(round(sports_balls)),
         year = as.numeric(year),
         trash_wheel = "Mr. Trash Wheel")
```

``` r
professortrash_df =
  read_excel("./202610 Trash Wheel Collection Data.xlsx",
             sheet = "Professor Trash Wheel",
             range = "A2:M139") |>
  janitor::clean_names() |>
  drop_na(dumpster) |>
  mutate(year = as.numeric(year),
         plastic_bottles = as.numeric(plastic_bottles),
         trash_wheel = "Professor Trash Wheel")
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `plastic_bottles = as.numeric(plastic_bottles)`.
    ## Caused by warning:
    ## ! NAs introduced by coercion

``` r
gwynnda_df =
  read_excel("./202610 Trash Wheel Collection Data.xlsx",
             sheet = "Gwynnda the Good Wheel of the W",
             range = "A2:L394") |>
  janitor::clean_names() |>
  drop_na(dumpster) |>
  mutate(year = as.numeric(year),
         trash_wheel = "Gwynnda Trash Wheel")
```

\#Combining datasets

``` r
trashwheels_df =
  bind_rows(mrtrash_df, professortrash_df, gwynnda_df) |>
  janitor::clean_names()
```

The final trashwheels dataset has a total of 1269 observations. These
observations are a combination of data from Mr. Trash Wheel, Gwynnda the
Good Wheel of the W, and Professor Trash Wheel.

Professor Trash Wheel collected 294.96 tons of trash.

Gwynnda collected a total number of 18120cigarette butts in June 2022.

\#Question 3

``` r
mci_df = read_csv("./MCI_baseline.csv",
                  skip = 1,
                  na = c("NA", ".", "")) |>
  janitor::clean_names() |>
  mutate(
    sex = case_match(sex, 1 ~"male", 0 ~ "female"),
    apoe4 = case_match(apoe4, 1 ~ "carrier", 0 ~ "non-carrier"))
```

    ## Rows: 483 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (6): ID, Current Age, Sex, Education, apoe4, Age at onset
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
mci_baseline =
  mci_df |>
  filter(is.na(age_at_onset) | age_at_onset > current_age)
```

The first line of the imported file was skipped as it did not contain
data, rather comments.Sex and apoe4 were recoded and converted from
numerical to character values. We then filterd to keep those who did not
develop MCI/ MCI came after baseline age.

There were 483 participants recruited. Of those recruited, 93 developed
MCI. The average baseline age is 65 years. 30 % of women in the study
are apoe4 carriers.

``` r
mci_amyloid_df =
  read_csv("mci_amyloid.csv",
           skip = 1,
           na = c("NA", ".", "")) |>
  janitor::clean_names() |>
  rename(id = study_id)
```

    ## Rows: 487 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (5): Baseline, Time 2, Time 4, Time 6, Time 8
    ## dbl (1): Study ID
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

The first line of the imported file was skipped as it did not contain
data, rather comments. ‘Study_id’ was renamed to ‘id’ in order to match
the baseline data.This dataset has 487 participants.

``` r
anti_join(mci_baseline, mci_amyloid_df, by = "id") |>
  nrow()
```

    ## [1] 8

``` r
anti_join(mci_amyloid_df, mci_baseline, by = "id") |>
  nrow()
```

    ## [1] 16

There are 8 participants in the baseline that are not in the amyloid
dataset. There are 16 participants in the amyloid that are not in the
baseline dataset.

``` r
mci_combined_df =
  inner_join(mci_baseline, mci_amyloid_df, by = "id")

write_csv(mci_combined_df, "mci_combined_df.csv")
```

There are 471 subjects in the combined dataset.There is a combined total
of 11 variables.
