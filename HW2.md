HW2
================

loading library for hw2

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   4.0.0     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

Loading 3 datasets

``` r
pols_data <-
  read_csv("~/Desktop/💻/按课程类型分类/P8105 Data Science 1/Homeworks/HW2/fivethirtyeight_datasets/pols-month.csv",na = c("NA",".","") ) |> 
  janitor::clean_names()
```

    ## Rows: 822 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (8): prez_gop, gov_gop, sen_gop, rep_gop, prez_dem, gov_dem, sen_dem, r...
    ## date (1): mon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
unemployment_data  <-
  read_csv("~/Desktop/💻/按课程类型分类/P8105 Data Science 1/Homeworks/HW2/fivethirtyeight_datasets/unemployment.csv",na = c("NA",".","") ) |> 
  janitor::clean_names()
```

    ## Rows: 68 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (13): Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
snp_data  <-
  read_csv("~/Desktop/💻/按课程类型分类/P8105 Data Science 1/Homeworks/HW2/fivethirtyeight_datasets/snp.csv",na = c("NA",".","") )  |> 
  janitor::clean_names()
```

    ## Rows: 787 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): date
    ## dbl (1): close
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Use separate() to break up the variable mon into integer variables

``` r
d <- pols_data |> 
  
  separate(mon, into = c("year","month","day"), sep = "-") |> 
  
    mutate(
    year = as.numeric(year),
    month = as.numeric(month),
    day = as.numeric(day)
    ) |> 
  
  rename(gop = prez_gop) |> 
  rename(dem = prez_dem) |> 
  mutate(
    month = case_match(
      month,
      1 ~ "jan",
      2 ~ "feb",
      3  ~ "mar", 
      4  ~ "apr", 
      5  ~ "may", 
      6  ~ "jun", 
      7  ~ "jul", 
      8  ~ "aug", 
      9  ~ "sep", 
      10 ~ "oct", 
      11 ~ "nov",
      12 ~ "dec"
    ),
  month = as.factor(month)
  ) |> 
  select(-day) |> 
  pivot_longer(
    col = c(gop,dem),
    names_to = "president",
    values_to = "value"
  ) |> 
  select(-value) |> 
  arrange(desc(year))
d
```

    ## # A tibble: 1,644 × 9
    ##     year month gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem president
    ##    <dbl> <fct>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>    
    ##  1  2015 jan        31      54     245      18      44     188 gop      
    ##  2  2015 jan        31      54     245      18      44     188 dem      
    ##  3  2015 feb        31      54     245      18      44     188 gop      
    ##  4  2015 feb        31      54     245      18      44     188 dem      
    ##  5  2015 mar        31      54     245      18      44     188 gop      
    ##  6  2015 mar        31      54     245      18      44     188 dem      
    ##  7  2015 apr        31      54     244      18      44     188 gop      
    ##  8  2015 apr        31      54     244      18      44     188 dem      
    ##  9  2015 may        31      54     245      18      44     188 gop      
    ## 10  2015 may        31      54     245      18      44     188 dem      
    ## # ℹ 1,634 more rows

``` r
p <- snp_data |> 
  
  separate(date, into = c("month","day","year"), sep = "/") |>
  mutate(
    year = as.numeric(year),
    month = as.numeric(month),
    day = as.numeric(day)
    ) |> 
  select(year,month,day,close) |> 
  mutate(
  year = if_else(year < 30, year + 2000, year + 1900)
) |> 
  mutate(
    month = case_match(
      month,
      1 ~ "jan",
      2 ~ "feb",
      3  ~ "mar", 
      4  ~ "apr", 
      5  ~ "may", 
      6  ~ "jun", 
      7  ~ "jul", 
      8  ~ "aug", 
      9  ~ "sep", 
      10 ~ "oct", 
      11 ~ "nov",
      12 ~ "dec"
    ),
  month = as.factor(month)
  ) |> 
  select(-day)|> 
  arrange(desc(year))

p
```

    ## # A tibble: 787 × 3
    ##     year month close
    ##    <dbl> <fct> <dbl>
    ##  1  2015 jul   2080.
    ##  2  2015 jun   2063.
    ##  3  2015 may   2107.
    ##  4  2015 apr   2086.
    ##  5  2015 mar   2068.
    ##  6  2015 feb   2104.
    ##  7  2015 jan   1995.
    ##  8  2014 dec   2059.
    ##  9  2014 nov   2068.
    ## 10  2014 oct   2018.
    ## # ℹ 777 more rows

``` r
q <- unemployment_data |> 
  pivot_longer(
    cols = c(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec),
    names_to = "month",
    values_to = "rate"
    )|> 
  arrange(desc(year))
q
```

    ## # A tibble: 816 × 3
    ##     year month  rate
    ##    <dbl> <chr> <dbl>
    ##  1  2015 jan     5.7
    ##  2  2015 feb     5.5
    ##  3  2015 mar     5.5
    ##  4  2015 apr     5.4
    ##  5  2015 may     5.5
    ##  6  2015 jun     5.3
    ##  7  2015 jul    NA  
    ##  8  2015 aug    NA  
    ##  9  2015 sep    NA  
    ## 10  2015 oct    NA  
    ## # ℹ 806 more rows

``` r
final_data <- d |> 
  left_join(q, by = c("year", "month")) |> 
  left_join(p, by = c("year", "month"))
final_data
```

    ## # A tibble: 1,644 × 11
    ##     year month gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem president  rate
    ##    <dbl> <chr>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>     <dbl>
    ##  1  2015 jan        31      54     245      18      44     188 gop         5.7
    ##  2  2015 jan        31      54     245      18      44     188 dem         5.7
    ##  3  2015 feb        31      54     245      18      44     188 gop         5.5
    ##  4  2015 feb        31      54     245      18      44     188 dem         5.5
    ##  5  2015 mar        31      54     245      18      44     188 gop         5.5
    ##  6  2015 mar        31      54     245      18      44     188 dem         5.5
    ##  7  2015 apr        31      54     244      18      44     188 gop         5.4
    ##  8  2015 apr        31      54     244      18      44     188 dem         5.4
    ##  9  2015 may        31      54     245      18      44     188 gop         5.5
    ## 10  2015 may        31      54     245      18      44     188 dem         5.5
    ## # ℹ 1,634 more rows
    ## # ℹ 1 more variable: close <dbl>

The pols-month.csv data contains partisan control of the US presidential
election and Congress, recorded on a monthly basis.

The snp.csv data contains closing prices for the S&P 500 index,
documenting the performance of financial markets.

The unemployment.csv data provides the monthly unemployment rate for the
US.

After cleaning and merging, we obtain a tidy data frame, `final_data`,
with 1644 rows and 11 columns, spanning approximately 1947 to 2015
years. Key variables include:

year, month: Time key

president: Presidential party (gov/dem)

close: S&P 500 monthly closing index

unemployment: Monthly unemployment rate

This merged dataset allows us to simultaneously examine the relationship
between politics, the economy, and the labor market.

``` r
library(readxl)
```

``` r
Trash_Wheel_data <- 
  janitor::clean_names(read_excel("~/Desktop/💻/按课程类型分类/P8105 Data Science 1/Homeworks/HW2/202509 Trash Wheel Collection Data.xlsx", sheet = "Mr. Trash Wheel") ) 
```

    ## New names:
    ## • `` -> `...15`
    ## • `` -> `...16`

``` r
Trash_Wheel_data <- Trash_Wheel_data |>
  mutate(year = as.numeric(year)) |> 
  select(
    "dumpster","month","year","date","weight_tons","volume_cubic_yards",
    "plastic_bottles","polystyrene","cigarette_butts","glass_bottles",
    "plastic_bags","wrappers","homes_powered","sports_balls"
  ) |> 
  filter(!is.na(date)) |> 
  mutate(sports_balls = as.integer( round(sports_balls ) ) ) |> 
  mutate(wheel_name   = "Mr. Trash Wheel" )
Trash_Wheel_data
```

    ## # A tibble: 707 × 15
    ##    dumpster month  year date                weight_tons volume_cubic_yards
    ##       <dbl> <chr> <dbl> <dttm>                    <dbl>              <dbl>
    ##  1        1 May    2014 2014-05-16 00:00:00        4.31                 18
    ##  2        2 May    2014 2014-05-16 00:00:00        2.74                 13
    ##  3        3 May    2014 2014-05-16 00:00:00        3.45                 15
    ##  4        4 May    2014 2014-05-17 00:00:00        3.1                  15
    ##  5        5 May    2014 2014-05-17 00:00:00        4.06                 18
    ##  6        6 May    2014 2014-05-20 00:00:00        2.71                 13
    ##  7        7 May    2014 2014-05-21 00:00:00        1.91                  8
    ##  8        8 May    2014 2014-05-28 00:00:00        3.7                  16
    ##  9        9 June   2014 2014-06-05 00:00:00        2.52                 14
    ## 10       10 June   2014 2014-06-11 00:00:00        3.76                 18
    ## # ℹ 697 more rows
    ## # ℹ 9 more variables: plastic_bottles <dbl>, polystyrene <dbl>,
    ## #   cigarette_butts <dbl>, glass_bottles <dbl>, plastic_bags <dbl>,
    ## #   wrappers <dbl>, homes_powered <dbl>, sports_balls <int>, wheel_name <chr>

``` r
Professor_Trash_Wheel_data <- 
  janitor::clean_names(read_excel("~/Desktop/💻/按课程类型分类/P8105 Data Science 1/Homeworks/HW2/202509 Trash Wheel Collection Data.xlsx", sheet = "Professor Trash Wheel") )

Professor_Trash_Wheel_data <- Professor_Trash_Wheel_data |> 
  select(
    "dumpster","month","year","date","weight_tons","volume_cubic_yards",
    "plastic_bottles","polystyrene","cigarette_butts","glass_bottles",
    "plastic_bags","wrappers","homes_powered"
  ) |> 
  filter(!is.na(date)) |> 
  mutate(wheel_name   = "Professor Trash Wheel" )
Professor_Trash_Wheel_data
```

    ## # A tibble: 132 × 14
    ##    dumpster month     year date                weight_tons volume_cubic_yards
    ##       <dbl> <chr>    <dbl> <dttm>                    <dbl>              <dbl>
    ##  1        1 January   2017 2017-01-02 00:00:00        1.79                 15
    ##  2        2 January   2017 2017-01-30 00:00:00        1.58                 15
    ##  3        3 February  2017 2017-02-26 00:00:00        2.32                 18
    ##  4        4 February  2017 2017-02-26 00:00:00        3.72                 15
    ##  5        5 February  2017 2017-02-28 00:00:00        1.45                 15
    ##  6        6 March     2017 2017-03-30 00:00:00        1.71                 15
    ##  7        7 April     2017 2017-04-01 00:00:00        1.82                 15
    ##  8        8 April     2017 2017-04-20 00:00:00        2.37                 15
    ##  9        9 May       2017 2017-05-10 00:00:00        2.64                 15
    ## 10       10 May       2017 2017-05-26 00:00:00        2.78                 15
    ## # ℹ 122 more rows
    ## # ℹ 8 more variables: plastic_bottles <dbl>, polystyrene <dbl>,
    ## #   cigarette_butts <dbl>, glass_bottles <dbl>, plastic_bags <dbl>,
    ## #   wrappers <dbl>, homes_powered <dbl>, wheel_name <chr>

``` r
Gwynnda_Trash_Wheel_data <-
  janitor::clean_names(read_excel("~/Desktop/💻/按课程类型分类/P8105 Data Science 1/Homeworks/HW2/202509 Trash Wheel Collection Data.xlsx", sheet = "Gwynns Falls Trash Wheel") )

Gwynnda_Trash_Wheel_data <- Gwynnda_Trash_Wheel_data |> 
  select(
    "dumpster","month","year","date","weight_tons","volume_cubic_yards",
    "plastic_bottles","polystyrene","cigarette_butts",
    "plastic_bags","wrappers","homes_powered"
  ) |> 
  filter(!is.na(date)) |> 
  mutate(wheel_name   = "Gwynnda Trash Wheel" )
Gwynnda_Trash_Wheel_data
```

    ## # A tibble: 349 × 13
    ##    dumpster month   year date                weight_tons volume_cubic_yards
    ##       <dbl> <chr>  <dbl> <dttm>                    <dbl>              <dbl>
    ##  1        1 July    2021 2021-07-03 00:00:00        0.93                 15
    ##  2        2 July    2021 2021-07-07 00:00:00        2.26                 15
    ##  3        3 July    2021 2021-07-07 00:00:00        1.62                 15
    ##  4        4 July    2021 2021-07-16 00:00:00        1.76                 15
    ##  5        5 July    2021 2021-07-30 00:00:00        1.53                 15
    ##  6        6 August  2021 2021-08-11 00:00:00        2.06                 15
    ##  7        7 August  2021 2021-08-14 00:00:00        1.9                  15
    ##  8        8 August  2021 2021-08-16 00:00:00        2.16                 15
    ##  9        9 August  2021 2021-08-16 00:00:00        2.6                  15
    ## 10       10 August  2021 2021-08-17 00:00:00        3.21                 15
    ## # ℹ 339 more rows
    ## # ℹ 7 more variables: plastic_bottles <dbl>, polystyrene <dbl>,
    ## #   cigarette_butts <dbl>, plastic_bags <dbl>, wrappers <dbl>,
    ## #   homes_powered <dbl>, wheel_name <chr>

``` r
Final_Trash_wheel_data <- bind_rows(Trash_Wheel_data,Professor_Trash_Wheel_data,Gwynnda_Trash_Wheel_data) |> 
  arrange(date, wheel_name)
Final_Trash_wheel_data
```

    ## # A tibble: 1,188 × 15
    ##    dumpster month  year date                weight_tons volume_cubic_yards
    ##       <dbl> <chr> <dbl> <dttm>                    <dbl>              <dbl>
    ##  1        1 May    2014 2014-05-16 00:00:00        4.31                 18
    ##  2        2 May    2014 2014-05-16 00:00:00        2.74                 13
    ##  3        3 May    2014 2014-05-16 00:00:00        3.45                 15
    ##  4        4 May    2014 2014-05-17 00:00:00        3.1                  15
    ##  5        5 May    2014 2014-05-17 00:00:00        4.06                 18
    ##  6        6 May    2014 2014-05-20 00:00:00        2.71                 13
    ##  7        7 May    2014 2014-05-21 00:00:00        1.91                  8
    ##  8        8 May    2014 2014-05-28 00:00:00        3.7                  16
    ##  9        9 June   2014 2014-06-05 00:00:00        2.52                 14
    ## 10       10 June   2014 2014-06-11 00:00:00        3.76                 18
    ## # ℹ 1,178 more rows
    ## # ℹ 9 more variables: plastic_bottles <dbl>, polystyrene <dbl>,
    ## #   cigarette_butts <dbl>, glass_bottles <dbl>, plastic_bags <dbl>,
    ## #   wrappers <dbl>, homes_powered <dbl>, sports_balls <int>, wheel_name <chr>

``` r
Professor_total_weight <- Professor_Trash_Wheel_data |> 
  pull(weight_tons) |> 
  sum(na.rm = TRUE)
Professor_total_weight
```

    ## [1] 282.26

``` r
Gwynnda_202206_cigarette <- Gwynnda_Trash_Wheel_data |> 
  filter(year(date) == 2022, month(date) == 6) |> 
  pull(cigarette_butts) |>
  sum(na.rm = TRUE)
Gwynnda_202206_cigarette
```

    ## [1] 18120

## Explaination

We collated and merged the datasets from the three collection devices,
resulting in a tidy data frame with 1644 observations. Examples of key
variables include c(2015, 2015, 2015, 2015, 2015, 2015), c(“jan”, “jan”,
“feb”, “feb”, “mar”, “mar”), c(31, 31, 31, 31, 31, 31), c(54, 54, 54,
54, 54, 54), c(245, 245, 245, 245, 245, 245), c(18, 18, 18, 18, 18, 18),
c(44, 44, 44, 44, 44, 44), c(188, 188, 188, 188, 188, 188), c(“gop”,
“dem”, “gop”, “dem”, “gop”, “dem”), c(5.7, 5.7, 5.5, 5.5, 5.5, 5.5),
c(1994.98999, 1994.98999, 2104.5, 2104.5, 2067.889893, 2067.889893).

Professor Trash Wheel collected a total of **282.3 tons of trash in the
available data. Gwynnda collected a total of **18,120\*\* cigarette
butts in **June 2022**.

For reproducibility, we imported the data using `readxl::read_excel()`
with the specified worksheet and omitted rows and columns containing
annotations/charts during cleaning (this was achieved by filtering out
missing rows for `date` and `dumpster` and retaining only the relevant
columns for trash items). Additionally, we round the *Sports Balls*
variable to the nearest integer and convert it to integer type
(`as.integer(round(...))`) to be consistent with downstream analyses.

``` r
zip_data  <-
  read_csv("~/Desktop/💻/按课程类型分类/P8105 Data Science 1/Homeworks/HW2/zillow_data/Zip Codes.csv",na = c("NA",".","") ) |> 
  janitor::clean_names()
```

    ## Rows: 322 Columns: 7
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (4): County, County Code, File Date, Neighborhood
    ## dbl (3): State FIPS, County FIPS, ZipCode
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
zip_data
```

    ## # A tibble: 322 × 7
    ##    county state_fips county_code county_fips zip_code file_date neighborhood    
    ##    <chr>       <dbl> <chr>             <dbl>    <dbl> <chr>     <chr>           
    ##  1 Bronx          36 005               36005    10451 7/25/07   High Bridge and…
    ##  2 Bronx          36 005               36005    10452 7/25/07   High Bridge and…
    ##  3 Bronx          36 005               36005    10453 7/25/07   Central Bronx   
    ##  4 Bronx          36 005               36005    10454 7/25/07   Hunts Point and…
    ##  5 Bronx          36 005               36005    10455 7/25/07   Hunts Point and…
    ##  6 Bronx          36 005               36005    10456 7/25/07   High Bridge and…
    ##  7 Bronx          36 005               36005    10457 7/25/07   Central Bronx   
    ##  8 Bronx          36 005               36005    10458 7/25/07   Bronx Park and …
    ##  9 Bronx          36 005               36005    10459 7/25/07   Hunts Point and…
    ## 10 Bronx          36 005               36005    10460 7/25/07   Central Bronx   
    ## # ℹ 312 more rows

``` r
zip_zori_data  <-
  read_csv("~/Desktop/💻/按课程类型分类/P8105 Data Science 1/Homeworks/HW2/zillow_data/Zip_zori_uc_sfrcondomfr_sm_month_NYC.csv",na = c("NA",".","") ) |>
  janitor::clean_names()
```

    ## Rows: 149 Columns: 125
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (6): RegionType, StateName, State, City, Metro, CountyName
    ## dbl (119): RegionID, SizeRank, RegionName, 2015-01-31, 2015-02-28, 2015-03-3...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
zip_zori_data
```

    ## # A tibble: 149 × 125
    ##    region_id size_rank region_name region_type state_name state city     metro  
    ##        <dbl>     <dbl>       <dbl> <chr>       <chr>      <chr> <chr>    <chr>  
    ##  1     62080         4       11368 zip         NY         NY    New York New Yo…
    ##  2     62093         7       11385 zip         NY         NY    New York New Yo…
    ##  3     62019         9       11208 zip         NY         NY    New York New Yo…
    ##  4     62046        16       11236 zip         NY         NY    New York New Yo…
    ##  5     61807        17       10467 zip         NY         NY    New York New Yo…
    ##  6     62085        18       11373 zip         NY         NY    New York New Yo…
    ##  7     62037        19       11226 zip         NY         NY    New York New Yo…
    ##  8     62018        20       11207 zip         NY         NY    New York New Yo…
    ##  9     61639        29       10025 zip         NY         NY    New York New Yo…
    ## 10     62025        32       11214 zip         NY         NY    New York New Yo…
    ## # ℹ 139 more rows
    ## # ℹ 117 more variables: county_name <chr>, x2015_01_31 <dbl>,
    ## #   x2015_02_28 <dbl>, x2015_03_31 <dbl>, x2015_04_30 <dbl>, x2015_05_31 <dbl>,
    ## #   x2015_06_30 <dbl>, x2015_07_31 <dbl>, x2015_08_31 <dbl>, x2015_09_30 <dbl>,
    ## #   x2015_10_31 <dbl>, x2015_11_30 <dbl>, x2015_12_31 <dbl>, x2016_01_31 <dbl>,
    ## #   x2016_02_29 <dbl>, x2016_03_31 <dbl>, x2016_04_30 <dbl>, x2016_05_31 <dbl>,
    ## #   x2016_06_30 <dbl>, x2016_07_31 <dbl>, x2016_08_31 <dbl>, …

``` r
zip_data_1 <- zip_data |> 
  mutate(zip_code = as.character(zip_code)) |> 
  select(zip_code, county, neighborhood) |> 
  distinct() |> 
  arrange(zip_code, county) |> 
    distinct(zip_code, .keep_all = TRUE) |> 
  mutate(
    borough = case_when(
      county == "Bronx"    ~ "Bronx",
      county == "Kings"    ~ "Brooklyn",
      county == "New York" ~ "Manhattan",
      county == "Queens"   ~ "Queens",
      county == "Richmond" ~ "Staten Island",
    )
  )
zip_data_1
```

    ## # A tibble: 320 × 4
    ##    zip_code county   neighborhood                  borough  
    ##    <chr>    <chr>    <chr>                         <chr>    
    ##  1 10001    New York Chelsea and Clinton           Manhattan
    ##  2 10002    New York Lower East Side               Manhattan
    ##  3 10003    New York Lower East Side               Manhattan
    ##  4 10004    New York Lower Manhattan               Manhattan
    ##  5 10005    New York Lower Manhattan               Manhattan
    ##  6 10006    New York Lower Manhattan               Manhattan
    ##  7 10007    New York Lower Manhattan               Manhattan
    ##  8 10008    New York <NA>                          Manhattan
    ##  9 10009    New York Lower East Side               Manhattan
    ## 10 10010    New York Gramercy Park and Murray Hill Manhattan
    ## # ℹ 310 more rows

``` r
zip_zori_data_1 <- zip_zori_data |> 
  pivot_longer(
    x2015_01_31:x2024_08_31,
    names_to = "date",
    values_to = "zori"
  ) |> 
  mutate(date = ymd(str_remove(date, "^x"))) |> 
  mutate(zip_code = as.character(region_name))
zip_zori_data_1
```

    ## # A tibble: 17,284 × 12
    ##    region_id size_rank region_name region_type state_name state city     metro  
    ##        <dbl>     <dbl>       <dbl> <chr>       <chr>      <chr> <chr>    <chr>  
    ##  1     62080         4       11368 zip         NY         NY    New York New Yo…
    ##  2     62080         4       11368 zip         NY         NY    New York New Yo…
    ##  3     62080         4       11368 zip         NY         NY    New York New Yo…
    ##  4     62080         4       11368 zip         NY         NY    New York New Yo…
    ##  5     62080         4       11368 zip         NY         NY    New York New Yo…
    ##  6     62080         4       11368 zip         NY         NY    New York New Yo…
    ##  7     62080         4       11368 zip         NY         NY    New York New Yo…
    ##  8     62080         4       11368 zip         NY         NY    New York New Yo…
    ##  9     62080         4       11368 zip         NY         NY    New York New Yo…
    ## 10     62080         4       11368 zip         NY         NY    New York New Yo…
    ## # ℹ 17,274 more rows
    ## # ℹ 4 more variables: county_name <chr>, date <date>, zori <dbl>,
    ## #   zip_code <chr>

``` r
final_tidy <- zip_zori_data_1 |> 
  left_join(zip_data_1, by = "zip_code") |> 

  select(
    zip_code, borough, neighborhood, date, zori,
    region_id, size_rank, region_type, state_name, state, city, metro, county_name
  ) |> 
  arrange(zip_code, date)
final_tidy
```

    ## # A tibble: 17,284 × 13
    ##    zip_code borough   neighborhood        date        zori region_id size_rank
    ##    <chr>    <chr>     <chr>               <date>     <dbl>     <dbl>     <dbl>
    ##  1 10001    Manhattan Chelsea and Clinton 2015-01-31 3855.     61615      4444
    ##  2 10001    Manhattan Chelsea and Clinton 2015-02-28 3892.     61615      4444
    ##  3 10001    Manhattan Chelsea and Clinton 2015-03-31 3898.     61615      4444
    ##  4 10001    Manhattan Chelsea and Clinton 2015-04-30 3970.     61615      4444
    ##  5 10001    Manhattan Chelsea and Clinton 2015-05-31 4033.     61615      4444
    ##  6 10001    Manhattan Chelsea and Clinton 2015-06-30 4071.     61615      4444
    ##  7 10001    Manhattan Chelsea and Clinton 2015-07-31 4067.     61615      4444
    ##  8 10001    Manhattan Chelsea and Clinton 2015-08-31 4070.     61615      4444
    ##  9 10001    Manhattan Chelsea and Clinton 2015-09-30 4040.     61615      4444
    ## 10 10001    Manhattan Chelsea and Clinton 2015-10-31 4023.     61615      4444
    ## # ℹ 17,274 more rows
    ## # ℹ 6 more variables: region_type <chr>, state_name <chr>, state <chr>,
    ## #   city <chr>, metro <chr>, county_name <chr>

``` r
x2020_2021_answer <- final_tidy |> 
  filter(
    (date >= ymd("2020-01-01") & date <= ymd("2020-01-31")) |
    (date >= ymd("2021-01-01") & date <= ymd("2021-01-31"))
  ) |> 
  mutate(date = format(date, "%Y-%m")) |> 
  select(zip_code, borough, neighborhood, date, zori) |> 
  pivot_wider(
    names_from = date, 
    values_from = zori
    ) |> 
  rename(Y2020 = `2020-01`, Y2021 = `2021-01`) |> 
  mutate(
    change = Y2021 - Y2020,
    change_percent = round(100 * change / Y2020 , 2)
  ) |> 
  arrange(change)

top10_change <- x2020_2021_answer |> 
  slice_head(n = 10) |> 
  select(zip_code, borough, neighborhood, Y2020, Y2021, change, change_percent)

top10_change
```

    ## # A tibble: 10 × 7
    ##    zip_code borough   neighborhood             Y2020 Y2021 change change_percent
    ##    <chr>    <chr>     <chr>                    <dbl> <dbl>  <dbl>          <dbl>
    ##  1 10007    Manhattan Lower Manhattan          6334. 5422.  -913.          -14.4
    ##  2 10069    Manhattan <NA>                     4623. 3875.  -748.          -16.2
    ##  3 10009    Manhattan Lower East Side          3406. 2692.  -714.          -21.0
    ##  4 10016    Manhattan Gramercy Park and Murra… 3731. 3019.  -712.          -19.1
    ##  5 10001    Manhattan Chelsea and Clinton      4108. 3398.  -710.          -17.3
    ##  6 10002    Manhattan Lower East Side          3645. 2935.  -710.          -19.5
    ##  7 10004    Manhattan Lower Manhattan          3150. 2444.  -706.          -22.4
    ##  8 10038    Manhattan Lower Manhattan          3573. 2876.  -698.          -19.5
    ##  9 10012    Manhattan Greenwich Village and S… 3629. 2942.  -686.          -18.9
    ## 10 10010    Manhattan Gramercy Park and Murra… 3697. 3012.  -685.          -18.5

17284 observations, 13 variables， 149distinct zip code, and 42 distinct
community

Before the merge, I removed the duplicated ZIP table, resolving rare
duplicate rows caused by cross-county mis-entries, such as 11201 (should
belong to Kings/Brooklyn) and 10463 (should belong to the Bronx). This
ensured that after the merge, each ZIP had only one record per month.

Zillow’s ZORI often excludes ZIPs designated for PO Boxes, work units,
airports, and corporate buildings because they often lack available
rental samples. A small number of “border/suburban ZIPs” (such as 115xx)
may be included in the ZIP list, but they fall outside the coverage of
the NYC five-county area or the Zillow NYC dataset and are therefore
also missing.

ZORIs for some ZIPs in the early years (2015–2017) were missing, but
coverage has increased annually. Therefore, when performing
year-over-year/month-over-month comparisons, it is recommended to use
fixed months and filter out missing data.
