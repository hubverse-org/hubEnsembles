
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Example Simple Forecast Hub

**This repository is under construction.**

This repository is designed as an example modeling Hub that follows the
infrastructure guidelines laid out by the [Consortium of Infectious
Disease Modeling
Hubs](https://github.com/Infectious-Disease-Modeling-Hubs/). The example
is documented in more detail
[here](https://hubdocs.readthedocs.io/en/latest/format/intro-data-formats.html#running-examples).

The example model outputs that are provided here are adapted from
forecasts submitted to the US COVID-19 Forecast Hub, but have been
modified to provide examples of nowcasts. They should be viewed only as
illustrations of the data formats, not as realistic examples of nowcasts
and forecasts. In particular, scores calculated by comparing the model
outputs to the target data will not give a meaningul measure of
predictive skill.

## Working with the data

To work with the data in R, you can use code like the following:

``` r
library(hubUtils)
library(dplyr)

model_outputs <- hubUtils::connect_hub(hub_path = ".") %>%
    dplyr::collect()
head(model_outputs)
#> # A tibble: 6 × 8
#>   origin_date horizon location target  output_type output_type_id value model_id
#>   <date>        <int> <chr>    <chr>   <chr>                <dbl> <int> <chr>   
#> 1 2022-12-05       -6 20       inc co… quantile             0.01     22 UMass-ar
#> 2 2022-12-05       -6 20       inc co… quantile             0.025    24 UMass-ar
#> 3 2022-12-05       -6 20       inc co… quantile             0.05     26 UMass-ar
#> 4 2022-12-05       -6 20       inc co… quantile             0.1      28 UMass-ar
#> 5 2022-12-05       -6 20       inc co… quantile             0.15     30 UMass-ar
#> 6 2022-12-05       -6 20       inc co… quantile             0.2      32 UMass-ar

target_data <- read.csv("target-data/covid-hospitalizations.csv")
head(target_data)
#>     time_idx location value         target
#> 1 2021-03-21       46    12 inc covid hosp
#> 2 2021-03-04       45    82 inc covid hosp
#> 3 2021-02-26       46     7 inc covid hosp
#> 4 2021-02-20       44    21 inc covid hosp
#> 5 2021-02-09       44    19 inc covid hosp
#> 6 2021-01-25       25   224 inc covid hosp
```
