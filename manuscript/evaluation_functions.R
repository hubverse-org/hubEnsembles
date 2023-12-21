#' Evaluate forecasts (from scores) across different evaluation groupings
#'
#' @param scores A data frame of forecast scores to be summarized. Must have columns that match the output from `covidHubUtils::score_forecasts()` and include scores for a baseline model.
#' @param grouping_variables A character vector specifying the type of grouping to be used in evaluation. Options are `season`, `horizon`, `forecast_week`, `location`.
#' @param baseline_name A character string specifying the name of the baseline model to calculate relative metrics against
#' @param us_only A boolean specifying whether to summarize metrics for the us national level only or just states
#'
#' @return A data frame of summarized forecast score metrics across all provided forecasts
#' @export
#'
#' @examples
evaluate_flu_scores <- function(scores, grouping_variables, baseline_name, us_only=FALSE) {
  if (isFALSE("location" %in% grouping_variables)) {
    if (us_only) {
      scores <- filter(scores, location=="US")
    } else {
      scores <- filter(scores, location!="US")
    }
  }
  
  if ("season" %in% grouping_variables & isFALSE("season" %in% names(scores))) {
    scores <- scores |>
      dplyr::mutate(season = ifelse(forecast_date < as.Date("2022-08-01"), 
                                    "2021-2022", "2022-2023"))
  }
  
  summarized_scores_all <- scores |>
    dplyr::group_by(model, dplyr::across(dplyr::all_of(grouping_variables))) |>
    dplyr::summarize(wis = mean(wis), mae = mean(abs_error), 
                     cov50 = mean(coverage_50), cov95 = mean(coverage_95))
                     
  summarized_scores_baseline <- scores |>
    dplyr::filter(model==baseline_name) |>
    dplyr::group_by(model, dplyr::across(dplyr::all_of(grouping_variables))) |>
    dplyr::summarize(base_wis = mean(wis), base_mae = mean(abs_error)) |>
    dplyr::ungroup() |>
    dplyr::select(-model) 
    
  if (is.null(grouping_variables)) {
    summarized_scores_all <- summarized_scores_all |>
      dplyr::mutate(base_wis = unique(summarized_scores_baseline$base_wis),
                    base_mae = unique(summarized_scores_baseline$base_mae))
  } else {
    summarized_scores_all <- summarized_scores_all |>
      dplyr::left_join(summarized_scores_baseline, by = grouping_variables) 
    
  }
  
  summarized_scores_all |>
    dplyr::mutate(rwis = case_when(base_wis != 0 ~ wis/base_wis,
                                    base_wis == 0 & wis == 0 ~ 1,
                                    base_wis == 0 & wis != 0 ~ Inf),
                  rmae = case_when(base_mae != 0 ~ mae/base_mae,
                                    base_mae == 0 & mae == 0 ~ 1,
                                    base_mae == 0 & mae != 0 ~ Inf)) |>
    dplyr::select(-base_wis, -base_mae) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_variables))) |>
    mutate(across(where(is.numeric), round, digits=3)) |>
    dplyr::arrange(wis, .by_group=TRUE)

}

#' Plot summarized metrics against horizon
#'
#' @param summarized_scores A data frame of summarized scores. Must contain one row per model and horizon combination plus a `horizon` column
#' @param model_names An ordered vector of model names
#' @param model_colors An ordered vector of model colors. Must match with `model_names` order
#' @param y_var A string specifying which metric to plot as the y-variable
#' @param main A string specifying the plot title
#'
#' @return A scatter plot (with observations connected by lines) of the specified summary metric vs horizon
#' @export
#'
#' @examples
plot_evaluated_scores <- function(summarized_scores, model_names, model_colors, y_var="wis", main) {

  if (y_var == "wis") {
    gg <- ggplot(summarized_scores, mapping=aes(x=horizon, y=wis, group=model)) +
      coord_cartesian(ylim = c(min(summarized_scores$wis)*0.9, median(filter(summarized_scores, horizon==4)$wis)*1.5))
  } else if (y_var == "mae") {
    gg <- ggplot(summarized_scores, mapping=aes(x=horizon, y=mae, group=model)) +
      coord_cartesian(ylim = c(min(summarized_scores$mae)*0.9, median(filter(summarized_scores, horizon==4)$mae)*1.5))
  } else if (y_var == "cov95") {
    gg <- ggplot(summarized_scores, mapping=aes(x=horizon, y=cov95, group=model)) +
      geom_hline(aes(yintercept=0.95)) 
  } else if (y_var == "cov50") {
    gg <- ggplot(summarized_scores, mapping=aes(x=horizon, y=cov95, group=model)) +
      geom_hline(aes(yintercept=0.50)) 
  }

  gg +
    geom_point(mapping=aes(col=model), alpha = 0.8) +
    geom_line(mapping=aes(col=model), alpha = 0.8) +
    scale_color_manual(breaks = model_names, values = model_colors) +
    labs(title=main, x="horizon", y=paste("average", y_var)) +
    theme_bw()
}


#' Plot summarized metrics against forecast_date
#'
#' @param summarized_scores A data frame of summarized scores. Must contain one row per model and horizon week combination plus a `horizon` column
#' @param model_names An ordered vector of model names
#' @param model_colors An ordered vector of model colors. Must match with `model_names` order
#' @param y_var A string specifying which metric to plot as the y-variable
#' @param horizon An integer specifying the horizon of the desired metric to plot
#' @param main A string specifying the plot title
#'
#' @return A scatter plot (with observations connected by lines) of the specified summary metric vs forecast date
#' @export
#'
#' @examples
plot_evaluated_scores_forecast_date <- function(summarized_scores, model_names, model_colors, y_var="wis", h=1, main, truth_data=NULL, truth_scaling=c(0.1, 0.15)) {

  data_to_plot <- summarized_scores |>
    dplyr::filter(horizon == h)
    
  if (!is.null(truth_data)) {
    date_range <- interval(min(data_to_plot$forecast_date), max(data_to_plot$forecast_date))
  
    truth_to_plot <- truth_data |>
      dplyr::mutate(target_end_date=target_end_date-days(5)) |>
      dplyr::filter(target_end_date %within% date_range) |>
      dplyr::group_by(model, target_end_date) |>
      dplyr::summarize(value = mean(value)) 
    
    truth_to_plot <- truth_to_plot |>
      dplyr::mutate(value=case_when(
        y_var == "wis" ~ value *truth_scaling[1], 
        y_var == "mae" ~ value*truth_scaling[2], 
        (y_var == "cov95" | y_var == "cov50") & target_end_date < as.Date("2022-08-01") ~ -0.15*value/max(truth_to_plot$value) + 1, 
        (y_var == "cov95" | y_var == "cov50") & target_end_date > as.Date("2022-08-01") ~ -0.5*value/max(truth_to_plot$value) + 1, 
        .default = value*truth_scaling[1]
      )) 
  }
  
  if (y_var == "wis") {
    gg <- ggplot(data_to_plot, mapping=aes(x=forecast_date, y=wis, group=model)) +
      coord_cartesian(ylim = c(0, sum(quantile(data_to_plot$wis, prob=c(0.25, 0.99)))))
  } else if (y_var == "mae") {
    gg <- ggplot(data_to_plot, mapping=aes(x=forecast_date, y=mae, group=model)) +
      coord_cartesian(ylim = c(0, sum(quantile(data_to_plot$mae, prob=c(0.25, 0.99)))))
  } else if (y_var == "cov95") {
    gg <- ggplot(data_to_plot, mapping=aes(x=forecast_date, y=cov95, group=model)) +
#      geom_jitter(height=0.05, width=0, mapping=aes(color=model)) +
      coord_cartesian(ylim = c(0, 1.05)) +
      geom_hline(aes(yintercept=0.95))
  } else if (y_var == "cov50") {
    gg <- ggplot(data_to_plot, mapping=aes(x=forecast_date, y=cov95, group=model)) +
#      geom_jitter(height=0.05, width=0, mapping=aes(color=model)) +
      coord_cartesian(ylim = c(0, 1.05)) +
      geom_hline(aes(yintercept=0.50))
  }
  
  if (!is.null(truth_data)) {
    gg +
      geom_point(truth_to_plot, mapping=aes(x=target_end_date, y=value, group=model), col="black", alpha = 0.8) +
      geom_line(truth_to_plot, mapping=aes(x=target_end_date, y=value, group=model), col="black", alpha = 0.8) +
      geom_point(mapping=aes(col=model)) +
      geom_line(mapping=aes(col=model)) +
      scale_x_date(name=NULL, date_breaks = "2 months", date_labels = "%b '%y") +
      scale_color_manual(breaks = model_names, values = model_colors) +
      labs(title=main, x="forecast date", y=paste("average", y_var)) +
      theme_bw()
    
  } else {
    gg +
      geom_point(mapping=aes(col=model), alpha = 0.8) +
      geom_line(mapping=aes(col=model), alpha = 0.8) +
      scale_x_date(name=NULL, date_breaks = "2 months", date_labels = "%b '%y") +
      scale_color_manual(breaks = model_names, values = model_colors) +
      labs(title=main, x="forecast date", y=paste("average", y_var)) +
      theme_bw()
  }
}



###################################################################################################
# Helper functions

# function for pairwise comparison of models
pairwise_comparison <-
  function(heat_scores, mx, my, subset = rep(TRUE, nrow(heat_scores)), permutation_test = FALSE){

  # apply subset:
  heat_scores <- heat_scores[subset, ]

  # subsets of available heat_scores for both models:
  subx <- subset(heat_scores, model == mx)
  suby <- subset(heat_scores, model == my)

  # merge together and restrict to overlap:
  sub <- merge(subx, suby, by = c("forecast_date", "location", "horizon"), all.x = FALSE, all.y = FALSE)

  # compute ratio:
  ratio <- sum(sub$wis.x) / sum(sub$wis.y)

  # perform permutation tests:
  if(permutation_test){
    pval <- permutationTest(sub$wis.x, sub$wis.y,
                            nPermutation = 999)$pVal.permut

    # aggregate by forecast date:
    sub_fcd <- aggregate(cbind(wis.x, wis.y) ~ forecast_date, data = sub, FUN = mean)
    pval_fcd <- permutationTest(sub_fcd$wis.x, sub_fcd$wis.y, nPermutation = 999)$pVal.permut
  } else {
    pval <- NULL
    pval_fcd <- NULL
  }

  return(list(ratio = ratio, pval = pval, pval_fcd = pval_fcd, mx = mx, my = my))
}


###################################################################################################
# Calculate and plot pairwise WIS

#' Calculate and plot pairwise WIS by location
#'
#' @param scores A data frame of forecast scores to plot
#' @param truth A data frame of truth data
#' @param model_levels Ordered vector of model names used as axis levels
#' @param baseline_name Name of baseline model (must be included in model_levels)
#'
#' @return A tile plot of pairwise relative WIS broken down by location and model using the scores provided
#' @export
#'
#' @examples
plot_wis_loc <- function(scores, truth, model_levels, baseline_name) { # potentially add choice of wis, mae, coverage?
  #reorder states, reorder models
  data("hub_locations")

  inc_scores <- scores %>%
    filter(!(location %in% c("22", "US") & forecast_date <= as.Date("2021-01-04")))

  # bring all forecast_dates to Monday:
#  inc_scores$forecast_date <- next_monday(inc_scores$forecast_date)

  # select relevant columns:
  heat_scores <- inc_scores %>%
    left_join(covidHubUtils::hub_locations_flusight, by = c("location" = "fips")) %>%
    select("model", "forecast_date", "location", "location_name", "horizon", "abs_error", "wis", "horizon") %>%
    droplevels()

  # the included models and locations:
  models <- unique(heat_scores$model)
  locations <- unique(heat_scores$location)
  location_names <- unique(heat_scores$location_name)

  # compute pairwise and relative WIS for each location separately:
  for(i in seq_along(locations)){

    # select location:
    loc <- locations[i]
    loc_name <- location_names[i]

    # matrix to store:
    results_ratio_temp <- matrix(ncol = length(models),
                                 nrow = length(models),
                                 dimnames = list(models, models))

    # run pairwise comparison for chosen location:
    for(mx in seq_along(models)){
      for(my in 1:mx){
        pwc <- pairwise_comparison(
          heat_scores = filter(heat_scores, horizon %in% 1:4), mx = models[mx], my = models[my],
          permutation_test = FALSE, # disable permutation test to speed up things
          subset = filter(heat_scores, horizon %in% 1:4)$location == loc # this will subset to the respective location inside the function
        )
        results_ratio_temp[mx, my] <- pwc$ratio
        results_ratio_temp[my, mx] <- 1/pwc$ratio
      }
    }

    # compute the geometric means etc
    ind_baseline <- which(rownames(results_ratio_temp) == baseline_name)
    geom_mean_ratios_temp <- exp(rowMeans(log(results_ratio_temp[, -ind_baseline]), na.rm = TRUE))
    ratios_baseline_temp <- results_ratio_temp[, baseline_name]
    ratios_baseline2_temp <- geom_mean_ratios_temp/geom_mean_ratios_temp[baseline_name]

    # summarize results:
    to_add <- data.frame(model = names(ratios_baseline2_temp),
                         location = loc,
                         location_name = loc_name,
                         relative_wis = ratios_baseline2_temp,
                         log_relative_wis = log(ratios_baseline2_temp))

    # append to already stored:
    if (i == 1) { # initialize at first location
      average_by_loc <- to_add
    } else {
      average_by_loc <- rbind(average_by_loc, to_add)
    }
  }

  ## plot of true data by state, tiled
  truth_dat <- truth %>%
    left_join(covidHubUtils::hub_locations_flusight, by = c("location" = "fips")) %>%
    group_by(location, location_name) %>%
    summarize(cum_value=sum(value)) %>%
    ungroup() %>%
    mutate(location_name = reorder(location_name, cum_value)) %>%
    pull(location_name)

  average_by_loc_to_plot <- average_by_loc %>%
    mutate(location_name = fct_relevel(location_name, levels(truth_dat)),
           relative_wis_text = sprintf("%.2f", round(relative_wis, 2)),
           log_relative_wis = log2(relative_wis),
           model = fct_relevel(model, model_levels)) %>%
    filter(!is.na(relative_wis))

  # plot:
  fig_wis_loc <- average_by_loc_to_plot %>%
    ggplot(aes(x=model, y=location_name,
               fill= scales::oob_squish(log_relative_wis, range = c(-2, 1.5)))) +
    geom_tile() +
    geom_text(aes(label = relative_wis_text), size = 2.5) + # I adapted the rounding
    scale_fill_gradient2(
      low = "steelblue",
      high = "red",
      midpoint = 0,
      na.value = "grey50",
      name = "Relative WIS",
      breaks = c(-2,-1,0,1),
      labels =c("0.25", 0.5, 1, "2+")
    ) +
    xlab(NULL) + ylab(NULL) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
#     color=
#        ifelse(levels(average_by_loc_to_plot$model) %in% models_to_highlight, "red", "black")),
      axis.title.x = element_text(size = 9),
      axis.text.y = element_text(size = 9),
      title = element_text(size = 9)
    ) +
    theme_bw()

  print(fig_wis_loc)
  # return(fig_wis_loc)
}

