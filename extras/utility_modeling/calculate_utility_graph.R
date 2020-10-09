#
# Runs a bootstrap estimation procedure to estimate uncertainty
# bounds on a given utility curve.
#
# Libraries used: sigr, boot, cdata, rquery/rqdatatable, wrapr
#
# Inputs:
# d: data frame of predictions and outcomes
# prediction_column_name: name of predictions column
# outcome_column_name: name of outcome column
# true_positive_value : reward for a true positive (scalar)
# false_positive_value: penalty for a false positive (scalar)
# true_negative_value: reward for a true negative (scalar)
# false_negative_value: penalty for a false negative (scalar)
#
# Returns a list:
# plot_thin: data frame in thin form of total value (utility) curve,
#            mean curve of bootstrap estimates, and curve from a parametric estimate,
#            all as functions of threshold
# boot_stats: frame of mean and median curves of bootstrap estimates, along with
#             the boundary curves of the 95% and 50% quantiles, in wide form
#
estimate_utility_graph <- function(
  d,
  ...,
  prediction_column_name,
  outcome_column_name,
  true_positive_value,
  false_positive_value,
  true_negative_value,
  false_negative_value) {
  wrapr::stop_if_dot_args(substitute(list(...)), "estimate_utility_graph")

  # set costs
  d$true_positive_value <- true_positive_value
  d$false_positive_value <- false_positive_value
  d$true_negative_value <- true_negative_value
  d$false_negative_value <- false_negative_value

  # calculate untility curve (actual values)
  values <- model_utility(d, prediction_column_name, outcome_column_name)

  # get the thresholds
  threshold_list <- values$threshold[(!is.na(values$threshold))]

  # calculates the utility curve from a bootstrap sample (described by indices)
  f <- function(d, indices, ...) {
    vi <- model_utility(d[indices, ], prediction_column_name, outcome_column_name)
    fn <- approxfun(vi$threshold, vi$total_value,
                    yleft = min(vi$total_value),
                    yright = max(vi$total_value))
    fn(threshold_list)
  }

  # run the bootstrap
  boot_stats <- boot(data = d, statistic = f, R = 1000,
                     parallel = 'multicore', ncpus = parallel::detectCores())
  boot_data <- as.data.frame(boot_stats$t)
  # each column corresponds to a threshold
  colnames(boot_data) <- threshold_list

  # put it into long form
  boot_data <- pivot_to_blocks(boot_data,
                               nameForNewKeyColumn = 'threshold',
                               nameForNewValueColumn = 'total_value',
                               columnsToTakeFrom = colnames(boot_data))
  # turn thresholds back into numbers
  boot_data$threshold <- as.numeric(boot_data$threshold)

  # functions to calculate quantiles
  q_0.025 <- function(x) { quantile(x, probs = 0.025) }
  q_0.25 <- function(x) { quantile(x, probs = 0.25) }
  q_0.50 <- function(x) { quantile(x, probs = 0.50) }
  q_0.75 <- function(x) { quantile(x, probs = 0.75) }
  q_0.975 <- function(x) { quantile(x, probs = 0.975) }

  # create summary frame
  boot_summary <- project(boot_data,
                          mean_total_value = mean(total_value),
                          q_0.025 = q_0.025(total_value),
                          q_0.25 = q_0.25(total_value),
                          q_0.50 = q_0.50(total_value),
                          q_0.75 = q_0.75(total_value),
                          q_0.975 = q_0.975(total_value),
                          groupby = 'threshold') %.>%
    orderby(., 'threshold')

  value_range <- values[
    (values$threshold >= min(threshold_list)) &
      (values$threshold <= max(threshold_list)), ]

  # get the actual utility curve and the bootstrap mean curves
  # (2 frames)
  boot_thin <- boot_summary %.>%
    select_columns(., qc(threshold, mean_total_value)) %.>%
    rename_columns(., 'total_value' := 'mean_total_value') %.>%
    extend(., estimate = 'bootstrapped value')

  value_thin <- value_range %.>%
    select_columns(., qc(threshold, total_value)) %.>%
    extend(., estimate = 'estimated value')
  value_thin <- value_thin[complete.cases(value_thin), , drop = FALSE]


  # estimate a parametric value curve
  # try to recover per-class beta distribution parameters
  unpack[shape1_pos, shape2_pos, shape1_neg, shape2_neg] <-
    sigr::find_ROC_matching_ab(modelPredictions = d[[prediction_column_name]],
                               yValues = d[[outcome_column_name]])
  total_pos <- sum(d[[outcome_column_name]])
  total_neg <- sum(!d[[outcome_column_name]])
  # generate the required tails
  theoretical_values <- data.frame(
    estimate = 'parametric fit',
    threshold = values$threshold)
  pos_take_count <- pbeta(theoretical_values$threshold, shape1 = shape1_pos, shape2 = shape2_pos, lower.tail = FALSE) * total_pos
  pos_take_count[is.na(pos_take_count)] <- 0
  neg_take_count <- pbeta(theoretical_values$threshold, shape1 = shape1_neg, shape2 = shape2_neg, lower.tail = FALSE) * total_neg
  neg_take_count[is.na(neg_take_count)] <- 0
  theoretical_values$true_negative_count <- total_neg - neg_take_count
  theoretical_values$false_negative_count <- total_pos - pos_take_count
  theoretical_values$true_positive_count <- pos_take_count
  theoretical_values$false_positive_count <- neg_take_count
  theoretical_values$total_value <-
    theoretical_values$true_negative_count * true_negative_value +
    theoretical_values$false_negative_count * false_negative_value +
    theoretical_values$true_positive_count * true_positive_value +
    theoretical_values$false_positive_count * false_positive_value


  plot_thin <- rbind(boot_thin, value_thin, theoretical_values[, qc(threshold, total_value, estimate)])
  plot_thin <- plot_thin[
    (complete.cases(plot_thin)) &
      (plot_thin$threshold >= min(threshold_list)) &
      (plot_thin$threshold <= max(threshold_list)), ]

  list(plot_thin = plot_thin, boot_summary = boot_summary)
}
