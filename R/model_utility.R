
# TODO: confirm calculation

#' Estimate model utility
#'
#' Compute the model utility on a data set for taking all items
#' greater than or equal to each model-score threshold. The user
#' specifies the outcome (a binary classification target), a model
#' score (numeric), and the values (positive, negative, or zero) of
#' each case: true positives, false positives, true negatives, and
#' false negatives. What is returned is a table of model thresholds
#' and the total value of using this model score plus the given threshold
#' as a classification rule. NA is used to mark a threshold where no rows
#' are selected.
#'
#' @param d A data.frame containing all data and outcome values.
#' @param model_name Name of the column containing model predictions.
#' @param outcome_name Name of the column containing the truth values.
#' @param ... Not used, forces later argument to be specified by name.
#' @param outcome_target truth value considered to be TRUE.
#' @param true_positive_value_column_name column name of per-row values of true positive cases. Only used on positive instances.
#' @param false_positive_value_column_name column name of per-row values of false positive cases. Only used on negative instances.
#' @param true_negative_value_column_name column name of per-row values of true negative cases. Only used on negative instances.
#' @param false_negative_value_column_name column name of per-row values of false negative cases. Only used on positive instances.
#' @return data.frame of all threshold values.
#'
#' @examples
#'
#' d <- data.frame(
#'   predicted_probability = c(0, 0.5, 0.5, 0.5),
#'   made_purchase = c(FALSE, TRUE, FALSE, FALSE),
#'   false_positive_value = -5,    # acting on any predicted positive costs $5
#'   true_positive_value = 95,     # revenue on a true positive is $100 minus action cost
#'   true_negative_value = 0,      # true negatives have no value in our application
#'   false_negative_value = -0.01  # adding a small notional tax for false negatives,
#'                                 # don't want our competitor getting these accounts.
#'   )
#'
#' values <- model_utility(d, 'predicted_probability', 'made_purchase')
#' best_strategy <- values[values$total_value >= max(values$total_value), ][1, ]
#' t(best_strategy)
#'
#' @export
#'
model_utility <- function(
  d,
  model_name,
  outcome_name,
  ...,
  outcome_target = TRUE,
  true_positive_value_column_name = 'true_positive_value',
  false_positive_value_column_name = 'false_positive_value',
  true_negative_value_column_name = 'true_negative_value',
  false_negative_value_column_name = 'false_negative_value') {
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr::model_utility")
  d <- d[ , c(model_name,
              outcome_name,
              true_positive_value_column_name,
              false_positive_value_column_name,
              true_negative_value_column_name,
              false_negative_value_column_name),
          drop = FALSE]
  d[[outcome_name]] <- d[[outcome_name]] == outcome_target
  model_order <- order(d[[model_name]], decreasing = TRUE)
  d <- d[model_order, ]
  rownames(d) <- NULL
  # get basic cumulative statistics
  result <- data.frame(
    threshold = d[[model_name]],
    count_taken = seq_len(nrow(d)),
    true_positive_count = cumsum(d[[outcome_name]]),
    false_positive_count = cumsum(!d[[outcome_name]]),
    true_positive_value = cumsum(ifelse(d[[outcome_name]], d[[true_positive_value_column_name]], 0)),
    false_positive_value = cumsum(ifelse(d[[outcome_name]], 0, d[[false_positive_value_column_name]])),
    true_negative_value_cumsum = cumsum(ifelse(d[[outcome_name]], 0, d[[true_negative_value_column_name]])),
    false_negative_value_cumsum = cumsum(ifelse(d[[outcome_name]], d[[false_negative_value_column_name]], 0)),
    model = model_name
  )
  # remove inaccessible rows
  burried_rows <- result$threshold[-1] >= result$threshold[-nrow(result)]
  if(any(burried_rows)) {
    result <- result[!c(burried_rows, FALSE), ]
    rownames(result) <- NULL
  }
  # add in ideal "nothing selected" row
  result <- rbind(
    data.frame(
      threshold = NA_real_,  # a large number would do here
      count_taken = 0,
      true_positive_count = 0,
      false_positive_count = 0,
      true_positive_value = 0,
      false_positive_value = 0,
      true_negative_value_cumsum = 0,
      false_negative_value_cumsum = 0,
      model = model_name
    ),
    result)
  rownames(result) <- NULL
  # add derived columns and clean up
  result$true_negative_count <- sum(!d[[outcome_name]]) - result$false_positive_count
  result$false_negative_count <- sum(d[[outcome_name]]) - result$true_positive_count
  result$true_negative_value <- sum(d[[true_negative_value_column_name]]) - result$true_negative_value_cumsum
  result$false_negative_value <- sum(d[[false_negative_value_column_name]]) - result$false_negative_value_cumsum
  result$true_negative_value_cumsum <- NULL
  result$false_negative_value_cumsum <- NULL
  result$total_value = result$true_positive_value + result$false_positive_value +
    result$true_negative_value + result$false_negative_value
  result
}
