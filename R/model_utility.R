


calc_utility_impl <- function(
  d,
  model_name,
  outcome_name,
  ...,
  outcome_target = TRUE,
  true_positive_value_column_name = 'true_positive_value',
  false_positive_value_column_name = 'false_positive_value',
  true_negative_value_column_name = 'true_negative_value',
  false_negative_value_column_name = 'false_negative_value') {
  wrapr::stop_if_dot_args(substitute(list(...)), "sigr:::calc_utility_impl")
  # narrow frame
  d <- d[ , c(model_name,
              outcome_name,
              true_positive_value_column_name,
              false_positive_value_column_name,
              true_negative_value_column_name,
              false_negative_value_column_name),
          drop = FALSE]
  # make sure outcome is TRUE/FALSE
  d[[outcome_name]] <- d[[outcome_name]] == outcome_target
  # simplify to impossible cases cost zero
  d[[true_positive_value_column_name]][!d[[outcome_name]]] <- 0
  d[[false_positive_value_column_name]][d[[outcome_name]]] <- 0
  d[[true_negative_value_column_name]][d[[outcome_name]]] <- 0
  d[[false_negative_value_column_name]][!d[[outcome_name]]] <- 0
  model_order <- order(d[[model_name]], decreasing = TRUE)
  d <- d[model_order, ]
  rownames(d) <- NULL
  # get basic cumulative statistics
  result <- data.frame(
    model = model_name,
    threshold = d[[model_name]],
    count_taken = seq_len(nrow(d)),
    fraction_taken = 0,
    true_positive_value = cumsum(d[[true_positive_value_column_name]]),
    false_positive_value = cumsum(d[[false_positive_value_column_name]]),
    true_negative_value_cumsum = cumsum(d[[true_negative_value_column_name]]),
    false_negative_value_cumsum = cumsum(d[[false_negative_value_column_name]])
  )
  result$fraction_taken <- result$count_taken/max(result$count_taken)
  # remove inaccessible rows
  burried_rows <- result$threshold[-1] >= result$threshold[-nrow(result)]
  if(any(burried_rows)) {
    result <- result[!c(burried_rows, FALSE), ]
    rownames(result) <- NULL
  }
  if(length(result$threshold) > 1) {
    # move thresholds to midpoints (except end)
    result$threshold <- c((result$threshold[-1] + result$threshold[-nrow(result)])/2,
                          result$threshold[[length(result$threshold)]])
    # resort to ascending threshold order
    result <- result[order(result$threshold), ]
    rownames(result) <- NULL
  }
  # add in ideal "nothing selected" row
  result <- rbind(
    result,
    data.frame(
      model = model_name,
      threshold = NA_real_,  # a large number would do here
      count_taken = 0,
      fraction_taken = 0.0,
      true_positive_value = 0,
      false_positive_value = 0,
      true_negative_value_cumsum = 0,
      false_negative_value_cumsum = 0
    ))
  rownames(result) <- NULL
  # add derived columns and clean up
  result$true_negative_value <- sum(d[[true_negative_value_column_name]]) - result$true_negative_value_cumsum
  result$false_negative_value <- sum(d[[false_negative_value_column_name]]) - result$false_negative_value_cumsum
  result$true_negative_value_cumsum <- NULL
  result$false_negative_value_cumsum <- NULL
  result$total_value = result$true_positive_value + result$false_positive_value +
    result$true_negative_value + result$false_negative_value
  result
}


#' Estimate model utility
#'
#' Compute the utility of a model score on a classification data set. For each
#' threshold of interest we compute the utility of the classification rule of taking all items
#' with model score greater than or equal to the threshold. The user
#' specifies the outcome (a binary classification target), a model
#' score (numeric), and the utility values (positive, negative, or zero) of
#' each case: true positives, false positives, true negatives, and
#' false negatives. What is returned is a table of model thresholds
#' and the total value of using this model score plus the given threshold
#' as a classification rule. NA is used to mark a threshold where no rows
#' are selected.
#'
#' A worked example can be found here: \url{https://github.com/WinVector/sigr/blob/main/extras/UtilityExample.md}.
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
#'   true_negative_value = 0.001,  # true negatives have no value in our application
#'                                 # but just give ourselves a small reward for being right
#'   false_negative_value = -0.01  # adding a small notional tax for false negatives,
#'                                 # don't want our competitor getting these accounts.
#'   )
#'
#' values <- model_utility(d, 'predicted_probability', 'made_purchase')
#' best_strategy <- values[values$total_value >= max(values$total_value), ][1, ]
#' t(best_strategy)
#'
#'
#'
#'# a bigger example
#'
#' d <- data.frame(
#'   predicted_probability = stats::runif(100),
#'   made_purchase = sample(c(FALSE, TRUE), replace = TRUE, size = 100),
#'   false_positive_value = -5,    # acting on any predicted positive costs $5
#'   true_positive_value = 95,     # revenue on a true positive is $100 minus action cost
#'   true_negative_value = 0.001,  # true negatives have no value in our application
#'                                 # but just give ourselves a small reward for being right
#'   false_negative_value = -0.01  # adding a small notional tax for false negatives,
#'   # don't want our competitor getting these accounts.
#' )
#'
#' values <- model_utility(d, 'predicted_probability', 'made_purchase')
#'
#' # plot the estimated total utility as a function of threshold
#' plot(values$threshold, values$total_value)
#'
#' best_strategy <- values[values$total_value >= max(values$total_value), ][1, ]
#' t(best_strategy)
#'
#'
#' # without utilities example
#'
#' d <- data.frame(
#'   predicted_probability = c(0, 0.5, 0.5, 0.5),
#'   made_purchase = c(FALSE, TRUE, FALSE, FALSE))
#' model_utility(d, 'predicted_probability', 'made_purchase')
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
  have_value_columns <-
    (!is.null(true_positive_value_column_name)) &&
    (!is.null(false_positive_value_column_name)) &&
    (!is.null(true_negative_value_column_name)) &&
    (!is.null(false_negative_value_column_name)) &&
    (length(setdiff(
      c(true_positive_value_column_name, false_positive_value_column_name,
        true_negative_value_column_name, false_negative_value_column_name),
      colnames(d))) == 0)
  # narrow frame
  if(have_value_columns) {
    d <- d[ , c(model_name,
                outcome_name,
                true_positive_value_column_name,
                false_positive_value_column_name,
                true_negative_value_column_name,
                false_negative_value_column_name),
            drop = FALSE]
  } else {
    d <- d[ , c(model_name,
                outcome_name),
            drop = FALSE]
  }
  # get complete counts
  d_count <- d
  d_count[[true_positive_value_column_name]] <- 1
  d_count[[false_positive_value_column_name]] <- 1
  d_count[[true_negative_value_column_name]] <- 1
  d_count[[false_negative_value_column_name]] <- 1
  result_counts <- calc_utility_impl(
    d = d_count,
    model_name = model_name,
    outcome_name = outcome_name,
    outcome_target = outcome_target,
    true_positive_value_column_name = true_positive_value_column_name,
    false_positive_value_column_name = false_positive_value_column_name,
    true_negative_value_column_name = true_negative_value_column_name,
    false_negative_value_column_name = false_negative_value_column_name)
  result_counts$true_negative_count <- result_counts$true_negative_value
  result_counts$false_negative_count <- result_counts$false_negative_value
  result_counts$true_positive_count <- result_counts$true_positive_value
  result_counts$false_positive_count <- result_counts$false_positive_value
  result_counts$true_positive_value <- NULL
  result_counts$false_positive_value <- NULL
  result_counts$true_negative_value <- NULL
  result_counts$false_negative_value <- NULL
  result_counts$total_value <- NULL
  result <- result_counts
  if(have_value_columns) {
    # get user specified utilities
    result_utility <- calc_utility_impl(
      d = d,
      model_name = model_name,
      outcome_name = outcome_name,
      outcome_target = outcome_target,
      true_positive_value_column_name = true_positive_value_column_name,
      false_positive_value_column_name = false_positive_value_column_name,
      true_negative_value_column_name = true_negative_value_column_name,
      false_negative_value_column_name = false_negative_value_column_name)
    # combine and return results
    result <- cbind(
      result_utility,
      result_counts[ ,
                     c('true_negative_count', 'false_negative_count', 'true_positive_count', 'false_positive_count')]
    )
  }
  result
}





#' Check a few things we expect to be true for the utility frame.
#'
#' Utility to inspect a utility frame for some debugging.
#'
#' @param values model_utility result
#' @param ... Not used, forces later argument to be specified by name.
#' @param constant_utilities logical, if TRUE assume utilities were constant per-row.
#' @return NULL if okay, else a string describing the problem.
#'
#' @export
#'
#' @examples
#'
#' d <- data.frame(
#'   predicted_probability = c(0, 0.5, 0.5, 0.5),
#'   made_purchase = c(FALSE, TRUE, FALSE, FALSE),
#'   false_positive_value = -5,    # acting on any predicted positive costs $5
#'   true_positive_value = 95,     # revenue on a true positive is $100 minus action cost
#'   true_negative_value = 0.001,  # true negatives have no value in our application
#'                                 # but just give ourselves a small reward for being right
#'   false_negative_value = -0.01  # adding a small notional tax for false negatives,
#'                                 # don't want our competitor getting these accounts.
#'   )
#'
#' values <- model_utility(d, 'predicted_probability', 'made_purchase')
#' check_utility_calc(values,
#'                    orig_score = d$predicted_probability,
#'                    orig_outcome = d$made_purchase)
#'
#' @keywords internal
check_utility_calc <- function(values,
                               ...,
                               orig_score = NULL, orig_outcome = NULL,
                               constant_utilities = FALSE) {
  n <- nrow(values)
  if(n < 2) {
    return("too few rows")
  }
  if(!is.na(values$threshold[[n]])) {
    return("pseudo-observation wasn't at the end")
  }
  if(any(is.na(values$threshold[-n]))) {
    return("more than one NA threshold")
  }
  if(!isTRUE(all(values$threshold[-c(n-1, n)] < values$threshold[-c(1, n)]))) {
    return("non-NA threshold are not strictly increasing")
  }
  count_sum <- values$true_negative_count + values$false_negative_count +
    values$true_positive_count + values$false_positive_count
  if(length(unique(count_sum)) != 1) {
    return("counts don't total to a constant")
  }
  if(count_sum[[1]] != max(values$count_taken)) {
    return("counts total doesn't match max taken")
  }
  if(length(unique(values$model)) != 1) {
    return("model name is not a constant")
  }
  # check columns for not-NA
  for(col in c('model', 'count_taken', 'fraction_taken',
               'true_positive_value', 'false_positive_value',
               'true_negative_value', 'false_negative_value',
               'true_negative_count', 'false_negative_count',
               'true_positive_count', 'false_positive_count')) {
    if(isTRUE(any(is.na(values[[col]])))) {
      return(paste0('column ', col, ' had a NA value'))
    }
  }
  # check columns for non-negativity
  for(col in c('count_taken', 'fraction_taken',
               'true_negative_count', 'false_negative_count',
               'true_positive_count', 'false_positive_count')) {
    if(!isTRUE(all(values[[col]] >= 0))) {
      return(paste0('column ', col, ' had a negative value'))
    }
  }
  # check columns are strictly decreasing
  for(col in c('count_taken', 'fraction_taken')) {
    if(!isTRUE(all(values[[col]][-n] > values[[col]][-1]))) {
      return(paste0("column ", col, " is not strictly decreasing"))
    }
  }
  # check columns are non-increasing
  for(col in c('true_positive_count', 'false_positive_count')) {
    if(!isTRUE(all(values[[col]][-n] >= values[[col]][-1]))) {
      return(paste0("column ", col, " is not strictly decreasing"))
    }
  }
  # check columns are non-decreasing
  for(col in c('true_negative_count', 'false_negative_count')) {
    if(!isTRUE(all(values[[col]][-n] <= values[[col]][-1]))) {
      return(paste0("column ", col, " is not strictly decreasing"))
    }
  }
  # check columns are monotone
  for(col in c('true_positive_value', 'false_positive_value',
               'true_negative_value', 'false_negative_value')) {
    if((!isTRUE(all(values[[col]][-n] <= values[[col]][-1]))) &&
       (!isTRUE(all(values[[col]][-n] >= values[[col]][-1])))) {
      return(paste0("column ", col, " is not monotone"))
    }
  }
  # check columns start at zero
  for(col in c('true_negative_value', 'false_negative_value',
               'true_negative_count', 'false_negative_count')) {
    if(values[[col]][[1]] != 0) {
      return(paste0("column ", col, " didn't start at zero"))
    }
  }
  # check columns end at zero
  for(col in c('count_taken', 'fraction_taken',
               'true_positive_value', 'false_positive_value',
               'true_positive_count', 'false_positive_count')) {
    if(values[[col]][[n]] != 0) {
      return(paste0("column ", col, " didn't end at zero"))
    }
  }
  if(values$fraction_taken[[1]] != 1.0) {
    return("fraction taken didn't start at 1")
  }
  if(values$fraction_taken[[n]] != 0.0) {
    return("fraction taken didn't end at 1")
  }
  sum_negative <- values$true_negative_count + values$false_positive_count
  if(length(unique(sum_negative)) != 1) {
    return("true_negative_count + false_positive_count didn't sum to a constant")
  }
  sum_positive <- values$true_positive_count + values$false_negative_count
  if(length(unique(sum_positive)) != 1) {
    return("true_positive_count + false_negative_count didn't sum to a constant")
  }
  if(constant_utilities) {
    # check utilities track counts
    for(pair in list(
      c('true_positive_count', 'true_positive_value'),
      c('false_positive_count', 'false_positive_value'),
      c('true_negative_count', 'true_negative_value'),
      c('false_negative_count', 'false_negative_value')
    )) {
      count_col <- pair[[1]]
      value_col <- pair[[2]]
      if(!isTRUE(all(values[[value_col]] == 0))) {
        if(any((values[[count_col]] == 0) & (values[[value_col]] != 0))) {
          return(paste0("value col is non-zero where count col is zero: ", count_col))
        }
        ratios <- unique((values[[value_col]] / values[[count_col]])[values[[count_col]] != 0])
        if(length(ratios) > 1) {
          typical <- mean(ratios)
          if(max(abs(ratios - typical)) > 1.0e-3) {
            return(paste0("value col is not a constant ratio of count col: ", count_col))
          }
        }
      }
    }
  }
  if(!is.null(orig_score)) {
    # calculate the counts for a threshold
    # slow, used to confirm cumsum calculation
    get_counts = function(threshold, score, cls) {
      data.frame(threshold = threshold,
                 count_taken = sum(score >= threshold),
                 true_negative_count = sum((score < threshold) & (cls==FALSE)),
                 false_negative_count = sum((score < threshold) & (cls==TRUE)),
                 true_positive_count = sum((score >= threshold) & (cls==TRUE)),
                 false_positive_count = sum((score >= threshold) & (cls==FALSE)))
    }

    get_all_counts = function(scores, cls) {
      thresholds <- sort(unique(scores[!is.na(scores)]))
      thresholds <- c(thresholds, max(thresholds) + 1)
      flist = lapply(thresholds, function(th) get_counts(th, scores, cls))
      do.call(rbind, flist)
    }

    # now check against a slower, simpler to document calculation
    check <- get_all_counts(orig_score, orig_outcome)
    check_cols <- c("true_negative_count", "false_negative_count", "true_positive_count", "false_positive_count")
    wrapr::check_equiv_frames(values[,  check_cols], check[, check_cols])
  }
  NULL  # good
}
