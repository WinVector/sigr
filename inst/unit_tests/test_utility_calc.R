
test_utility_calc_1 <- function() {
  d <- data.frame(
    predicted_probability = c(0, 0.5, 0.5, 0.5),
    made_purchase = c(FALSE, TRUE, FALSE, FALSE),
    false_positive_value = -5,    # acting on any predicted positive costs $5
    true_positive_value = 95,     # revenue on a true positive is $100 minus action cost
    true_negative_value = 0.001,  # true negatives have no value in our application
    # but just give ourselves a small reward for being right
    false_negative_value = -0.01  # adding a small notional tax for false negatives,
    # don't want our competitor getting these accounts.
  )

  values <- model_utility(d, 'predicted_probability', 'made_purchase')
  RUnit::checkTrue(is.null(check_utility_calc(values, constant_utilities = TRUE,
                                              orig_score = d$predicted_probability,
                                              orig_outcome = d$made_purchase)))

  # cat(wrapr::draw_frame(values))

  expect <- wrapr::build_frame(
    "model"                  , "threshold", "count_taken", "fraction_taken", "true_positive_value", "false_positive_value", "true_negative_value", "false_negative_value", "total_value", "true_negative_count", "false_negative_count", "true_positive_count", "false_positive_count" |
      "predicted_probability", 0          , 4            , 1               , 95                   , -15                   , 0                    , 0                     , 80           , 0                    , 0                     , 1                    , 3                      |
      "predicted_probability", 0.25       , 3            , 0.75            , 95                   , -10                   , 0.001                , 0                     , 85           , 1                    , 0                     , 1                    , 2                      |
      "predicted_probability", NA_real_   , 0            , 0               , 0                    , 0                     , 0.003                , -0.01                 , -0.007       , 3                    , 1                     , 0                    , 0                      )

  wrapr::check_equiv_frames(values, expect, tolerance = 1.e-3)

  invisible(NULL)
}

test_utility_calc_1b <- function() {
  d <- data.frame(
    predicted_probability = c(0, 0, 0, 0.5),
    made_purchase = c(FALSE, TRUE, FALSE, FALSE),
    false_positive_value = -5,    # acting on any predicted positive costs $5
    true_positive_value = 95,     # revenue on a true positive is $100 minus action cost
    true_negative_value = 0.001,  # true negatives have no value in our application
    # but just give ourselves a small reward for being right
    false_negative_value = -0.01  # adding a small notional tax for false negatives,
    # don't want our competitor getting these accounts.
  )

  values <- model_utility(d, 'predicted_probability', 'made_purchase')
  RUnit::checkTrue(is.null(check_utility_calc(values, constant_utilities = TRUE,
                                              orig_score = d$predicted_probability,
                                              orig_outcome = d$made_purchase)))

  # cat(wrapr::draw_frame(values))

  expect <- wrapr::build_frame(
    "model"                  , "threshold", "count_taken", "fraction_taken", "true_positive_value", "false_positive_value", "true_negative_value", "false_negative_value", "total_value", "true_negative_count", "false_negative_count", "true_positive_count", "false_positive_count" |
      "predicted_probability", 0          , 4            , 1               , 95                   , -15                   , 0                    , 0                     , 80           , 0                    , 0                     , 1                    , 3                      |
      "predicted_probability", 0.25       , 1            , 0.25            , 0                    , -5                    , 0.002                , -0.01                 , -5.008       , 2                    , 1                     , 0                    , 1                      |
      "predicted_probability", NA_real_   , 0            , 0               , 0                    , 0                     , 0.003                , -0.01                 , -0.007       , 3                    , 1                     , 0                    , 0                      )

  wrapr::check_equiv_frames(values, expect, tolerance = 1.e-3)

  invisible(NULL)
}


test_utility_calc_2 <- function() {
  d <- data.frame(
    predicted_probability = c(0, 0.25, 0.5, 0.5, 0.5, 0.75, 0.8, 1.0),
    made_purchase = c(FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE),
    false_positive_value = -5,    # acting on any predicted positive costs $5
    true_positive_value = 95,     # revenue on a true positive is $100 minus action cost
    true_negative_value = 0.001,  # true negatives have no value in our application
    # but just give ourselves a small reward for being right
    false_negative_value = -0.01  # adding a small notional tax for false negatives,
    # don't want our competitor getting these accounts.
  )

  values <- model_utility(d, 'predicted_probability', 'made_purchase')
  RUnit::checkTrue(is.null(check_utility_calc(values, constant_utilities = TRUE,
                                              orig_score = d$predicted_probability,
                                              orig_outcome = d$made_purchase)))

  # cat(wrapr::draw_frame(values))

  expect <- wrapr::build_frame(
    "model"                  , "threshold", "count_taken", "fraction_taken", "true_positive_value", "false_positive_value", "true_negative_value", "false_negative_value", "total_value", "true_negative_count", "false_negative_count", "true_positive_count", "false_positive_count" |
      "predicted_probability", 0          , 8            , 1               , 380                  , -20                   , 0                    , 0                     , 360          , 0                    , 0                     , 4                    , 4                      |
      "predicted_probability", 0.125      , 7            , 0.875           , 380                  , -15                   , 0.001                , 0                     , 365          , 1                    , 0                     , 4                    , 3                      |
      "predicted_probability", 0.375      , 6            , 0.75            , 285                  , -15                   , 0.001                , -0.01                 , 270          , 1                    , 1                     , 3                    , 3                      |
      "predicted_probability", 0.625      , 3            , 0.375           , 190                  , -5                    , 0.003                , -0.02                 , 185          , 3                    , 2                     , 2                    , 1                      |
      "predicted_probability", 0.775      , 2            , 0.25            , 95                   , -5                    , 0.003                , -0.03                 , 89.97        , 3                    , 3                     , 1                    , 1                      |
      "predicted_probability", 0.9        , 1            , 0.125           , 95                   , 0                     , 0.004                , -0.03                 , 94.97        , 4                    , 3                     , 1                    , 0                      |
      "predicted_probability", NA_real_   , 0            , 0               , 0                    , 0                     , 0.004                , -0.04                 , -0.036       , 4                    , 4                     , 0                    , 0                      )
  wrapr::check_equiv_frames(values, expect, tolerance = 1.e-3)

  invisible(NULL)
}


test_utility_calc_3 <- function() {
  d <- data.frame(
    predicted_probability = runif(100),
    made_purchase = sample(c(FALSE, TRUE), replace = TRUE, size = 100),
    false_positive_value = -5,    # acting on any predicted positive costs $5
    true_positive_value = 95,     # revenue on a true positive is $100 minus action cost
    true_negative_value = 0.001,  # true negatives have no value in our application
    # but just give ourselves a small reward for being right
    false_negative_value = -0.01  # adding a small notional tax for false negatives,
    # don't want our competitor getting these accounts.
  )

  values <- model_utility(d, 'predicted_probability', 'made_purchase')
  RUnit::checkTrue(is.null(check_utility_calc(values, constant_utilities = TRUE,
                                              orig_score = d$predicted_probability,
                                              orig_outcome = d$made_purchase)))

  invisible(NULL)
}
