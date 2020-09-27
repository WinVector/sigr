
test_utility_calc <- function() {
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
  RUnit::checkTrue(is.null(check_utility_calc(values)))

  # cat(wrapr::draw_frame(values))

  expect <- wrapr::build_frame(
    "model"                  , "threshold", "count_taken", "fraction_taken", "true_positive_value", "false_positive_value", "true_negative_value", "false_negative_value", "total_value", "true_negative_count", "false_negative_count", "true_positive_count", "false_positive_count" |
      "predicted_probability", 0          , 4            , 1               , 95                   , -15                   , 0                    , 0                     , 80           , 0                    , 0                     , 1                    , 3                      |
      "predicted_probability", 0.25       , 3            , 0.75            , 95                   , -10                   , 0.001                , 0                     , 85           , 1                    , 0                     , 1                    , 2                      |
      "predicted_probability", NA_real_   , 0            , 0               , 0                    , 0                     , 0.003                , -0.01                 , -0.007       , 3                    , 1                     , 0                    , 0                      )

  wrapr::check_equiv_frames(values, expect, tolerance = 1.e-3)

  invisible(NULL)
}
