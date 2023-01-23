#' Build answer options
#'
#' Buils answer options for multiple choice questions
#'
#'  This functions builds 5 (and only 5, currently) dynamic answer options.
#' Useful for dynamic exercises in r-exams.
#' At present, two algorithms are supported for creating answer options: "coef" and "prop"
#' "coef" builds options by multiplying the correct answer by `scale_factor` (and adds a constant in some instances; see below for details)
#' "prop" build options by multiplying the odds by some `scale_factor` (3 in default)
#' The correct answer is the FIRST of element of the returned answers (the rest are the wrong ones)
#'
#' @param correct_answer numeric the value of the correct answer
#' @param nr_answer_options defaults to 5 (integer), other values or not supported currently
#' @param algo_wrong_answers "coef" (large range of values) or "prop" (values from 0 to 1)
#' @param scale_factor defaults to 2, useful for building plausible distractors which as a function of a multiple of the true solution
#' @param format_answer defaults to FALSE using the functions exams::fmt()
#'
#' @return numeric vector of length 5
#' @export
#'
#' @examples
#' \dontrun{build_answer_options(42)}
#'  42.0000000 -41.0000000   0.2896458   0.4603042   0.7941220
build_answer_options <- function(correct_answer, nr_answer_options = 5, algo_wrong_answers = "prop", scale_factor = 2, format_answer = FALSE){


  if (nr_answer_options != 5) stop("Only 5 answer options are supported at the moment.")
  if (!(algo_wrong_answers %in% c("prop", "coef"))) stop(paste0(algo_wrong_answers, ": This method for construction wrong answers is not supported."))
  if (is.null(correct_answer)) stop("Please specify the correct answer")
  if (is.na(correct_answer)) stop("Please specify the correct answer")
  if (correct_answer == "") stop("Please specify the correct answer")



  wrong_answers <- NA


  # build answer options if algo is "coef":
  if (algo_wrong_answers == "coef"){
    k <- scale_factor
    if (abs(round(correct_answer, 1 )) < 0.1) k <- 1
    wrong_answers <- c(correct_answer * scale_factor + k,
                       correct_answer / scale_factor + k*2,
                       correct_answer * (scale_factor + 1) + k + 1,
                       correct_answer  *  (scale_factor + 1) - k - 1)
    answer_options <- c(correct_answer, wrong_answers)
    answer_options_fmt <- exams::fmt(answer_options)
  }

  mutate_prop_by_odd <- function(prop, scale_by_odds_factor) {
    # helper function
    odds <- prop / (1 - prop)

    odds_new <- odds * scale_by_odds_factor

    prop_new <- odds_new / (1 + odds_new)

    out <- prop_new

    return(out)
  }


  # build answer options if algo is "prop":
  if (algo_wrong_answers == "prop"){

    correct_answer_odds <- correct_answer / (1 - correct_answer)
    if (correct_answer_odds > 8 | correct_answer_odds < .125) {
      e1 <- runif(1,.01, .05)
      e2 <- runif(1, -0.07, 0.07)
      e3 <- runif(1, -0.08, 0.08)
      wrong_answers <- c(1-correct_answer,
                         .25 + e1,
                         .50 + e2,
                         .75 + e3)
    } else {
      wrong_answers <- c(mutate_prop_by_odd(correct_answer,  2),
                         mutate_prop_by_odd(correct_answer,  4),
                         mutate_prop_by_odd(correct_answer,  1/2),
                         mutate_prop_by_odd(correct_answer,  1/4))
    }
    answer_options <- c(correct_answer, wrong_answers)
  }


  # rm duplicates:
  while (any(duplicated(answer_options))) {

    is_duplicate <- duplicated(answer_options)
    which_duplicate <- which(duplicated(answer_options))

    for (i in which_duplicate){
      if (algo_wrong_answers == "prop") {answer_options[i] <-
        answer_options[i-1] + sample(setdiff(seq(0.05, .95, by = .1), answer_options), 1)}
      if (algo_wrong_answers == "coef") {answer_options[i] <-
        answer_options[i-1] + sample(setdiff(seq(0.05, .95, by = .1), answer_options), 1)}
    }

  } # end of while

  if (format_answer) answer_options <- exams::fmt(answer_options)


  return(answer_options)


}

