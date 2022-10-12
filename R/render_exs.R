
#' render R/exams exercises
#'
#' Renders R/exams exercises file to an output format such as html.
#'
#' @param exs list of file name of exercises to be rendered (string)
#' @param thema_nr (optinal) Should a number be suffixed to the resulting exercise file? (string)
#' @param render_moodle (FALSE) render to Moodle format?
#' @param html (TRUE) render to HTML format using a simple stylesheet?
#' @param pdf_print (TRUE) print from HTML to pdf?
#' @param yamlrmd (FALSE) render to yaml rmd?
#' @param output_path path where should the output file be saved (string)
#' @param template the HTML template to use (path) (string)
#' @param use_seed (TRUE) Should the random seed for the exercises (if used) be stored for reproducibility?
#' @param n_students (1) How many variants should be divised in case of dynamic exercises?
#' @param output_name (NULL, optional) an optional output name of the resulting file
#' @param prime_factor (1) use different primes for the seeds
#' @param my_edir path where the exercise file can be found (string)
#' @param verbose (FALSE) print out details
#'
#' @return nothing of importance, but files are written
#' @export
#'
#' @examples
#' \dontrun{render_exs(my_exs)}

render_exs <- function(exs,
                       thema_nr = "",
                       render_moodle = FALSE,
                       render_html = TRUE,
                       render_pdf_print = TRUE,
                       render_markdown = TRUE,
                       render_yamlrmd = FALSE,
                       output_path,
                       template = "plain.html",
                       use_seed = TRUE,
                       n_students = 1,
                       output_name = NULL,
                       prime_factor = 1,
                       my_edir = "exercises/sebastiansauer",
                       verbose = FALSE,
                       ...) {

  # output: rendered exercises in different formats (html with/without solutions, pdf print from html)
  # details: a seed matrix is used, to change set `use_seed = FALSE`

  if (is.null(output_name)) {
    output_name_aufgaben <- paste0("Thema",thema_nr ,"-Aufgaben")
    output_name_loesungen <- paste0("Thema",thema_nr ,"-Loesungen")
  } else {
    output_name_aufgaben <- output_name
    output_name_loesungen <- output_name
  }

  n_exs <- length(exs)

  cat(paste0("Output path is: ",output_path, "\n"))
  cat(paste0("Number of exercises is: ", n_exs,"\n"))
  cat(paste0("Number of students (different versions) is: ", n_students,"\n"))



  set_seed_matrix <- function(primefactor) {
    # this helper function determines the randoms seeds for reproducibility
    # most importantly, the exercises will have the same numbers, so that confusion may be avoided.

    n_exs_times_n_students <- n_exs*n_students

    if (prime_factor > 10)
      stop("Prime factor must be not bigger than 10.")

   data(primes, package = "teachertools")


    #prime_numbers <-
      #primes::generate_n_primes(n_exs_times_n_students * 10)

    prime_start <- n_exs_times_n_students * (primefactor - 1) + 1
    primes_stop <- n_exs_times_n_students * primefactor

    if (primes_stop > 1000) stop("Prime number to large. Reduce prime numbers or students or exercises")


    primes_chosen <- primes$prime_numbers[prime_start:primes_stop]

    stopifnot(length(primes_chosen) == n_exs_times_n_students)

    seed_matrix <-
      primes_chosen %>%
      matrix(nrow = n_students,
             byrow = T)

    return(seed_matrix)
  }

  seed_matrix <- set_seed_matrix(primefactor = prime_factor)



  # HTML OHNE Lösungen
  if (render_html) {

    cat("Now rendering exercises without solutions.\n")

    exams::exams2html(
      file = exs,
      , n = n_students
      , dir = output_path
      , name = output_name_aufgaben
      , question = "<h4>Aufgabe</h4>",
      #, solution = "<h4>Lösung</h4>"
      , solution = FALSE,  #"<h4>Lösung</h4>"
      , mathjax = TRUE  # needed for Chrome only,
      , edir = my_edir
      , seed = seed_matrix
      #, template = "templates/template_uebungsaufgaben_v2.html"
      , ...
    )
  }


  # HTML MIT Lösungen
  if (render_html) {

    cat("Now rendering exercises with solutions.\n")

    if (is.null(output_name)) output_name <- paste0("Thema",thema_nr ,"-Loesungen")


    exams::exams2html(
      file = exs,
      , n = n_students
      , dir = output_path
      , name = output_name_loesungen
      , question = "<h4>Aufgabe</h4>",
      , solution = "<h4>Lösung</h4>"
      #, solution = FALSE,  #"<h4>Lösung</h4>"
      , mathjax = TRUE  # needed for Chrome only,
      , edir = my_edir
      , seed = seed_matrix
      #, template = "templates/template_loesungen.html"
      , ...
    )
  }


  if (render_markdown) {

    cat("Now rendering exercises with solutions to MARKDOWN.\n")

    if (is.null(output_name)) output_name <- paste0("Thema",thema_nr ,"-Loesungen")


    exams::exams2pandoc(
      file = exs,
      , n = n_students
      , dir = output_path
      , name = output_name_loesungen
      , template = template
      , question = "Aufgabe",
      , solution = "Lösung"
      , type = "markdown"
      #, solution = FALSE,  #"<h4>Lösung</h4>"
      , mathjax = TRUE  # needed for Chrome only,
      , edir = my_edir
      , ...
    )
  }


  if (render_pdf_print) {

    cat("Now printing HTML to PDF.\n")

    pagedown::chrome_print(
      input = paste0(output_path,"/Thema",thema_nr ,"-Loesungen1.html"),
      output = paste0(output_path,"/Thema", thema_nr ,"-Loesungen1.pdf"),
      wait = 10,
      timeout = 30,
      verbose = 1
    )
  }





  if (render_moodle) {
    # Render MOODLE

    cat("Now rendering to Moodle XML.\n")

    exams::exams2moodle(
      file = exs,
      , name = output_name_aufgaben
      , dir = output_path
      , n = n_students
      , edir = my_edir
      , verbose = TRUE
      , quiet = FALSE
      , rule = "none"
      , mchoice = list(shuffle = TRUE,
                       eval = exams_eval(rule = "none"))
      , schoice = list(shuffle = TRUE,
                       eval = exams_eval(rule = "none"))
      , ...
    )
  }


  if (render_yamlrmd) {
    #source("https://raw.githubusercontent.com/sebastiansauer/Lehre/main/R-Code/exam2yamlrmd.R")


    exs_w_path <- vector(mode = "character", length = length(exs))

    # check if file exists:
    for (i in seq_along(exs)){
      if (verbose) cat(paste0("Now processing file ", i, ": ", exs[i],"\n"))

      exs_w_path[i] <- list.files(my_edir,
                                  pattern = exs[i],
                                  recursive = TRUE,
                                  full.names = TRUE) %>%
        here::here()

      stopifnot(file.exists(exs_w_path[i]))
    }

    # convert to yaml-rmd:
    for (i in seq_along(exs)){
      exam2yamlrmd(examfile = exs_w_path[i],
                   path_output = output_path,
                   print_categories = TRUE)
    }



  }


}

