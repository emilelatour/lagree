#' @title Calculate pairwise agreement statistics for multiple raters
#'
#' @description This function calculates pairwise agreement statistics for
#' multiple raters. It computes statistics such as pairwise kappa (both
#' unweighted and weighted) and the proportion of observed agreement. It
#' summarizes these statistics in a table or data frame.
#'
#' @param data A data frame or tibble containing the ratings from multiple
#'   raters.
#' @param ... Variable (column) names of the raters in the data.
#' @param type A character string indicating whether to calculate "unweighted"
#'   or "weighted" kappa.
#'
#' @importFrom dplyr select mutate rename filter slice arrange pull summarise
#'   ungroup across
#' @importFrom tidyr crossing unnest spread
#' @importFrom rlang enquos .data
#' @importFrom broom tidy
#' @importFrom purrr map map2 map_int
#' @importFrom janitor clean_names
#' @importFrom irr agree
#' @importFrom psych cohen.kappa
#' @importFrom scales number
#' @importFrom stats qnorm
#'
#' @return A list containing the results and tables of the pairwise agreement
#'   statistics, including kappa results and observed agreement.
#'
#' @examples
#' diagnostic_df <- data.frame(stringsAsFactors = FALSE,
#'   id = c(1:30),
#'   rater_1 = c("Yes", "No", "No", "No", "No", "No", "No", "No", "No", "No",
#'               "No", "No", "No", "No", "No", "No", "No", "No", "No", "No",
#'               "No", "Yes", "No", "No", "No", "No", "No", "No", "No", "No"),
#'   rater_2 = c("Yes", "No", "No", "No", "No", "No", "No", "No", "No", "No",
#'               "Yes", "No", "No", "Yes", "No", "No", "No", "No", "No", "No",
#'               "No", "Yes", "No", "No", "Yes", "No", "No", "No", "No", "No"),
#'   rater_3 = c("Yes", "No", "No", "No", "No", "No", "No", "No", "Yes", "No",
#'               "Yes", "Yes", "No", "Yes", "Yes", "No", "No", "No", "Yes", "No",
#'               "No", "Yes", "Yes", "Yes", "Yes", "No", "No", "Yes", "No", "No")
#' )
#'
#' results <- calc_pw_kappa(diagnostic_df, rater_1, rater_2, rater_3, type = "unweighted")
#' names(results)
#' results$k_table
#' results$k_min_max
#' results$po_table
#' results$po_min_max
#'
#' @export
calc_pw_kappa <- function(data, ..., type = "unweighted") {

  # Silence Undefined global functions or variables warning
  combo <- conf_high <- conf_low <- estimate <- kap <- po <- se_po <- x <- y <- NULL
  lower_ci <- n <- upper_ci <- NULL

  vars <- rlang::enquos(...)

  #### Check number of ratings --------------------------------

  # check <- data %>%
  #   dplyr::select(!!! vars) %>%
  #   dplyr::pull(.) %>%
  #   unique(.)
  #
  # if (length(check) > 2) stop("Only 2 ratings are allowed.")

  #### Helper functions --------------------------------

  ## get_table ---------------

  get_table <- function(data, x, y) {

    # table(data[[x]], data[[y]])

    # Combine unique levels of both variables
    all_levels <- union(levels(factor(data[[x]])), levels(factor(data[[y]])))

    # Create table with counts for each combination of levels
    res_table <- table(factor(data[[x]], levels = all_levels),
                       factor(data[[y]], levels = all_levels))

    return(res_table)

  }

  ## get_kappa ---------------

  get_kappa <- function(table) {
    invisible(
      suppressWarnings(
        suppressMessages(
          broom::tidy(psych::cohen.kappa(x = table)) %>%
            # dplyr::filter(type == type) %>%
            janitor::clean_names(.) #%>%
          # dplyr::select(estimate, conf_low, conf_high)
        )
      )
    )
  }

  ## get_agree ----------------

  # get_agree <- function(data, x, y) {
  #   # x <- rlang::enquo(x)
  #   # y <- rlang::enquo(y)
  #   data %>%
  #     # dplyr::select(!! x, !! y) %>%
  #     dplyr::select(x, y) %>%
  #     irr::agree(.)
  #
  # }


  #### Pairwise kappa --------------------------------

  pw_k_results <- data %>%
    dplyr::select(!!! vars) %>%
    names() %>%
    tidyr::crossing(x = ., y = .) %>%
    mutate(table = purrr::map2(.x = x,
                               .y = y,
                               .f = ~ get_table(data = data, x = .x, y = .y)),
           kap = purrr::map(.x = table,
                            .f = ~ get_kappa(table = .x))) %>%
    tidyr::unnest(kap) %>%
    # dplyr::filter(type == type) %>%
    mutate(combo =
             paste0(scales::number(x = estimate,
                                   accuracy = 0.01,
                                   trim = FALSE),
                    " (",
                    scales::number(x = conf_low,
                                   accuracy = 0.01,
                                   trim = FALSE),
                    " to ",
                    scales::number(x = conf_high,
                                   accuracy = 0.01,
                                   trim = FALSE),
                    ")"))  %>%
    dplyr::rename(
      # new = old
      "lower_ci" = "conf_low",
      "upper_ci" = "conf_high"
    ) # %>%
  # dplyr::select(x, y, combo) %>%
  # tidyr::spread(key = y, value = combo)


  ## Make table of kappas ---------------

  if (type == "unweighted") {
    pw_k_results <- pw_k_results %>%
      dplyr::filter(type == "unweighted")
  } else if (type == "weighted") {
    pw_k_results <- pw_k_results %>%
      dplyr::filter(type == "weighted")
  }


  pw_k_table <- pw_k_results %>%
    dplyr::select(x, y, combo) %>%
    tidyr::spread(data = ., key = y, value = combo)


  ## Turn values above diagonal to "" ---------------
  pw_k_table[upper.tri(pw_k_table)] <- ""


  #### Get min/max kappa --------------------------------

  ranked_res <- pw_k_results %>%
    dplyr::filter(estimate < 1.0) %>%
    mutate(rank = rank(estimate,
                       ties.method = "first"))

  min_max_kappa <- dplyr::bind_rows(ranked_res %>%
                                      dplyr::slice(which.min(rank)),
                                    ranked_res %>%
                                      dplyr::slice(which.max(rank))
  )


  #### Observed agreement pairwise --------------------------------

  pw_po_results <- data %>%
    dplyr::select(!!! vars) %>%
    names() %>%
    tidyr::crossing(x = ., y = .) %>%
    mutate(table = purrr::map2(.x = x,
                               .y = y,
                               .f = ~ get_table(data = data, x = .x, y = .y)),
           n = purrr::map_int(.x = table,
                              .f = ~ sum(.x, na.rm = TRUE)),
           po = purrr::map_int(.x = table,
                               .f = ~ sum(diag(.x), na.rm = TRUE)),
           po = po / n
    ) %>%
    mutate(se_po = sqrt(po * (1 - po) / n),
           lower_ci = po + c(-1) * qnorm(1 - .05 / 2) * se_po,
           upper_ci = po + c(1) * qnorm(1 - .05 / 2) * se_po) %>%
    mutate(dplyr::across(.cols = c(po:upper_ci),
                         .fns = ~ round(x = .,
                                        digits = 3))) %>%
    mutate(combo =
             paste0(scales::number(x = po,
                                   accuracy = 0.01,
                                   trim = FALSE),
                    " (",
                    scales::number(x = lower_ci,
                                   accuracy = 0.01,
                                   trim = FALSE),
                    " to ",
                    scales::number(x = upper_ci,
                                   accuracy = 0.01,
                                   trim = FALSE),
                    ")"))

  ## Make table of observed agreement ---------------

  pw_po_table <- pw_po_results %>%
    dplyr::select(x, y, combo) %>%
    tidyr::spread(data = ., key = y, value = combo)


  ## Turn values above diagonal to "" ---------------
  pw_po_table[upper.tri(pw_po_table)] <- ""


  #### Get min/max po --------------------------------

  ranked_po <- pw_po_results %>%
    dplyr::filter(po < 1.0) %>%
    mutate(rank = rank(po,
                       ties.method = "first"))

  min_max_po <- dplyr::bind_rows(ranked_po %>%
                                   dplyr::slice(which.min(rank)),
                                 ranked_po %>%
                                   dplyr::slice(which.max(rank))
  )



  #### Return a list --------------------------------

  return(
    list(
      k_results = pw_k_results,
      k_table = pw_k_table,
      k_min_max = min_max_kappa,
      po_results = pw_po_results,
      po_table = pw_po_table,
      po_min_max = min_max_po
    ))


}
