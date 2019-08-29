#
# #### Packages --------------------------------
#
# library(tidyverse)
#
# #### Source other functions --------------------------------
#
# # here::here()
# # source(here::here("R",
# #                   "kappa-2-table.R"))
# #
# # source(here::here("R",
# #                   "scott-2-table.R"))
# #
# # source(here::here("R",
# #                   "ac-weights.R"))
# #
# # source(here::here("R",
# #                   "calc-agreement-mat.R"))
# #
# #
# # source(here::here("R",
# #                   "kappa-3-table.R"))
#
# #### Sample data --------------------------------
#
# # Rating of Four Subjects by Five Raters (Table 2 of Finn, 1970)
# finn_1970_table <- tibble::tribble(
#   ~subject, ~rater_I, ~rater_II, ~rater_III, ~rater_IV, ~rater_V,
#   "A",        2,         2,          3,         2,        2,
#   "B",        2,         2,          2,         2,        2,
#   "C",        2,         2,          2,         2,        1,
#   "D",        1,         2,          2,         2,        2) %>%
#   mutate_at(.vars = vars(dplyr::starts_with("rater")),
#             .funs = list(~ factor(.,
#                                   levels = c(1, 2, 3))))
#
# table_215 <- tibble::tribble(
#   ~unit, ~rater_1, ~rater_2, ~rater_3, ~rater_4,
#   1,      "a",      "a",       NA,      "a",
#   2,      "b",      "b",      "c",      "b",
#   3,      "c",      "c",      "c",      "c",
#   4,      "c",      "c",      "c",      "c",
#   5,      "b",      "b",      "b",      "b",
#   6,      "a",      "b",      "c",      "d",
#   7,      "d",      "d",      "d",      "d",
#   8,      "a",      "a",      "b",      "a",
#   9,      "b",      "b",      "b",      "b",
#   10,       NA,      "e",      "e",      "e",
#   11,       NA,       NA,      "a",      "a",
#   12,       NA,       NA,      "c",       NA
# )
#
#
# #### Arguments --------------------------------
# data <- neurologists %>%
#   dplyr::rename(rater_1 = new_orleans,
#                 rater_2 = winnipeg)
#
# data <- table_215
#
#
# # subject_id <- rlang::enquo(subject_id)
# # ...
# # ratings
# weights = "unweighted"
# conf_lev = 0.95
# N = Inf
# subject_id <- NULL
#
#
# ## Creating a vector the categories used by the raters ----------------
#
# # If the raw data are factors, then this will get the unique factor levels
# fac_lvls <- data %>%
#   # dplyr::select(...) %>%
#   dplyr::select(dplyr::starts_with("rater")) %>%
#   unlist() %>%
#   unique() %>%
#   levels()
#
# # Else if there are not factor levels then this will get the unique values
# if (is.null(fac_lvls)) {
#   fac_lvls <- data %>%
#     # dplyr::select(...) %>%
#   dplyr::select(dplyr::starts_with("rater")) %>%
#     unlist() %>%
#     unique() %>%
#     purrr::discard(., is.na)
# }
#
#
# ## Calculate number of raters, subjects, and categories ----------------
#
# summary_counts <- data %>%
#   # dplyr::select(...) %>%
#   dplyr::select(dplyr::starts_with("rater")) %>%
#   summarise(k_raters = ncol(.),
#             n_subjects = nrow(.),
#             q_categories = length(fac_lvls))
#
#
#
# ## Table for 2 rater case ----------------
#
# if (purrr::pluck(summary_counts, "k_raters") == 2) {
#
#   ## Make a q x q ratings matirx ----------------
#
#   ratings <- data %>%
#     # dplyr::select(...) %>%
#     dplyr::select(dplyr::starts_with("rater")) %>%
#     table()
#
#   if(dim(ratings)[1] != dim(ratings)[2]){
#     stop('The contingency table should have the same number of rows and columns!')
#   }
#
#   ## Calc number of subjects and categories ----------------
#
#   n <- sum(ratings) # number of subjects
#   f <- n / N # final population correction
#   q <- ncol(ratings) # number of categories
#
#
#   ## Calc the weights if given ----------------
#
#   if (any(weights %in% c("unweighted",
#                          "quadratic",
#                          "linear",
#                          "ordinal",
#                          "radical",
#                          "ratio",
#                          "circular",
#                          "bipolar"))) {
#     weights_mat <- ac_weights(categ = c(1:q),
#                               weight_type = weights)
#   } else {
#     weights_mat <- weights
#   }
#
#   if (dim(weights_mat)[1] != dim(weights_mat)[2]) {
#     stop('The weights provided should have the same number of rows and columns!')
#   }
#
#   if (dim(weights_mat)[1] != q) {
#     stop('The weights table is not the same size as the ratings table')
#   }
#
#
#   ## Calculate results table ----------------
#
#   results_table <- dplyr::bind_rows(
#     # bp2_table(ratings = tab),
#     kappa_2_table(ratings, weights_mat, conf_lev, N),
#     scott_2_table(ratings, weights_mat, conf_lev, N) # ,
#     # gwet_ac1_table(ratings = tab),
#     # krippen2_table(ratings = tab)
#   )
#
#   ## Update summary counts table ----------------
#
#   # Does not really apply in the 2x2 case. Stata just shows "ratings per
#   # subject" which will always be two...
#
#
#
#   ## Calc table for 3 or more raters ----------------
#
# } else if (purrr::pluck(summary_counts, "k_raters") >= 3) {
#
#
#   ## Make a n x q ratings matirx ----------------
#   # n x q agreement matrix representing the distribution of raters by subjects
#   # (n) and category (q)
#   ratings <- calc_agree_mat(data,
#                             # ...,
#                             dplyr::starts_with("rater"),
#                             subject_id)
#
#   ## Calc number of subjects and categories ----------------
#
#   agree_mat <- as.matrix(ratings)
#
#   n <- nrow(agree_mat) # number of subjects
#   q <- ncol(agree_mat) # number of categories
#
#   f <- n / N # final population correction
#
#
#   ## Create the weights matrix ----------------
#
#   if (any(weights %in% c("unweighted",
#                          "quadratic",
#                          "linear",
#                          "ordinal",
#                          "radical",
#                          "ratio",
#                          "circular",
#                          "bipolar"))) {
#     weights_mat <- ac_weights(categ = c(1:q),
#                               weight_type = weights)
#   } else {
#     weights_mat <- weights
#   }
#
#   if (dim(weights_mat)[1] != dim(weights_mat)[2]) {
#     stop('The weights provided should have the same number of rows and columns!')
#   }
#
#   if (dim(weights_mat)[1] != q) {
#     stop('The weights table is not the same dimension as the number of categories')
#   }
#
#
#   ## Calculate results table ----------------
#
#   results_table <- dplyr::bind_rows(
#     # bp2_table(ratings = tab),
#     kappa_3_table(ratings, weights_mat, conf_lev, N),
#     # scott_2_table(ratings, weights_mat, conf_lev, N) # ,
#     # gwet_ac1_table(ratings = tab),
#     # krippen2_table(ratings = tab)
#   )
#
# }
#
# return(results_table)
#
#
#
#
# #### Make it a function --------------------------------
#
#
# calc_ac_table <- function(data,
#                           ...,
#                           weights = "unweighted",
#                           conf_lev = 0.95,
#                           N = Inf,
#                           subject_id = NULL) {
#
#
#
#   ## Create vector of the categories used by the raters ----------------
#
#   # If the raw data are factors, then this will get the unique factor levels
#   fac_lvls <- data %>%
#     dplyr::select(...) %>%
#     unlist() %>%
#     unique() %>%
#     levels()
#
#   # Else if there are not factor levels then this will get the unique values
#   if (is.null(fac_lvls)) {
#     fac_lvls <- data %>%
#       dplyr::select(...) %>%
#       unlist() %>%
#       unique() %>%
#       purrr::discard(., is.na)
#   }
#
#
#   ## Calculate number of raters, subjects, and categories ----------------
#
#   summary_counts <- data %>%
#     dplyr::select(...) %>%
#     summarise(k_raters = ncol(.),
#               n_subjects = nrow(.),
#               q_categories = length(fac_lvls))
#
#
#
#   ## Table for 2 rater case ----------------
#
#   if (purrr::pluck(summary_counts, "k_raters") == 2) {
#
#     ## Make a q x q ratings matirx ----------------
#
#     ratings <- data %>%
#       dplyr::select(...) %>%
#       table()
#
#     if(dim(ratings)[1] != dim(ratings)[2]){
#       stop('The contingency table should have the same number of rows and columns!')
#     }
#
#     ## Calc number of subjects and categories ----------------
#
#     n <- sum(ratings) # number of subjects
#     f <- n / N # final population correction
#     q <- ncol(ratings) # number of categories
#
#
#     ## Calc the weights if given ----------------
#
#     if (any(weights %in% c("unweighted",
#                            "quadratic",
#                            "linear",
#                            "ordinal",
#                            "radical",
#                            "ratio",
#                            "circular",
#                            "bipolar"))) {
#       weights_mat <- ac_weights(categ = c(1:q),
#                                 weight_type = weights)
#     } else {
#       weights_mat <- weights
#     }
#
#     if (dim(weights_mat)[1] != dim(weights_mat)[2]) {
#       stop('The weights provided should have the same number of rows and columns!')
#     }
#
#     if (dim(weights_mat)[1] != q) {
#       stop('The weights table is not the same size as the ratings table')
#     }
#
#
#     ## Calculate results table ----------------
#
#     results_table <- dplyr::bind_rows(
#       # bp2_table(ratings = tab),
#       kappa_2_table(ratings, weights_mat, conf_lev, N),
#       scott_2_table(ratings, weights_mat, conf_lev, N) # ,
#       # gwet_ac1_table(ratings = tab),
#       # krippen2_table(ratings = tab)
#     )
#
#     ## Update summary counts table ----------------
#
#     # Does not really apply in the 2x2 case. Stata just shows "ratings per
#     # subject" which will always be two...
#
#
#
#     ## Calc table for 3 or more raters ----------------
#
#   } else if (purrr::pluck(summary_counts, "k_raters") >= 3) {
#
#
#     ## Make a n x q ratings matirx ----------------
#     # n x q agreement matrix representing the distribution of raters by subjects
#     # (n) and category (q)
#     ratings <- calc_agree_mat(data,
#                               ...,
#                               subject_id)
#
#     ## Calc number of subjects and categories ----------------
#
#     agree_mat <- as.matrix(ratings)
#
#     n <- nrow(agree_mat) # number of subjects
#     q <- ncol(agree_mat) # number of categories
#
#     f <- n / N # final population correction
#
#
#     ## Create the weights matrix ----------------
#
#     if (any(weights %in% c("unweighted",
#                            "quadratic",
#                            "linear",
#                            "ordinal",
#                            "radical",
#                            "ratio",
#                            "circular",
#                            "bipolar"))) {
#       weights_mat <- ac_weights(categ = c(1:q),
#                                 weight_type = weights)
#     } else {
#       weights_mat <- weights
#     }
#
#     if (dim(weights_mat)[1] != dim(weights_mat)[2]) {
#       stop('The weights provided should have the same number of rows and columns!')
#     }
#
#     if (dim(weights_mat)[1] != q) {
#       stop('The weights table is not the same dimension as the number of categories')
#     }
#
#
#     ## Calculate results table ----------------
#
#     results_table <- dplyr::bind_rows(
#       # bp2_table(ratings = tab),
#       kappa_3_table(ratings, weights_mat, conf_lev, N),
#       # scott_2_table(ratings, weights_mat, conf_lev, N) # ,
#       # gwet_ac1_table(ratings = tab),
#       # krippen2_table(ratings = tab)
#     )
#
#   }
#
#   return(results_table)
#
# }
#
#
# #### Arguments --------------------------------
# data <- neurologists %>%
#   dplyr::rename(rater_1 = new_orleans,
#                 rater_2 = winnipeg)
#
# data <- table_215
#
#
# #### 2 rater example --------------------------------
#
# calc_ac_table(data = neurologists,
#               new_orleans, winnipeg)
#
#
# #### 2 rater example --------------------------------
#
# calc_ac_table(data = table_215,
#               dplyr::starts_with("rater"))
