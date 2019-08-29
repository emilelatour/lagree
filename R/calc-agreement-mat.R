#' @title 
#' Calculate an agreement matrix
#' 
#' @description 
#' Create an n x q agreement matrix representing the distribution of raters by
#' subjects (n) and category (q)
#'
#' @param data A tbld_df or data.frame
#' @param ... Columns (unquoted) with the "ratings" done by the raters. Follows
#'   the argument methodology from `dplyr::select`.
#' @param subject_id Optional. Name of the column (unquoted) that contains the
#'   IDs for the subjects or units being rated.
#' 
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom purrr discard
#' @importFrom rlang enquo
#' @importFrom rlang quo_is_null
#' @importFrom tidyr gather
#' 
#' @references 
#' 2014. Handbook of Inter-Rater Reliability: The Definitive Guide to Measuring
#' the Extent of Agreement Among Raters. 4th ed. Gaithersburg, MD: Advanced
#' Analytics.
#'
#' @return An n x q table
#' @export
#'
#' @examples
#' # See Gwet page 72
#' # Classification of 12 subjects by 4 raters into 5 categories
#' 
#' library(dplyr)
#' library(tibble)
#' 
#' table_215 <- tibble::tribble(
#'   ~unit, ~rater_1, ~rater_2, ~rater_3, ~rater_4,
#'   1,      "a",      "a",       NA,      "a",
#'   2,      "b",      "b",      "c",      "b",
#'   3,      "c",      "c",      "c",      "c",
#'   4,      "c",      "c",      "c",      "c",
#'   5,      "b",      "b",      "b",      "b",
#'   6,      "a",      "b",      "c",      "d",
#'   7,      "d",      "d",      "d",      "d",
#'   8,      "a",      "a",      "b",      "a",
#'   9,      "b",      "b",      "b",      "b",
#'   10,       NA,      "e",      "e",      "e",
#'   11,       NA,       NA,      "a",      "a",
#'   12,       NA,       NA,      "c",       NA
#' )
#' 
#' calc_agree_mat(data = table_215, 
#'                dplyr::starts_with("rater"))
#' calc_agree_mat(data = table_215, 
#'                dplyr::starts_with("rater"), 
#'                subject_id = "unit")
#' 
calc_agree_mat <- function(data, ..., 
                           subject_id = NULL) { 
  
  subject_id <- rlang::enquo(subject_id)
  
  
  ## Creating a vector the categories used by the raters ---------------- 
  
  # If the raw data are factors, then this will get the unique factor levels
  fac_lvls <- data %>% 
    dplyr::select(...) %>% 
    unlist() %>% 
    unique() %>% 
    levels()
  
  # Else if there are not factor levels then this will get the unique values
  if (is.null(fac_lvls)) { 
    fac_lvls <- data %>% 
      dplyr::select(...) %>% 
      unlist() %>% 
      unique() %>% 
      purrr::discard(., is.na)
  }
  
  
  ## Calculate number of raters, subjects, and categories ---------------- 
  
  summary_counts <- data %>% 
    dplyr::select(...) %>% 
    summarise(k_raters = ncol(.), 
              n_subjects = nrow(.), 
              q_categories = length(fac_lvls))
  
  
  if (rlang::quo_is_null(subject_id)) { 
    
    agree_mat <- data %>% 
      dplyr::select(...) %>% 
      mutate(subject = dplyr::row_number(), 
             subject = factor(subject)) %>% 
      tidyr::gather(., 
                    key = "raters", 
                    value = "ratings", 
                    - subject, 
                    factor_key = TRUE) %>% 
      mutate(ratings = factor(ratings, 
                              levels = fac_lvls)) %>% 
      with(., table(subject, ratings))
    
  } else if (!rlang::quo_is_null(subject_id)) { 
    
    agree_mat <- data %>% 
      dplyr::select(!! subject_id, ...) %>%  
      tidyr::gather(.,
                    key = "raters",
                    value = "ratings",
                    ...,
                    factor_key = TRUE) %>%
      mutate(ratings = factor(ratings,
                              levels = fac_lvls)) %>%
      dplyr::select(!! subject_id, ratings) %>%
      table(.) 
    
  }
  
  return(agree_mat)
  
}


# #### Gwet's version -------------------------------- 
# 
# 
# # ==============================================================
# # trim(x): This is an r function for trimming leading and trealing blanks
# # ==============================================================
# trim <- function( x ) {
#   gsub("(^[[:space:]]+|[[:space:]]+$)", "", x) 
# }
# 
# calc_agree_mat_gwet <- function(data, ..., 
#                                     subject_id = NULL) { 
#   
#   ratings.mat <- data %>% 
#     dplyr::select(...) %>% 
#     as.matrix(.)
#   
#   if (is.character(ratings.mat)) {
#     ratings.mat <- trim(toupper(ratings.mat))
#     ratings.mat[ratings.mat == ''] <- NA_character_
#   }
#   
#   n <- nrow(ratings.mat) # number of subjects
#   r <- ncol(ratings.mat) # number of raters
#   # f <- n / N # finite population correction
#   
#   # creating a vector containing all categories used by the raters
#   
#   categ.init <- unique(na.omit(as.vector(ratings.mat)))
#   categ <- sort(categ.init)
#   q <- length(categ)
#   
#   agree.mat <- matrix(0, nrow = n, ncol = q)
#   
#   for (k in 1:q) {
#     categ.is.k <- (ratings.mat == categ[k])
#     agree.mat[, k] <-
#       (replace(categ.is.k, is.na(categ.is.k), FALSE)) %*% rep(1, r)
#   }
#   
#   return(agree.mat)
#   
#   
# }

