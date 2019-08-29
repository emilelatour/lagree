
# #### Packages -------------------------------- 
# 
pacman::p_load(
  tidyverse,     # packages ggplot2, dplyr, tidyr, readr, purrr, tibble,
  # stringr, and forcats
  install = FALSE
)


#### radiologist data -------------------------------- 

# From Altman, statistics in medicine p. 403
# https://books.google.com/books?id=v-walRnRxWQC&pg=PA403&lpg=PA403&dq=85+xeromammagrams+agreement&source=bl&ots=Sx_ZEhxs4j&sig=ACfU3U2anEuufva5kbwr0IHX6PlLjf0EGw&hl=en&sa=X&ved=2ahUKEwjqobCuiJzkAhVBGzQIHaX4CqoQ6AEwE3oECAgQAQ#v=onepage&q=85%20xeromammagrams%20agreement&f=false

# From larger study: Boyd et al., 1982

radiologist <- tibble::tribble(
  ~radiologist_a, ~radiologist_b,
        "Normal",       "Normal",
        "Normal",       "benign",
        "benign",       "Normal",
        "benign",       "benign",
        "benign",      "suspect",
       "suspect",       "Normal",
       "suspect",       "benign",
       "suspect",      "suspect",
       "suspect",       "cancer",
        "cancer",       "cancer",
        "Normal",       "Normal",
        "Normal",       "Normal",
        "Normal",       "Normal",
        "Normal",       "Normal",
        "Normal",       "Normal",
        "Normal",       "Normal",
        "Normal",       "Normal",
        "Normal",       "Normal",
        "Normal",       "Normal",
        "Normal",       "Normal",
        "Normal",       "Normal",
        "Normal",       "Normal",
        "Normal",       "Normal",
        "Normal",       "Normal",
        "Normal",       "Normal",
        "Normal",       "Normal",
        "Normal",       "Normal",
        "Normal",       "Normal",
        "Normal",       "Normal",
        "Normal",       "Normal",
        "Normal",       "benign",
        "Normal",       "benign",
        "Normal",       "benign",
        "Normal",       "benign",
        "Normal",       "benign",
        "Normal",       "benign",
        "Normal",       "benign",
        "Normal",       "benign",
        "Normal",       "benign",
        "Normal",       "benign",
        "Normal",       "benign",
        "benign",       "Normal",
        "benign",       "Normal",
        "benign",       "Normal",
        "benign",       "benign",
        "benign",       "benign",
        "benign",       "benign",
        "benign",       "benign",
        "benign",       "benign",
        "benign",       "benign",
        "benign",       "benign",
        "benign",       "benign",
        "benign",       "benign",
        "benign",       "benign",
        "benign",       "benign",
        "benign",       "benign",
        "benign",       "benign",
        "benign",       "benign",
        "benign",       "benign",
        "benign",       "benign",
       "suspect",       "Normal",
       "suspect",       "Normal",
       "suspect",       "benign",
       "suspect",       "benign",
       "suspect",       "benign",
       "suspect",       "benign",
       "suspect",       "benign",
       "suspect",       "benign",
       "suspect",       "benign",
       "suspect",       "benign",
       "suspect",      "suspect",
       "suspect",      "suspect",
       "suspect",      "suspect",
       "suspect",      "suspect",
       "suspect",      "suspect",
       "suspect",      "suspect",
       "suspect",      "suspect",
       "suspect",      "suspect",
       "suspect",      "suspect",
       "suspect",      "suspect",
       "suspect",      "suspect",
       "suspect",      "suspect",
       "suspect",      "suspect",
       "suspect",      "suspect",
       "suspect",       "cancer"
  )


radiologist <- radiologist %>% 
  mutate_at(.vars = vars(radiologist_a, radiologist_b), 
            .funs = list(~ factor(., 
                                  levels = c("Normal", "benign", "suspect", "cancer"))))

# radiologist
# 
# save(radiologist, 
#      file = "/Users/latour/Dropbox/repos/lagree/data/radiologist.rda")


#### Example 2 -- fictional data -------------------------------- 

two_raters <- tibble::tribble(
  ~rater_a, ~rater_b,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  1,        1,
  2,        1,
  2,        1,
  1,        2,
  1,        2,
  1,        2,
  1,        2,
  1,        2) %>% 
  mutate_all(.tbl = ., 
             .funs = list(~ as.factor(.)))


# two_raters
# 
# save(two_raters,
#      file = "/Users/latour/Dropbox/repos/lagree/data/two_raters.rda")


#### Multi rater exmple -------------------------------- 

# 5 raters classify 10 subjects into 1 of 3 rating categories

rvary2 <- tibble::tribble(
  ~subject, ~rater1, ~rater2, ~rater3, ~rater4, ~rater5,
         1,       1,       2,       2,      NA,       2,
         2,       1,       1,       3,       3,       3,
         3,       3,       3,       3,       3,       3,
         4,       1,       1,       1,       1,       3,
         5,       1,       1,       1,       3,       3,
         6,       1,       2,       2,       2,       2,
         7,       1,       1,       1,       1,       1,
         8,       2,       2,       2,       2,       3,
         9,       1,       3,      NA,      NA,       3,
        10,       1,       1,       1,       3,       3
  ) %>% 
  mutate_at(.vars = vars(rater1:rater5), 
            .funs = list(~ factor(., levels = c(1, 2, 3))))

# rvary2
# 
# save(rvary2,
#      file = "/Users/latour/Dropbox/repos/lagree/data/rvary2.rda")


#### neurologists -------------------------------- 

# Diagnostic Classification of Multiple Sclerosis Patients by Two Neurologists.
# From Landis and Koch (1977)

# two neurologists classified 69 patients into 4 diagnostic categories

# neuro_table <- matrix(c(5, 3, 0, 0,
#                         3, 11, 4, 0,
#                         2, 13, 3, 4,
#                         1, 2, 4, 14), ncol = 4, byrow = TRUE)
# 
# dimnames(neuro_table) <- list(new_orleans = c("1", "2", "3", "4"), 
#                               winnipeg = c("1", "2", "3", "4"))

neurologists <- tibble::tribble(
                  ~new_orleans, ~winnipeg,
                             1,         1,
                             1,         1,
                             1,         1,
                             1,         1,
                             1,         1,
                             2,         1,
                             2,         1,
                             2,         1,
                             3,         1,
                             3,         1,
                             4,         1,
                             1,         2,
                             1,         2,
                             1,         2,
                             2,         2,
                             2,         2,
                             2,         2,
                             2,         2,
                             2,         2,
                             2,         2,
                             2,         2,
                             2,         2,
                             2,         2,
                             2,         2,
                             2,         2,
                             3,         2,
                             3,         2,
                             3,         2,
                             3,         2,
                             3,         2,
                             3,         2,
                             3,         2,
                             3,         2,
                             3,         2,
                             3,         2,
                             3,         2,
                             3,         2,
                             3,         2,
                             4,         2,
                             4,         2,
                             2,         3,
                             2,         3,
                             2,         3,
                             2,         3,
                             3,         3,
                             3,         3,
                             3,         3,
                             4,         3,
                             4,         3,
                             4,         3,
                             4,         3,
                             3,         4,
                             3,         4,
                             3,         4,
                             3,         4,
                             4,         4,
                             4,         4,
                             4,         4,
                             4,         4,
                             4,         4,
                             4,         4,
                             4,         4,
                             4,         4,
                             4,         4,
                             4,         4,
                             4,         4,
                             4,         4,
                             4,         4,
                             4,         4)

# neurologists
# 
# save(neurologists,
#      file = "/Users/latour/Dropbox/repos/lagree/data/neurologists.rda")


#### Table 1 of Fleiss (1971) -------------------------------- 

# Distribution of 6 Raters by Subject and Category: six raters classified 4
# patients into 5 diagnostic categories


# psych_dx_table <- matrix(c(0, 0, 0, 6, 0, 
#                            0, 1, 4, 0, 1, 
#                            2, 0, 4, 0, 0, 
#                            0, 3, 3, 0, 0), ncol = 5, byrow = TRUE)
# 
# dimnames(psych_dx_table) <- list(subject = c("A", "B", "C", "D"), 
#                                  category = c("Depression", 
#                                               "Personality disorder", 
#                                               "Schizophrenia", 
#                                               "Neurosis", 
#                                               "Other"))
# 
# psych_dx_table
# 
# lamisc::counts_to_cases(psych_dx_table)



diagnosis <- tibble::tribble(
  ~subject,                ~rater1,                ~rater2,                ~rater3,                ~rater4,                ~rater5,                ~rater6,
  1,             "Neurosis",             "Neurosis",             "Neurosis",             "Neurosis",             "Neurosis",             "Neurosis",
  2, "Personality Disorder", "Personality Disorder", "Personality Disorder",                "Other",                "Other",                "Other",
  3, "Personality Disorder",        "Schizophrenia",        "Schizophrenia",        "Schizophrenia",        "Schizophrenia",                "Other",
  4,                "Other",                "Other",                "Other",                "Other",                "Other",                "Other",
  5, "Personality Disorder", "Personality Disorder", "Personality Disorder",             "Neurosis",             "Neurosis",             "Neurosis",
  6,           "Depression",           "Depression",        "Schizophrenia",        "Schizophrenia",        "Schizophrenia",        "Schizophrenia",
  7,        "Schizophrenia",        "Schizophrenia",        "Schizophrenia",        "Schizophrenia",                "Other",                "Other",
  8,           "Depression",           "Depression",        "Schizophrenia",        "Schizophrenia",        "Schizophrenia",             "Neurosis",
  9,           "Depression",           "Depression",             "Neurosis",             "Neurosis",             "Neurosis",             "Neurosis",
  10,                "Other",                "Other",                "Other",                "Other",                "Other",                "Other",
  11,           "Depression",             "Neurosis",             "Neurosis",             "Neurosis",             "Neurosis",             "Neurosis",
  12,           "Depression", "Personality Disorder",             "Neurosis",             "Neurosis",             "Neurosis",             "Neurosis",
  13, "Personality Disorder", "Personality Disorder", "Personality Disorder",        "Schizophrenia",        "Schizophrenia",        "Schizophrenia",
  14,           "Depression",             "Neurosis",             "Neurosis",             "Neurosis",             "Neurosis",             "Neurosis",
  15, "Personality Disorder", "Personality Disorder",             "Neurosis",             "Neurosis",             "Neurosis",                "Other",
  16,        "Schizophrenia",        "Schizophrenia",        "Schizophrenia",        "Schizophrenia",        "Schizophrenia",                "Other",
  17,           "Depression",           "Depression",           "Depression",             "Neurosis",                "Other",                "Other",
  18,           "Depression",           "Depression",           "Depression",           "Depression",           "Depression", "Personality Disorder",
  19, "Personality Disorder", "Personality Disorder",             "Neurosis",             "Neurosis",             "Neurosis",             "Neurosis",
  20,           "Depression",        "Schizophrenia",        "Schizophrenia",                "Other",                "Other",                "Other",
  21,                "Other",                "Other",                "Other",                "Other",                "Other",                "Other",
  22, "Personality Disorder",             "Neurosis",             "Neurosis",             "Neurosis",             "Neurosis",             "Neurosis",
  23, "Personality Disorder", "Personality Disorder",             "Neurosis",                "Other",                "Other",                "Other",
  24,           "Depression",           "Depression",             "Neurosis",             "Neurosis",             "Neurosis",             "Neurosis",
  25,           "Depression",             "Neurosis",             "Neurosis",             "Neurosis",             "Neurosis",                "Other",
  26, "Personality Disorder", "Personality Disorder", "Personality Disorder", "Personality Disorder", "Personality Disorder",             "Neurosis",
  27,           "Depression",           "Depression",           "Depression",           "Depression",                "Other",                "Other",
  28, "Personality Disorder", "Personality Disorder",             "Neurosis",             "Neurosis",             "Neurosis",             "Neurosis",
  29,           "Depression",        "Schizophrenia",        "Schizophrenia",        "Schizophrenia",        "Schizophrenia",        "Schizophrenia",
  30,                "Other",                "Other",                "Other",                "Other",                "Other",                "Other"
) %>% 
  mutate_at(.vars = vars(dplyr::starts_with("rater")), 
            .funs = list(~ factor(., 
                                  levels = c("Depression",
                                             "Personality Disorder",
                                             "Schizophrenia",
                                             "Neurosis",
                                             "Other"))))



diagnosis

calc_ac_ratings(data = dx_table, 
                dplyr::starts_with("rater"))


# diagnosis
# 
# save(diagnosis,
#      file = "/Users/latour/Dropbox/repos/lagree/data/diagnosis.rda")


#### Table 2 of Finn (1970) -------------------------------- 

# 5 raters classified 4 subjects into 3 categories labeled as {1, 2, 3}

finn_1970 <- tibble::tribble(
  ~subject, ~rater_I, ~rater_II, ~rater_III, ~rater_IV, ~rater_V,
       "A",        2,         2,          3,         2,        2,
       "B",        2,         2,          2,         2,        2,
       "C",        2,         2,          2,         2,        1,
       "D",        1,         2,          2,         2,        2
  ) %>% 
  mutate_at(.vars = vars(dplyr::starts_with("rater")), 
            .funs = list(~ factor(., 
                                  levels = c(1, 2, 3))))


# finn_1970
# 
# save(finn_1970,
#      file = "/Users/latour/Dropbox/repos/lagree/data/finn_1970.rda")




#### From irr package -------------------------------- 

# library(irr)
# data(anxiety)
# data("diagnoses")
# data("video")
# data("vision")
# video
