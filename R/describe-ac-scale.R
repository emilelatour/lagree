
#' Interpretation Scale for Agreement Coefficients
#'
#' Provides interpretation scales for various agreement coefficients based on
#' different classification schemes. The function supports the following scales:
#' "Landis and Koch", "Fleiss", "Altman", "Cicchetti", "Koo and Li", and
#' "McHugh".
#'
#' @param scale A string specifying the scale to use for interpretation. The
#'   available scales are: "Landis and Koch", "Fleiss", "Altman", "Cicchetti",
#'   "Koo and Li", and "McHugh". Defaults to "Landis and Koch".
#'
#' @return A tibble containing the lower bound, upper bound, and interpretation
#'   for the selected scale.
#'
#' @references
#' \itemize{
#' \item \href{https://www.jstor.org/stable/2529310?seq=1#metadata_info_tab_contents}{Landis and Koch (1977)}
#' \item \href{http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.456.3830&rep=rep1&type=pdf}{Fleiss (1981)}
#' \item \href{https://www.crcpress.com/Practical-Statistics-for-Medical-Research/Altman/p/book/9780412276309}{Altman (1991)}
#' \item \href{https://www.researchgate.net/publication/232556850_Guidelines_Criteria_and_Rules_of_Thumb_for_Evaluating_Normed_and_Standardized_Assessment_Instrument_in_Psychology}{Cicchetti (1994)}
#' \item \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3900052/}{McHugh (2012)}
#' \item \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4913118/}{Koo and Li (2016)}
#'
#' }
#'
#' @examples
#' # Example usage with Landis and Koch scale
#' ac_scale(scale = "Landis and Koch")
#'
#' # Example usage with Fleiss scale
#' ac_scale(scale = "Fleiss")
#'
#' @export

ac_scale <- function(scale = "Landis and Koch") {

  if (scale == "Landis and Koch") {

    tibble::tribble(
      ~lower_bound, ~upper_bound,  ~interpretation,
      0.8,          1.00, "Almost perfect",
      0.6,          0.8,  "Substantial",
      0.4,          0.6,  "Moderate",
      0.2,          0.4,  "Fair",
      0.0,          0.2,  "Slight",
      -1,         0.0,  "Poor"
    )

  } else if (scale == "Fleiss") {

    tibble::tribble(
      ~lower_bound, ~upper_bound,  ~interpretation,
      0.75,         1.00, "Excellent",
      0.40,         0.75, "Intermediate to good",
      -1,         0.40, "Poor"
    )

  } else if (scale == "Altman") {

    tibble::tribble(
      ~lower_bound, ~upper_bound,  ~interpretation,
      0.8,          1.00, "Very good",
      0.6,          0.8,  "Good",
      0.4,          0.6,  "Moderate",
      0.2,          0.4,  "Fair",
      -1,         0.2,  "Poor"
    )

  } else if (scale == "Cicchetti") {

    tibble::tribble(
      ~lower_bound, ~upper_bound,  ~interpretation,
      0.75,         1.00, "Excellent",
      0.6,          0.75, "Good",
      0.4,          0.6,  "Fair",
      -1,         0.4,  "Poor"
    )

  } else if (scale == "Koo and Li") {

    tibble::tribble(
      ~lower_bound, ~upper_bound,  ~interpretation,
      0.9,          1.00, "Excellent",
      0.75,         0.9,  "Good",
      0.5,          0.75, "Moderate",
      -1,         0.5,  "Poor"
    )

  } else if (scale == "McHugh") {

    tibble::tribble(
      ~lower_bound, ~upper_bound,  ~interpretation,
      0.9,          1.00, "Almost perfectt",
      0.8,          0.9,  "Strong",
      0.6,          0.8,  "Moderate",
      0.4,          0.6,  "Weak",
      0.21,         0.4,  "Minimal",
      -1,         0.21, "None"
    )
  }
}
