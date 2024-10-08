% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interpret-kappa.R
\name{interpret_kappa}
\alias{interpret_kappa}
\title{Interpretation of kappa statistic}
\usage{
interpret_kappa(kappa, scale = "Landis and Koch")
}
\arguments{
\item{kappa}{Numeric; kappa statistic}

\item{scale}{Which benchmark scale to use for interpretation. The default is
that proposed by Landis and Koch (1977). Options include "Landis and Koch",
"Fleiss", "Altman", "Cicchetti", "McHugh", and "Koo and Li".}
}
\value{
A string
}
\description{
Returns the interpretation of the strength of agreement determined by the
kappa statistic with a variety of different benchmark scales to choose from.
In practice, these can be used with other agreement statistics as well.
}
\examples{
interpret_kappa(0.5)
interpret_kappa(0.896)
interpret_kappa(-0.896)
interpret_kappa(0.15)
interpret_kappa(0.15, "Altman")

}
\references{
\itemize{
\item \href{https://www.jstor.org/stable/2529310?seq=1#metadata_info_tab_contents}{Landis and Koch (1977)}
\item \href{http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.456.3830&rep=rep1&type=pdf}{Fleiss (1981)}
\item \href{https://www.crcpress.com/Practical-Statistics-for-Medical-Research/Altman/p/book/9780412276309}{Altman (1991)}
\item \href{https://www.researchgate.net/publication/232556850_Guidelines_Criteria_and_Rules_of_Thumb_for_Evaluating_Normed_and_Standardized_Assessment_Instrument_in_Psychology}{Cicchetti (1994)}
\item \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3900052/}{McHugh (2012)}
\item \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4913118/}{Koo and Li (2016)}

}
}
