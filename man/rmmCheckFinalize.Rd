% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check.R
\name{rmmCheckFinalize}
\alias{rmmCheckFinalize}
\title{Run a final check of an rmm object}
\usage{
rmmCheckFinalize(rmm, family = c("base"))
}
\arguments{
\item{rmm}{a range model metadata list}

\item{family}{The rmm family to check the rmm against}
}
\value{
Prints feedback to point out possible errors.
}
\description{
Check an rmm object for non-standard and missing values and fields
}
\details{
See Examples.
}
\examples{
rmm <- rmmTemplate() # Make an empty template
rmmCheckFinalize(rmm)


}
\seealso{
Other check: 
\code{\link{rmmCheckEmpty}()},
\code{\link{rmmCheckMissingNames}()},
\code{\link{rmmCheckName}()},
\code{\link{rmmCheckValue}()},
\code{\link{rmmCleanNULLs}()}
}
\author{
Cory Merow <cory.merow@gmail.com>, Brian Maitner <bmaitner@gmail.com>,
}
\concept{check}
