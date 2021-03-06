% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyzeMatrixPopModels.R
\name{analyzeMatrixPopModels}
\alias{analyzeMatrixPopModels}
\title{Analyze Matrix Population Models}
\usage{
analyzeMatrixPopModels(trans_data, SizeClass, TransitionYear, SeedSurvival,
  SeedBankSize, SeedsPerFruit, n.iter = 1000)
}
\arguments{
\item{trans_data}{dataset from which to create growth/retrogression/survival/fecundity transition matrix}

\item{SizeClass}{A vector containing the size breaks with which to create stages based on size.}

\item{SeedSurvival}{Rate at which seeds survive (i.e., rate at which seeds remain in seed stage). Can be a single value or vector of values.}

\item{SeedBankSize}{Number of seeds in the seed bank (can be a single value or vector of values).}

\item{SeedsPerFruit}{Average number of seeds produced per fruit (can be a single value or vector of values).}

\item{n.iter}{Number of iterations for pop.projection function. Default value is 1000.}

\item{clone_transition}{clonal reproduction transition matrix}
}
\value{
Returns a list including:
\itemize{
 	\item the growth/retrogression/survival/fecundity transition matrix and clonal reproduction transition matrix for each matrix population model analysis
 	\item the results of the matrix population model analysis, including repro. values for each stage, the stable stage distribution, sensitivities, elasticities, lambda, the damping ratio, the seed bank size, the number of seeds assumed per fruit, and the seed survival rate (the rate at which seeds stay in the seed stage from one year to the next).
	}
}
\description{
Analyze multiple matrix population models, allowing for a seed bank and clonal reproduction.
}

