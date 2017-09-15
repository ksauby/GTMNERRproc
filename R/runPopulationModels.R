#' Run Population Models
#'
#' @description Prepare transition matrices, then project matrices and analyze results.
#'
#' @param ldf list of data frames containing transition information for plants
#' @param cldf list of data frames containing information on clones and their parents
#' @param SizeClass
#' @param TransitionYear
#' @param SeedSurvival
#' @param SeedBankSize
#' @param SeedsPerFruit
#' @param n.iter
#'
#' @export

runPopulationModels <- function(ldf, cldf, SizeClass, TransitionYear, SeedSurvival, SeedBankSize, SeedsPerFruit, n.iter) {
	# prep data transition matrices, then analyze
	A <- list()
	for (i in 1:length(ldf)) {
		A[[i]] <- prepDataTransitionMatrix(
			ldf[[i]], 
			cldf[[i]], 
			SizeClass, 
			TransitionYear
		)
	}
	B <- list()
	for (i in 1:length(ldf)) {
		B[[i]] <- analyzeMatrixPopModels(
			trans_data = A[[i]],
			SizeClass = SizeClass,
			TransitionYear = TransitionYear,
			SeedSurvival = SeedSurvival,
			SeedBankSize = SeedBankSize,
			SeedsPerFruit = SeedsPerFruit,
			n.iter = n.iter
		)
	}
	return(B)
}