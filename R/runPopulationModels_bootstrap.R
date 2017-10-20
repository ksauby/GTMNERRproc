#' Run Boostrapped Population Models
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

runBootstrapPopulationModels <- function(ldf, cldf, SizeClass, TransitionYear, SeedSurvival, SeedBankSize, SeedsPerFruit, n.iter, n_bootstraps) {
	# prep data transition matrices, then analyze
	SizeClass <- SizeClass
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
		B[[i]] <- analyzeBootstrappedMatrixPopModels(
			trans_data = A[[i]],
			clonal_repro_dataset = cldf[[i]],
			SizeClass,
			TransitionYear,
			SeedSurvival,
			SeedBankSize,
			SeedsPerFruit,
			n.iter,
			n_bootstraps
		)
	}
	return(list(
		transition_matrix_data = A,
		population_modeling_results = B
	))
}