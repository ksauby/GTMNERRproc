#' Create Clonal Reproduction Transition Matrix
#'

#' @param clonal_repro_dat This dataset is created by the calculateClonalReproduction function. Each row represents information for a unique offspring. Each row contains the identity of the parent, the year, the size of the offspring, and the size of the parent.
#' @param TMdata Survey data used to create transition matrix.

#' @return Returns a list in which the first element is the count of the number of individuals in each stage-stage transition and the second element contains the rates at which individuals transition among stages.

#' @export

createClonalReproTransitionMatrix <- function(
	clonal_repro_dat, 
	TMdata, 
	stages
) {
	clonal_repro_dat_mod <- clonal_repro_dat %>% 
		rowwise() %>%
		mutate(
			parent_stage=cut(
				SizewClones_t, 
				SizeClass, 
				include.lowest=T, 
				labels=FALSE
			),
			offspring_stage=cut(
				Offspring.First_Size, 
				SizeClass, 
				include.lowest=T, 
				labels=FALSE
			)
		)
	clone_table <- table(
		clonal_repro_dat_mod$offspring_stage, 
		clonal_repro_dat_mod$parent_stage
	) %>% 
		as.data.frame.matrix %>% 
		data.matrix
	# number of parents per stage
	n_per_stage <- calculateNumberIndivperStage(TMdata, stages)
	# make the clone transition matrix the same dimensions as growth/retrogression/survival/fecundity transition matrix
	fill = matrix(
		0, length(stages), 3, 
		dimnames=list(
			stages, 
			stages[1:3])
	)
	clone_table <- merge(
		fill, 
		clone_table, 
		by="row.names", 
		all=T)
	rownames(clone_table) <- clone_table$Row.names
	clone_table[is.na(clone_table)] <- 0
	# reorder rows
	clone_table %<>% .[match(stages, .$Row.names), ] %>%
		.[, -1] %>% 
		as.matrix(rownames="Row.names")
	# make sure I have all of the columns that I need
	if (length(stages[!(stages %in% colnames(clone_table))]) > 0) {
		fill = matrix(
			0, 
			length(stages), 
			length(stages[!(stages %in% colnames(clone_table))]), 
			dimnames=list(
				stages, 
				stages[!(stages %in% colnames(clone_table))]
			)
		)
		clone_table <- merge(
			fill, 
			clone_table, 
			by="row.names", 
			all=T)
		rownames(clone_table) <- clone_table$Row.names
		clone_table[is.na(clone_table)] <- 0
	}	
	# reorder rows
	clone_table %<>% .[stages, stages]
	# function output
	clone_transition_rates <- calculateTransitionRates(
		clone_table, 
		as.vector(n_per_stage$n)
	)
	clone_transition_counts <- clone_table
	return(list(
		clone_transition_counts = clone_transition_counts, 
		clone_transition_rates = clone_transition_rates,
		n_per_stage = n_per_stage
	))	
}
	
