#' Analyze Matrix Population Models
#'
#' @description Analyze multiple matrix population models, allowing for a seed bank and clonal reproduction.

#' @return Returns a list including:
#' \itemize{
#'  	\item the growth/retrogression/survival/fecundity transition matrix and clonal reproduction transition matrix for each matrix population model analysis
#'  	\item the results of the matrix population model analysis, including repro. values for each stage, the stable stage distribution, sensitivities, elasticities, lambda, the damping ratio, the seed bank size, the number of seeds assumed per fruit, and the seed survival rate (the rate at which seeds stay in the seed stage from one year to the next).
#' 	}
#'
#' @param Dat dataset from which to create growth/retrogression/survival/fecundity transition matrix
#' @param SizeClass A vector containing the size breaks with which to create stages based on size.
#' @param TransitionYear
#' @param SeedSurvival Rate at which seeds survive (i.e., rate at which seeds remain in seed stage). Can be a single value or vector of values.
#' @param SeedBankSize Number of seeds in the seed bank (can be a single value or vector of values).
#' @param SeedsPerFruit Average number of seeds produced per fruit (can be a single value or vector of values).
#' @param clonal_repro_dataset dataset from which to create clonal reproduction transition matrix. Each row represents information for a unique offspring. Each row contains the identity of the parent, the year, the size of the offspring, and the size of the parent.
#' @param SizeClass
#' @param n.iter Number of iterations for pop.projection function. Default value is 1000.

#' @export


analyzeMatrixPopModels <- function(
	Dat,
	SizeClass,
	TransitionYear,
	SeedSurvival,
	SeedBankSize,
	SeedsPerFruit,
	clonal_repro_dataset,
	n.iter=1000
) {
	A <- vector("list", length(SeedBankSize))	
	B <- vector("list", length(SeedBankSize))	
	for (i in 1:length(SeedBankSize)) {
		for (j in 1:length(SeedsPerFruit)) {
			# ---------------------------------------------------------------- #
			# Create growth/retrogression/survival/fecundity transition matrix
			# ---------------------------------------------------------------- #
			TMdata <- prepDataTransitionMatrix(
				Dat,
				SizeClass,
				TransitionYear,
				SeedSurvival,
				SeedBankSize[i],
				SeedsPerFruit[j]
			)
			stages = TMdata[[3]][-length(TMdata[[3]])]
			proj_matrix <- projection.matrix(
				TMdata[[1]], 
				add = c(
					# transition from seed to seedling
					2, 1, TMdata[[2]]
				),
				sort = stages
			)
			# ---------------------------------------------------------------- #
			# Create clonal reproduction transition matrix
			# ---------------------------------------------------------------- #
			# get from calculateClonalReproduction function
			clonal_repro_dataset_mod <- clonal_repro_dataset %>% 
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
				clonal_repro_dataset_mod$offspring_stage, 
				clonal_repro_dataset_mod$parent_stage
			) %>% 
				as.data.frame.matrix %>% 
				data.matrix
			# number of parents per stage
			n_per_stage <- TMdata[[1]] %>%
				group_by(stage) %>%
				summarise(n = length(PlantID)) %>% 
				as.data.frame %>%
				filter(stage %in% colnames(clone_table))
			n_per_stage <- n_per_stage[,2]
			# clone transition matrix = number of clones per parent
			clone_transition = clone_table / n_per_stage
			# make the clone transition matrix the same dimensions as growth/retrogression/survival/fecundity transition matrix
			fill = matrix(
				0, length(stages), 3, 
				dimnames=list(
					rownames(proj_matrix), 
					colnames(proj_matrix)[1:3])
			)
			clone_transition <- merge(
				fill, 
				clone_transition, 
				by="row.names", 
				all=T)
			rownames(clone_transition) <- clone_transition$Row.names
			clone_transition[is.na(clone_transition)] <- 0
			# reorder rows
			clone_transition <- 
				clone_transition[match(stages,clone_transition$Row.names), ] %>%
				.[, -1] %>% 
				as.matrix(rownames="Row.names")

			# ---------------------------------------------------------------- #
			# add transition matrices together
			# ---------------------------------------------------------------- #
			all_proj_matrix <- proj_matrix + clone_transition
			# ---------------------------------------------------------------- #
			# starting numbers
			# ---------------------------------------------------------------- #
			n_per_stage <- NULL
			n_per_stage <- TMdata[[1]] %>%
				group_by(stage) %>%
				summarise(n = length(PlantID))
			# remove NA class
			n_per_stage <- n_per_stage[1:length(stages),]
			n_per_stage <- n_per_stage$n
			n_per_stage <- n_per_stage[1:(length(n_per_stage) - 1)]
			# ---------------------------------------------------------------- #
			# dynamics
			# ---------------------------------------------------------------- #
			# growth/retrogression/survival/fecundity transition matrix
			B[[i]][[j]]$projection_matrix <- proj_matrix
			# clonal reproduction transition matrix
			B[[i]][[j]]$clone_transition <- clone_transition
			for (k in 1:length(SeedSurvival)) {
				all_proj_matrix[1, 1] <- SeedSurvival[k]
				n <- c(SeedBankSize[i], n_per_stage)
				pr <- pop.projection(all_proj_matrix, n, iterations=n.iter)
				analysis.results <- eigen.analysis(all_proj_matrix)
				
				# create table of results				
				#		repro.values
				repro.value.dat <- analysis.results$repro.value
				names(repro.value.dat) <- 
					paste("repro.value",names(repro.value.dat),sep=".")
				repro.value.dat %<>% t %>% as.data.frame
				#		stable stage distribution
				stable.stage.dat <- analysis.results$stable.stage
				names(stable.stage.dat) <- 
					paste("stable.stage",names(stable.stage.dat),sep=".")
				stable.stage.dat %<>% t %>% as.data.frame
				#		elasticities
				elasticities <- analysis.results$elasticities %>% 
					as.data.frame %>%
					mutate(
						Name = paste("elasticities.", x$y, "-", x$x, sep="")
					) %>%
					as.data.frame(row.names=.$Name) %>%
					dplyr::select(Freq) %>%
					t %>%
					as.data.frame()
				#		sensitivities
				sensitivities <- analysis.results$sensitivities %>% 
					as.table %>% as.data.frame %>%
					mutate(
						Name = paste("sensitivities.", x$y, "-", x$x, sep="")
					) %>%
					as.data.frame(row.names=.$Name) %>%
					dplyr::select(Freq) %>%
					t %>%
					as.data.frame()
				A[[i]][[j]][[k]] <- data.frame(
					repro.value.dat,
					stable.stage.dat,
					sensitivities,
					elasticities,
					lambda1 		= analysis.results$lambda1,
					damping.ratio 	= analysis.results$damping.ratio,
					SeedBankSize 	= SeedBankSize[i],
					SeedsPerFruit 	= SeedsPerFruit[j],
					SeedSurvival 	= SeedSurvival[k]
				)				
			}
		}
	}
	return(list(transitionmatrices=B,resultstables=A))
}
