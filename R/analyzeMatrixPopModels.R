#' Analyze Matrix Population Models
#'
#' @description Analyze multiple matrix population models, allowing for a seed bank and clonal reproduction.

#' @return Returns a list including:
#' \itemize{
#'  	\item the growth/retrogression/survival/fecundity transition matrix and clonal reproduction transition matrix for each matrix population model analysis
#'  	\item the results of the matrix population model analysis, including repro. values for each stage, the stable stage distribution, sensitivities, elasticities, lambda, the damping ratio, the seed bank size, the number of seeds assumed per fruit, and the seed survival rate (the rate at which seeds stay in the seed stage from one year to the next).
#' 	}
#'
#' @param trans_data dataset from which to create growth/retrogression/survival/fecundity transition matrix
#' @param SizeClass A vector containing the size breaks with which to create stages based on size.
#' @param TransitionYear
#' @param SeedSurvival Rate at which seeds survive (i.e., rate at which seeds remain in seed stage). Can be a single value or vector of values.
#' @param SeedBankSize Number of seeds in the seed bank (can be a single value or vector of values).
#' @param SeedsPerFruit Average number of seeds produced per fruit (can be a single value or vector of values).
#' @param clone_transition clonal reproduction transition matrix
#' @param SizeClass
#' @param n.iter Number of iterations for pop.projection function. Default value is 1000.

#' @export


analyzeMatrixPopModels <- function(
	trans_data,
	SizeClass,
	TransitionYear,
	SeedSurvival,
	SeedBankSize,
	SeedsPerFruit,
	clone_transition,
	n.iter=1000
) {
	A <- vector("list", length(SeedBankSize))	
	B <- vector("list", length(SeedBankSize))	
	for (i in 1:length(SeedBankSize)) {
		for (j in 1:length(SeedsPerFruit)) {
			for (k in 1:length(SeedSurvival)) {
				# ----------------------------------------------------------- #
				# Create growth/retrogression/survival/fecundity transition matrix
				# ----------------------------------------------------------- #
				proj_matrix <- createProjectionMatrix(
					trans_data,
					SeedBankSize[i],
					SeedsPerFruit[j],
					SeedSurvival[k]
				)
				# ------------------------------------------------------- #
				# add transition matrices together
				# ------------------------------------------------------- #
				all_proj_matrix <- proj_matrix + clone_transition
				# ------------------------------------------------------- #
				# starting numbers
				# ------------------------------------------------------- #
				n_per_stage <- NULL
				n_per_stage <- calculateNumberIndivperStage(trans_data)
				# remove NA class
				n_per_stage <- n_per_stage[1:length(stages),]
				n_per_stage <- n_per_stage$n
				n_per_stage <- n_per_stage[1:(length(n_per_stage) - 1)]
				# ------------------------------------------------------- #
				# dynamics
				# ------------------------------------------------------- #
				# growth/retrogression/survival/fecundity transition matrix
				B[[i]][[j]]$projection_matrix <- proj_matrix
				# clonal reproduction transition matrix
				B[[i]][[j]]$clone_transition <- clone_transition
			
			
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
						Name = paste(
							"elasticities.", 
							Var2, "-", 
							Var1, 
							sep=""
						)
					) %>%
					as.data.frame(row.names=.$Name) %>%
					dplyr::select(Freq) %>%
					t %>%
					as.data.frame()
				#		sensitivities
				sensitivities <- analysis.results$sensitivities %>% 
					as.table %>% as.data.frame %>%
					mutate(
						Name = paste(
							"sensitivities.", 
							Var2, "-",
							Var1, 
							sep=""
						)
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
			A[[i]][[j]] <- do.call(rbind.data.frame, A[[i]][[j]])
		}
		A[[i]] <- do.call(rbind.data.frame, A[[i]])
	}
	A <- do.call(rbind.data.frame, A)
	return(list(transitionmatrices=B,resultstables=A))
}
