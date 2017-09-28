#' Successively print population matrices for inclusion in an appendix
#'
#' @param data.list
#' @param parent.method
#' @param  years
#' @param first.year
#'
#' @export

printPopulationMatrices <- function(data.list, parent.method, years, first.year, second.year) {
	for (i in 1:length(data.list)) {
		dat <- eval(parse(text=data.list[i]))
		cat("\\subsection{", dat$N.Stages.wo.break, "}", sep="")
		for (j in 1:length(parent.methods)){
			cat("\\subsubsection{ ``", toTitleCase(parent.methods[j]), "'' parent assignment method}", sep="")
			cat('\n')  
			# PLANTS WITH FRUIT
			temp <- xtable(
				dat[[1]][[j]]$trans01 %>% filter(Repro > 0), 
				digits=0,
				caption=paste(
					"Plants that produced fruit in fecundity-year ",
					first.year,
					".",
					sep=""
				)
			)
			names(temp) <- c(
				"PlantID", 
				paste("Stage,",first.year), 
				paste("Reproduction,",first.year), 
				paste("Stage",second.year), 
				paste("Reproduction",second.year)
			)
			print(
				temp,
				include.rownames=F, caption.placement = "top"
			)
			# CLONE TRANSITION COUNTS	
			print(
				xtable(
					dat[[1]][[j]]$clone_transition_counts, 
					digits=0,
					caption=c(
						paste(
							"Clone production counts (",
							years,
							" data) using the \`\`", 
							parent.methods[j],
							"\'\' parent assignment method. Columns represent the stages parent plants were in during fecundity-year $z$ and the rows represent the stages of clonal offspring produced in fecundity-year $z+1$."
						),
						paste(
							"Clone production counts (",
							years,
							" data) using the \`\`", 
							parent.methods[j],
							"\'\' parent assignment method."
						)
					)
				),
				caption.placement = "top"
			)
			# GROWTH, STATIS, RETROGRESSION, SURVIVAL
			print(
				xtable(
					dat[[1]][[j]]$transition.counts, 
					digits=0,
					caption=c(
						paste(
							"Growth, stasis, retrogression, and survival counts (",
							years,
							" data) using the \`\`", 
							parent.methods[j],
							"\'\' parent assignment method. Columns represent the stages plants were in during fecundity-year $z$ and the rows represent the stages the plants transitioned to in fecundity-year $z+1$."
						),
						paste(
							"Growth, stasis, retrogression, and survival counts (",
							years,
							" data) using the \`\`", 
							parent.methods[j],
							"\'\' parent assignment method."
						)
					)
				),
				caption.placement = "top"
			)
			# NUMBER OF INDIVIDUALS PER STAGE
			print(
				xtable(
					dat[[1]][[j]]$n_per_stage %>% filter(stage!="Seed"), 
					digits=0,
					caption=paste(
						"Number of individuals per stage in ",
						first.year,
						" using the \`\`",
						parent.methods[j],
						"\'\' parent assignment method.",
						sep=""
					)
				),
				include.rownames=F, caption.placement = "top"
			)
			cat("\\clearpage")
		}
	}
}


#' Successively print LTRE population matrices for inclusion in an appendix
#'
#' @param data.list
#' @param parent.method
#' @param  years
#' @param first.year
#' @param LTRE_variable
#'
#' @importFrom tools toTitleCase
#' @export


printLTREPopulationMatrices <- function(data.list, parent.method, years, first.year, second.year, LTRE_variable, LTRE_variable_section_heading) {
	for (i in 1:length(data.list)) {
		dat <- eval(parse(text=data.list[i]))
		cat("\\subsubsection{", LTRE_variable_section_heading, "}", sep="")
		for (j in 1:length(parent.methods)){
			cat('\n')  
			# PLANTS WITH FRUIT
			if (
				(
					dat[[4]][[j]]$trans01 %>% 
						filter(Repro > 0) %$% 
						dim(.)[1]
				) > 0 
			) {
				temp <- xtable(
					dat[[4]][[j]]$trans01 %>% filter(Repro > 0), 
					digits=0,
					caption=paste(
						"Plants that produced fruit in fecundity-year ",
						first.year,
						".",
						sep=""
					)
				)
				names(temp) <- c(
					"PlantID", 
					paste("Stage,",first.year), 
					paste("Reproduction,",first.year), 
					paste("Stage",second.year), 
					paste("Reproduction",second.year)
				)
				print(
					temp,
					include.rownames=F, caption.placement = "top"
				)	
			}
			
			# CLONE TRANSITION COUNTS	
			print(
				xtable(
					dat[[4]][[j]]$clone_transition_counts, 
					digits=0, 
					caption=c(
						paste(
							"Clone production counts using the \`\`", 
							parent.methods[j],
							"\'\' parent assignment method for the ",
							years,
							" data for plants ",
							LTRE_variable,
							" at some point during the study. Columns represent the stages parent plants were in during fecundity-year $z$ and the rows represent the stages of clonal offspring produced in fecundity-year $z+1$.",
							sep=""
						),
						paste(
							"Clone production counts using the \`\`", 
							parent.methods[j],
							"\'\' parent assignment method for the ",
							years,
							" data for plants ",
							LTRE_variable,
							" at some point during the study.",
							sep=""
						)
					)
				),
				caption.placement = "top"
			)
			# GROWTH, STATIS, RETROGRESSION, SURVIVAL
			print(
				xtable(
					dat[[4]][[j]]$transition.counts, 
					digits=0,
					caption=c(
						paste(
							"Growth, stasis, retrogression, and survival counts using the \`\`", 
							parent.methods[j],
							"\'\' parent assignment method for the ",
							years,
							" data for plants ",
							LTRE_variable,
							" at some point during the study. Columns represent the stages plants were in during fecundity-year $z$ and the rows represent the stages the plants transitioned to in fecundity-year $z+1$.",
							sep=""
						),
						paste(
							"Growth, stasis, retrogression, and survival counts using the \`\`", 
							parent.methods[j],
							"\'\' parent assignment method for the ",
							years,
							" data for plants ",
							LTRE_variable,
							" at some point during the study.",
							sep=""
						)
					)
				),
				caption.placement = "top"
			)
			# NUMBER OF INDIVIDUALS PER STAGE
			print(
				xtable(
					dat[[4]][[j]]$n_per_stage %>% filter(stage!="Seed"), 
					digits=0,
					caption=paste(
						"Number of individuals per stage in ",
						first.year,
						" for plants ",
						LTRE_variable,
						" at some point during the study, using the \`\`", 
						parent.methods[j],
						"\'\' parent assignment method.",
						sep=""
					)
				),
				include.rownames=F, caption.placement = "top"
			)
			cat("\\clearpage")
		}
	}
}
