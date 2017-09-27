#' Successively print population matrices for inclusion in an appendix
#'
#' @param data.list
#' @param parent.method
#' @param  years
#' @param first.year
#'
#' @export

printPopulationMatrices <- function(data.list, parent.method, years, first.year) {
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
				caption="Plants that produced fruit in fecundity-year z."
			)
			names(temp) <- c("PlantID", "Stage, z", "Reproduction, z", "Stage, z+1", "Reproduction, z+1")
			print(
				temp,
				include.rownames=F, caption.placement = "top"
			)
			# CLONE TRANSITION COUNTS	
			print(
				xtable(
					dat[[1]][[j]]$clone_transition_counts, 
					digits=0,
					caption=paste(
						"Clone production counts (",
						years,
						" data) using the \`\`", 
						parent.methods[j],
						"\'\' parent assignment method.",
						sep=""
					)
				),
				caption.placement = "top"
			)
			# GROWTH, STATIS, RETROGRESSION, SURVIVAL
			print(
				xtable(
					dat[[1]][[j]]$transition.counts, 
					digits=0,
					caption=paste(
						"Growth, stasis, retrogression, and survival counts (",
						years,
						" data) using the \`\`",
						parent.methods[j],
						"\'\' parent assignment method.",
						sep=""
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


printLTREPopulationMatrices <- function(data.list, parent.method, years, first.year, LTRE_variable, LTRE_variable_section_heading) {
	for (i in 1:length(data.list)) {
		dat <- eval(parse(text=data.list[i]))
		cat("\\subsubsection{", LTRE_variable_section_heading, "}", sep="")
		for (j in 1:length(parent.methods)){
			cat('\n')  
			# PLANTS WITH FRUIT
			if (
				(
					dat[[4]][[j]]$trans01 %>% 
						filter(Repro2 > 0) %$% 
						dim(.)[1]
				) > 0 
			) {
				temp <- xtable(
					dat[[4]][[j]]$trans01 %>% filter(Repro > 0), 
					digits=0,
					caption="Plants that produced fruit in fecundity-year z."
				)
				names(temp) <- c("PlantID", "Stage, z", "Reproduction, z", "Stage, z+1", "Reproduction, z+1")
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
					caption=paste(
						"Clone production counts using the \`\`", 
						parent.methods[j],
						"\'\' parent assignment method for the ",
						years,
						" data for plants ",
						LTRE_variable,
						".",
						sep=""
					)
				),
				caption.placement = "top"
			)
			# GROWTH, STATIS, RETROGRESSION, SURVIVAL
			print(
				xtable(
					dat[[4]][[j]]$transition.counts, 
					digits=0,
					caption=paste(
						"Growth, stasis, retrogression, and survival counts using the \`\`", 
						parent.methods[j],
						"\'\' parent assignment method for the ",
						years,
						" data for plants ",
						LTRE_variable,
						".",
						sep=""
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
						", using the \`\`", 
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
