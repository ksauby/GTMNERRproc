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
			cat("\\subsubsection{ ``", parent.methods[j], "'' parent assignment method}", sep="")
			cat('\n')  
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
				include.rownames=F, caption.placement = "top"
			)
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
				include.rownames=F, caption.placement = "top"
			)
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
#' @export


printLTREPopulationMatrices <- function(data.list, parent.method, years, first.year, LTRE_variable) {
	for (i in 1:length(data.list)) {
		dat <- eval(parse(text=data.list[i]))
		cat("\\subsubsection{", dat$N.Stages.wo.break, "}", sep="")
		for (j in 1:length(parent.methods)){
			cat('\n')  
			print(
				xtable(
					dat[[4]][[j]]$clone_transition_counts, 
					digits=0, 
					caption=paste(
						"Clone production counts (",
						years,
						" data for plants ",
						LTRE_variable,
						") using the \`\`", 
						parent.methods[j],
						"\'\' parent assignment method.",
						sep=""
					)
				),
				include.rownames=F, caption.placement = "top"
			)
			print(
				xtable(
					dat[[4]][[j]]$transition.counts, 
					digits=0,
					caption=paste(
						"Growth, stasis, retrogression, and survival counts (",
						years,
						" data for plants ",
						LTRE_variable,
						") using the \`\`", 
						parent.methods[j],
						"\'\' parent assignment method.",
						sep=""
					)
				),
				include.rownames=F, caption.placement = "top"
			)
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
