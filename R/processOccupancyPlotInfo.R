#' Process Plot Survey Data
#'
#' @description Steps:
#' \itemize{
#'  \item fix Cluster variable name
#'  \item include "In Demography Study" information
#'  \item add vegetation information
#' \itemize{
	#'  \item VegDensity
	#'  \item Canopy
	#'  \item Detritus
#' }
#'  \item add elevation, distance to water information
#' \itemize{
	#'  \item elevation
	#'  \item dist_to_water
#'  }
#'  \item limit to only plots still in study
#'  \item make coordinates numeric
#' 	}
#'
#' @export

processOccupancyPlotInfo <- function(Plot_Info, Plot.Vegetation, GIS_data_updated) {
	#--------------------------------------- Misc
	# fix Cluster name
	"Cluster" -> Plot_Info$Sampling[which(Plot_Info$Sampling=="Clusters")]
	Plot_Info$Island %<>% as.factor
	# file with ALL plots
	# Plot_Info_All <- Plot_Info
	#--------------------------------- include "In Demography Study" information
	Plot_Info %<>% merge(ClustersInDemographicStudy, by = "Cluster", all=T)
	# change NA to "no"
	Plot_Info$InDemographicStudy[which(is.na(Plot_Info$InDemographicStudy))] <- "no"
	# ---------------------------------------- ADD VEGETATION INFO ----------- #
	# has two records for 1572
	Plot_Info %<>% merge(Plot.Vegetation, by="Tag_Number", all=T)
	# ---------------------------------------- ADD GIS INFO ------------------ #
	# GIS_data_updated has remote sensing information, including elevation, dist_water
	Plot_Info %<>% merge(GIS_data_updated, by="Tag_Number", all=T)
	# limit to only plots still in study
	Plot_Info %<>% filter(RemovedFromStudy!="Yes")
	# make coordinates numeric
	Plot_Info[,c(
		"Easting", 
		"Northing")] %<>% 
		apply(., 2, as.numeric
	)
	return(Plot_Info)	
}