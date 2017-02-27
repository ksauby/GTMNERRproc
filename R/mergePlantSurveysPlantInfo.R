#' Merge Plant Info and Plant Surveys data files
#'
#' @description Add Island, Cluster, and Host Species information to the Plant_Surveys dataset.
#' @param Plant_Surveys Plant Survey Dataset
#' @param Plant_Info  Plant Information Dataset
#'
#' @export

mergePlantSurveysPlantInfo <- function(Plant_Surveys, Plant_Info) {
	Plant_Surveys <- Plant_Info %>%
		dplyr::select(
			ClusterID, 
			InDemographicStudy, 
			PlotPlantID, 
			Tag_Number, 
			Island, 
			Cluster2, 
			Network, 
			Species, 
			InBigPlantStudy, 
			N.PlotPlantIDs, 
			Easting, 
			Northing,
			RecruitmentMode
		) %>%
		merge(Plant_Surveys, by = "PlotPlantID")
	return(Plant_Surveys)
}
