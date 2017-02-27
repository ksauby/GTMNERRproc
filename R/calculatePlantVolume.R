#' Calculate Plant Volume
#'
#' @description Calculate plant volume as a cone, cylinder, and elliptic cylinder.
#' @param Plant_Surveys Plant_Surveys Plant Survey Dataset
#'
#' @export

calculatePlantVolume <- function(Plant_Surveys) {
	Plant_Surveys %>% 
		mutate(
			Cone_t 				= pi * (((Width_t + Perpen_Width)/4)^2) * 
									Height_t / 3,
			Cylinder_t 			= pi * ((Perpen_Width/2)^2) * Width_t,
			Elliptic_Cylinder_t = pi * Height_t/2 * Perpen_Width/2 * Width_t
		)
}
