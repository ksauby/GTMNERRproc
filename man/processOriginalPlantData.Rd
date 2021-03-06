% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processOriginalPlantData.R
\name{processOriginalPlantData}
\alias{processOriginalPlantData}
\title{Process Original Plant Survey Data}
\usage{
processOriginalPlantData(Original_Plant_Data)
}
\description{
I collected this data in December 2012, then gave the plants new Plant IDs. This data cannot be used for the demography study but can be used to determine plot occupancy.
Steps:
\itemize{
 \item Fix column names
 \item format dates
 \item format PlantIDs
 \item format convert "999" values to NA
 \item format change "yes"/"no" values of the insect survey, missing, and dead columns to 0/1
 \item format numeric columns
 \item check for observations of 0 pads, 0 height, or 0 width; if there are observations, stop
	}
}

