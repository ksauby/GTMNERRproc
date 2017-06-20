#' Fix Surveys for Plants with Inconsistent Records of Dead/Missing
#'
#' @description Process Plant_Surveys
#' @param Plant_Surveys Dataset with Plant Surveys
#'
#' @export

fixInconsistentDeadMissingRecords <- function(Plant_Surveys) {
	Plant_Surveys %>%
	mutate(
		# 7226: photos look the same so I think dead recordings are erroneous - should be marked as alive on 2014-09-13 and 2015-01-28
		Dead = replace(
			Dead,
			which(PlotPlantID=="7226" & Date=="2014-09-13"),
			0
		),
		Dead = replace(
			Dead,
			which(PlotPlantID=="7226" & Date=="2015-01-28"),
			0
		),
		# 7351: photos look the same so I think dead/missing recordings are erroneous; should be alive on 2014-09-08 and 2015-01-16
		Dead = replace(
			Dead,
			which(PlotPlantID=="7351" & Date=="2014-09-08"),
			0
		),
		Dead = replace(
			Dead,
			which(PlotPlantID=="7351" & Date=="2015-01-16"),
			0
		),
		# # 7396: mark as missing 2015-01-20 bc covered in ant mound
		Missing = replace(
			Missing,
			which(PlotPlantID=="7396" & Date=="2015-01-20"),
			1
		),
		# 7397: mark as missing 2015-01-20 and 2015-06-03
		Missing = replace(
			Missing,
			which(PlotPlantID=="7397" & Date=="2015-01-20"),
			1
		),
		Missing = replace(
			Missing,
			which(PlotPlantID=="7397" & Date=="2015-06-03"),
			1
		),
		# 8079: assume missing 2013-12-20 as well
		Missing = replace(
			Missing,
			which(PlotPlantID=="8079" & Date=="2013-12-20"),
			1
		),
		# 8418: assume it was dead on 2015-01-16
		Dead = replace(
			Dead,
			which(PlotPlantID=="8418" & Date=="2015-01-16"),
			1
		),
		# 8493: I guess it was buried on 2014-06-27 and 2015-01-21 (should be alive)
		Dead = replace(
			Dead,
			which(PlotPlantID=="8493" & Date=="2014-06-27"),
			0
		),
		Dead = replace(
			Dead,
			which(PlotPlantID=="8493" & Date=="2015-01-21"),
			0
		),
		# 8516: assume it was alive on 2013-12-20
		Dead = replace(
			Dead,
			which(PlotPlantID=="8516" & Date=="2013-12-20"),
			0
		),
		# 8643: assume it wasnt dead on  2013-12-20
		Dead = replace(
			Dead,
			which(PlotPlantID=="8643" & Date=="2013-12-20"),
			0
		),
		# 8709: must have been alive 2013-12-14
		Dead = replace(
			Dead,
			which(PlotPlantID=="8709" & Date=="2013-12-14"),
			0
		),
		# 8864: assume it was dead 2015-01-22
		Dead = replace(
			Dead,
			which(PlotPlantID=="8864" & Date=="2015-01-22"),
			1
		),
		# 8955: assume dead on 2014-06-28
		Dead = replace(
			Dead,
			which(PlotPlantID=="8955" & Date=="2014-06-28"),
			1
		),
		# 9064: assume alive on 2014-01-10
		Dead = replace(
			Dead,
			which(PlotPlantID=="9064" & Date=="2014-01-10"),
			0
		),
		# 9156: incorrectly marked dead on 2014-01-13 and 2014-06-10
		Dead = replace(
			Dead,
			which(PlotPlantID=="9156" & Date=="2014-01-13"),
			0
		),
		Dead = replace(
			Dead,
			which(PlotPlantID=="9156" & Date=="2014-06-10"),
			0
		),
		# 9347: still alive on 2014-05-10 and 2014-09-04
		Dead = replace(
			Dead,
			which(PlotPlantID=="9347" & Date=="2014-05-10"),
			0
		),
		Dead = replace(
			Dead,
			which(PlotPlantID=="9347" & Date=="2014-09-04"),
			0
		),
		# 9415: just call it dead on 2014-09-06
		Dead = replace(
			Dead,
			which(PlotPlantID=="9415" & Date=="2014-09-06"),
			1
		),
		# 9416: assume dead on 2015-01-09
		Dead = replace(
			Dead,
			which(PlotPlantID=="9416" & Date=="2015-01-09"),
			1
		),
		# 9481: was alive on 2014-07-15 and 2015-02-19
		Dead = replace(
			Dead,
			which(PlotPlantID=="9481" & Date=="2014-07-15"),
			0
		),
		Dead = replace(
			Dead,
			which(PlotPlantID=="9481" & Date=="2015-02-19"),
			0
		),
		# 9648: assume dead on 2015-01-17
		Dead = replace(
			Dead,
			which(PlotPlantID=="9648" & Date=="2015-01-17"),
			1
		),
		# 9712: assume dead on 2014-01-06
		Dead = replace(
			Dead,
			which(PlotPlantID=="9712" & Date=="2014-01-06"),
			1
		),
		# 9732: assume was alive on 2013-07-23 and 2013-12-17
		Dead = replace(
			Dead,
			which(PlotPlantID=="9732" & Date=="2013-07-23"),
			0
		),
		Dead = replace(
			Dead,
			which(PlotPlantID=="9732" & Date=="2013-12-17"),
			0
		),
		# 9748: assume alive 2013-07-23 and 2013-12-17
		Dead = replace(
			Dead,
			which(PlotPlantID=="9748" & Date=="2013-07-23"),
			0
		),
		Dead = replace(
			Dead,
			which(PlotPlantID=="9748" & Date=="2013-12-17"),
			0
		),
		# 9859: assume alive 2013-12-18
		Dead = replace(
			Dead,
			which(PlotPlantID=="9859" & Date=="2013-12-18"),
			0
		),
		# 9869: assume dead 2013-12-18
		Dead = replace(
			Dead,
			which(PlotPlantID=="9869" & Date=="2013-12-18"),
			1
		),
		# 9923: assume alive 2013-12-18, 2014-06-26, 2015-01-20
		Dead = replace(
			Dead,
			which(PlotPlantID=="9923" & Date=="2013-12-18"),
			0
		),
		Dead = replace(
			Dead,
			which(PlotPlantID=="9923" & Date=="2014-06-26"),
			0
		),
		Dead = replace(
			Dead,
			which(PlotPlantID=="9923" & Date=="2015-01-20"),
			0
		),
		# 9949: assume dead on 2013-12-18
		Dead = replace(
			Dead,
			which(PlotPlantID=="9949" & Date=="2013-12-18"),
			1
		),
		# 9967: assume alive on 2013-12-18
		Dead = replace(
			Dead,
			which(PlotPlantID=="9967" & Date=="2013-12-18"),
			0
		),
		# 8202: 
		# 7130: replace missing on 2015-01-21 with 0
		Missing = replace(
			Missing,
			which(PlotPlantID=="7130" & Date=="2015-01-21"),
			0
		),
		# 7152: replace missing on 2015-01-21 with 0
		Missing = replace(
			Missing,
			which(PlotPlantID=="7152" & Date=="2015-01-21"),
			0
		),
		# 7165: replace dead on 2015-01-20 with 0
		Dead = replace(
			Dead,
			which(PlotPlantID=="7165" & Date=="2015-01-20"),
			0
		),
		# 7168: replace dead on 2015-01-08 with 0
		Dead = replace(
			Dead,
			which(PlotPlantID=="7168" & Date=="2015-01-08"),
			0
		),
		# 7174: replace dead on 2015-01-08 with 0
		Dead = replace(
			Dead,
			which(PlotPlantID=="7174" & Date=="2015-01-08"),
			0
		),
		# 7192: replace dead on 2015-01-08 with 0
		Dead = replace(
			Dead,
			which(PlotPlantID=="7192" & Date=="2015-01-08"),
			0
		),
		# 7333: replace missing on 2015-01-15 with 0
		Missing = replace(
			Missing,
			which(PlotPlantID=="7333" & Date=="2015-01-15"),
			0
		),
		# 8013: replace dead on 2014-09-05 with 0
		Dead = replace(
			Dead,
			which(PlotPlantID=="8013" & Date=="2014-09-05"),
			0
		),
		# 8068: replace dead on 2015-01-15 with 0
		Dead = replace(
			Dead,
			which(PlotPlantID=="8068" & Date=="2015-01-15"),
			0
		),
		# 8077: replace dead on 2014-06-28 with 0
		Dead = replace(
			Dead,
			which(PlotPlantID=="8077" & Date=="2014-06-28"),
			0
		),
		# 8088: replace missing on 2014-06-04 with 0
		Missing = replace(
			Missing,
			which(PlotPlantID=="8088" & Date=="2014-06-04"),
			0
		),
		# 8126: replace missing on 2015-01-15 with 0
		Missing = replace(
			Missing,
			which(PlotPlantID=="8126" & Date=="2015-01-15"),
			0
		),
		# 8148: replace missing on 2014-06-28 with 0
		Missing = replace(
			Missing,
			which(PlotPlantID=="8148" & Date=="2014-06-28"),
			0
		),
		# 8168: replace dead on 2013-12-21 with 0
		Dead = replace(
			Dead,
			which(PlotPlantID=="8168" & Date=="2013-12-21"),
			0
		),
		# 8265: replace dead on 2015-01-16 with 0
		Dead = replace(
			Dead,
			which(PlotPlantID=="8265" & Date=="2015-01-16"),
			0
		),
		# 8270: replace dead on 2014-09-08 with 0
		Dead = replace(
			Dead,
			which(PlotPlantID=="8270" & Date=="2014-09-08"),
			0
		),
		# 8374: replace dead on 2015-01-20 with 0
		Dead = replace(
			Dead,
			which(PlotPlantID=="8374" & Date=="2015-01-20"),
			0
		),
		# 8403: replace missing on 2014-09-09 with 0
		Missing = replace(
			Missing,
			which(PlotPlantID=="8403" & Date=="2014-09-09"),
			0
		),
		# 8458: replace dead on 2013-12-21 with 0
		Dead = replace(
			Dead,
			which(PlotPlantID=="8458" & Date=="2013-12-21"),
			0
		),
		# 8487: replace dead on 2014-01-10 with 0
		Dead = replace(
			Dead,
			which(PlotPlantID=="8487" & Date=="2014-01-10"),
			0
		),
		# 8508: replace dead on 2013-07-29 with 0
		Dead = replace(
			Dead,
			which(PlotPlantID=="8508" & Date=="2013-07-29"),
			0
		),
		# 8555: replace dead on 2014-01-12 with 0
		Dead = replace(
			Dead,
			which(PlotPlantID=="8555" & Date=="2014-01-12"),
			0
		),
		# 8584: replace dead on 2015-01-28 with 0
		Dead = replace(
			Dead,
			which(PlotPlantID=="8584" & Date=="2015-01-28"),
			0
		),
		# 8652: replace dead on 2015-01-28 with 0
		Dead = replace(
			Dead,
			which(PlotPlantID=="8652" & Date=="2015-01-28"),
			0
		),
		# 8658: replace dead on 2015-01-08 with 0
		Dead = replace(
			Dead,
			which(PlotPlantID=="8658" & Date=="2015-01-08"),
			0
		),
		# 8673: replace dead on 2013-12-14 with 0
		Dead = replace(
			Dead,
			which(PlotPlantID=="8673" & Date=="2013-12-14"),
			0
		),
		# 8680: replace missing on 2015-02-03 with 0
		Missing = replace(
			Missing,
			which(PlotPlantID=="8680" & Date=="2015-02-03"),
			0
		),
		# 8735: replace dead on 2013-12-15 with 0
		Dead = replace(
			Dead,
			which(PlotPlantID=="8735" & Date=="2013-12-15"),
			0
		),
		# 8781: replace dead on 2015-01-17 with 0
		Dead = replace(
			Dead,
			which(PlotPlantID=="8781" & Date=="2015-01-17"),
			0
		),
		# 8826: replace missing on 2015-01-21 with 0
		Missing = replace(
			Missing,
			which(PlotPlantID=="8826" & Date=="2015-01-21"),
			0
		),
		# 8861: replace missing on 2015-01-20 with 0
		Missing = replace(
			Missing,
			which(PlotPlantID=="8861" & Date=="2015-01-20"),
			0
		),
		# 8911: replace dead on 2015-01-15 with 0
		Dead = replace(
			Dead,
			which(PlotPlantID=="8911" & Date=="2015-01-15"),
			0
		),
		# 8927: replace missing on 2015-01-15 with 0
		Missing = replace(
			Missing,
			which(PlotPlantID=="8927" & Date=="2015-01-15"),
			0
		),
		# 8951: replace dead on 2015-01-16 with 0
		Dead = replace(
			Dead,
			which(PlotPlantID=="8951" & Date=="2015-01-16"),
			0
		),
		# 8980: replace dead on 2015-01-16 with 0
		Dead = replace(
			Dead,
			which(PlotPlantID=="8980" & Date=="2015-01-16"),
			0
		),
		# 9069: replace missing on 2015-01-09 with 0
		Missing = replace(
			Missing,
			which(PlotPlantID=="9069" & Date=="2015-01-09"),
			0
		),
		# 9082: replace missing on 2014-06-27 with 0
		Missing = replace(
			Missing,
			which(PlotPlantID=="9082" & Date=="2014-06-27"),
			0
		),
		# 9220: replace dead on 2013-05-21 with 0
		Dead = replace(
			Dead,
			which(PlotPlantID=="9220" & Date=="2013-05-21"),
			0
		),
		# 9255: replace missing on 2015-02-03 with 0
		Missing = replace(
			Missing,
			which(PlotPlantID=="9255" & Date=="2015-02-03"),
			0
		),
		
		
		# 9338: replace dead on 2015-01-20 with 0
		# 9341: replace dead on 2015-01-20 with 0
		# 9350: replace dead on 2015-01-20 with 0
		# 9354: replace dead on 2015-01-20 with 0
		# 9394: replace dead on 2015-01-20 with 0
		# 9399: replace dead on 2015-01-20 with 0
		# 9401: replace dead on 2015-01-20 with 0
		# 9430: replace dead on 2015-01-20 with 0
		# 9437: replace dead on 2015-01-20 with 0
		# 9540: replace dead on 2015-01-20 with 0
		# 9583: replace dead on 2015-01-20 with 0
		# 9642: replace dead on 2015-01-20 with 0
		# 9682: replace dead on 2015-01-20 with 0
		# 9739: replace dead on 2015-01-20 with 0
		# 9747: replace dead on 2015-01-20 with 0
		# 9807: replace dead on 2015-01-20 with 0
		# 9818: replace dead on 2015-01-20 with 0
		# 9834: replace dead on 2015-01-20 with 0
		# 9871: replace dead on 2015-01-20 with 0
		# 9917: replace dead on 2015-01-20 with 0
		# 8087: replace dead on 2015-01-20 with 0
		# 8311: replace dead on 2015-01-20 with 0
		
	)
}