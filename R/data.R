#' Modified Scott & Burgan (2005) moisture scenarios.
#'
#' Moisture scenarios are a set of fuel moistures of surface fuels, on a dry-weight basis, for each of the surface fuel classes. Adapted from Scott & Burgan (2005), this dataset includes fuel moistures of litter.
#'@usage data("fuelMoisture")
#' @format A data frame with 16 observations of 7 variables:
#' \describe{
#'   \item{fmLitter}{moisture of litter (\%)}
#'   \item{fm1hr}{moisture of 1-hr fuel (\%)}
#'   \item{fm10hr}{moisture of 10-hr fuel (\%)}
#'   \item{fm100hr}{moisture of 100-hr fuel (\%)}
#'   \item{fmLiveHerb}{moisture of herbaceous fuel (\%)}
#'   \item{fmLiveWoody}{moisture of woody fuel (\%)}
#'   \item{description}{scenario description}
#' }
#' @source Scott, J., & Burgan, R. E. 2005. A new set of standard fire behavior fuel models for use with Rothermel’s surface fire spread model. \emph{RMRS-GTR-153}. Fort Collins, CO: US Department of Agriculture, Forest Service, Rocky Mountain Research Station.
#' @seealso  \code{\link{rothermel}}
"fuelMoisture"

#' Surface fuel models.
#'
#' Fuel models developed by Anderson (1982), Scott (1999), and Scott & Burgan (2005) for prediction of surface fire behavior.
#'@usage data("fuelModels")
#' @format A data frame with 60 observations of 18 variables:
#' \describe{
#'   \item{fuelModelType}{"S"tatic or "D"ynamic fuel load transfer}
#'   \item{loadLitter}{load of litter fuel (Mg/ha)}
#'   \item{load1hr}{load of 1-hr fuel (Mg/ha)}
#'   \item{load10hr}{load of 10-hr fuel (Mg/ha)}
#'   \item{load100hr}{load of 100-hr fuel (Mg/ha)}
#'   \item{loadLiveHerb}{ load of herbaceous fuel (Mg/ha)}
#'   \item{loadLiveWoody}{ load of woody fuel(Mg/ha)}
#'   \item{savLitter}{surface area to volume ratio of litter fuel (m2/m3)}
#'   \item{sav1hr}{surface area to volume ratio of 1-hr fuel (m2/m3)}
#'   \item{sav10hr}{surface area to volume ratio of 10-hr fuel (m2/m3)}
#'   \item{sav100hr}{surface area to volume ratio of 100-hr fuel (m2/m3)}
#'   \item{savLiveHerb}{ surface area to volume ratio of herbaceous fuel (m2/m3)}
#'   \item{savLiveWoody}{ surface area to volume ratio of woody fuel (m2/m3)}
#'   \item{fuelBedDepth}{ depth of woody fuel (cm)}
#'   \item{mxDead}{dead fuel moisture of extincton (\%)}
#'   \item{heat}{heat content (J/g)}
#'   \item{description}{fuel model description}
#'   \item{source}{scientific source}
#' }
#' @references Anderson, H.E. 1982. Aids to determining fuel models for estimating fire behavior. \emph{INT-GTR-122}. US Department of Agriculture, Forest Service, Intermountain Forest and Range Experimental Station.\cr
#' Scott, J.H. 1999. NEXUS: A system for assessing crown fire hazard. \emph{Fire Management Notes} \strong{59}(2):20 –24.\cr
#' Scott, J.H., & Burgan, R. E. 2005. A new set of standard fire behavior fuel models for use with Rothermel’s surface fire spread model. \emph{RMRS-GTR-153}. US Department of Agriculture, Forest Service, Rocky Mountain Research Station.
#' @seealso  \code{\link{rothermel}}
"fuelModels"

#' Bitmap of burning tree
#' Bitmap image of burning tree
#' @keywords internal
#' @seealso  \code{\link{fireChart}}
"tree"

#' Bitmap of fire plume
#' Bitmap image of fire plume
#' @keywords internal
#' @seealso  \code{\link{fireChart}}
"plume"

#' Bitmap of bulldozer
#' Bitmap image of bulldozer
#' @keywords internal
#' @seealso  \code{\link{fireChart}}
"bulldozer"

#' Bitmap of firefighter
#' Bitmap image of bulldozer
#' @keywords internal
#' @seealso  \code{\link{fireChart}}
"firefighter"

#' Colorado dry forest inventory summary.
#'
#' Fuels inventory summary of seven sampled forests in the southern Rocky Mountains and Colorado Plateau. Each forest was sapled before and after tree thinnings.
#'@usage data("coForest")
#' @format A data frame with 14 observations of 10 variables:
#' \describe{
#'   \item{site}{name of forest location}
#'   \item{status}{either before (pre) or after (post) forest thinning}
#'   \item{trees_perha}{tree density (trees/ha)}
#'   \item{basalArea_m2ha}{basal area (m2/ha)}
#'   \item{qmd_cm}{quandratic mean diameter (cm)}
#'   \item{height_m}{mean tree height (m)}
#'   \item{sfl_kgm2}{surface fuel load (kg/m2)}
#'   \item{cbd_kgm3}{canopy bulk density (kg/m3)}
#'   \item{cbh_m}{canopy base height (m)}
#'   \item{cfl_kgm2}{canopy fuel load (kg/m2)}
#' }
#' @source Ziegler, J.P., Hoffman, C., Battaglia, M., Mell, W., 2017. Spatially explicit measurements of forest structure
#' and fire behavior following restoration treatments in dry forests. \emph{Forest Ecology & Management}  \strong{386}, 1–12. doi:10.1016/j.foreco.2016.12.002
"coForest"


#' Rampart Range RAWS meteorological data
#'
#' Hourly meteorological data from April through September 2017 from the Rampart Range Remote Automated Weather Station (RAWS; Station ID: RRAC2), maintained by the United States Forest Service
#'@usage data("rrRAWS")
#' @format A data frame with 4392 observations of 4 variables:
#' \describe{
#'   \item{dateTime}{date and time of individual observation formatted as \code{"\%m/\%d/\%Y \%H:\%M"} }
#'   \item{temp_c}{air temperature (deg. C)}
#'   \item{rh}{relative humidity (\%)}
#'   \item{windSpeed_kmh}{wind speed (km/hr)}
#'   \item{precip_mm}{precipitation (mm)}
#' }
#' @source RAWS USA Climate Archive \url{https://raws.dri.edu/}
"rrRAWS"

#' Fire Behavior Officer's table
#'
#' Look up charts in tabular form to determine fine fuel moisture
#'@usage data("fboTable")
#' @format A list with two data frames:
#' \describe{
#'   \item{1}{a list of reference fine fuel moistures (\%) by temperature (deg. C) and relative humidity (\%)}
#'   \item{2}{a list of correction fine fuel moistures (\%) by month (0-12), hourly time (0-23),
#'    shading, cardinal aspect, slope cateory, and elevation level category}
#' }
#' @source Rothermel, R.C., Wilson, R.A., Morris, G.A., Sackett, S.S. 1986. Modeling moisture content of dead wildland fuels: input to the BEHAVE fire prediction system
#'  \emph{INT-RP-359}. US Department of Agriculture, Forest Service, Intermountain Forest and Range Experimental Station.
#' @keywords internal
"fboTable"


#' Template data for fire characteristics chart
#'
#' Data of heat per unit area (kJ/m2), rate of spread (m/min), and flame length (m) for creating the template of the fire characteristics chart
#'@usage data("fireChartData")
#' @source Andrews, P.L., & Rothermel, R.C. 1982. Charts for interpreting wildland fire behavior characteristics.
#'  \emph{INT-GTR-131}. US Department of Agriculture, Forest Service, Intermountain Forest and Range Experimental Station.
#' @keywords internal
"fireChartData"

#' KBDI Lookup table
#'
#' Data of drought factor by drought index, temperature and mean annual precipitation.
#'@usage data("fireChartData")
#' @source
#'  \emph{INT-GTR-131}. US Department of Agriculture, Forest Service, Intermountain Forest and Range Experimental Station.
#' @keywords internal
"kbdiTable"
