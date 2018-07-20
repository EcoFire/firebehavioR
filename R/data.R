#' Modified Scott & Burgan (2005) moisture scenarios.
#'
#' Moisture scenarios are a set of fuel moistures of surface fuels, on a dry-weight basis, for each of the surface fuel classes. Originally developed by Scott & Burgan (2005), this dataset includes fuel moistures of litter.
#'@usage data(fuelMoisture)
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
#' @seealso  \code{\link{nexus}}
"fuelMoisture"

#' Surface fuel models.
#'
#' Fuel models developed by Anderson (1982), Scott (1999), and Scott & Burgan (2005) for prediction of surface fire behavior.
#'@usage data(fuelModels)
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
#' @seealso  \code{\link{nexus}}
"fuelModels"
