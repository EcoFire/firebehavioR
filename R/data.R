#' Modified Scott & Burgan (2005) moisture scenarios.
#'
#' Moisture scenarios are a set of fuel moistures of surface fuels, on a dry-weight basis, for each of the surfacefuel classes. Originally developed by Scott & Burgan (2005), this dataset includes fuel moistures of litter.
#'@usage data(fuelMoisture)
#' @format A data frame with 16 observations of 7 variables:
#' \describe{
#'   \item{FM_litter}{moisture of litter, (\%)}
#'   \item{FM_1hr}{moisture of 1-hr fuel, (\%)}
#'   \item{FM_10hr}{moisture of 10-hr fuel, (\%)}
#'   \item{FM_100hr}{moisture of 100-hr fuel, (\%)}
#'   \item{FM_Live_Herb}{moisture of herbaceous fuel, (\%)}
#'   \item{FM_Live_Woody}{moisture of woody fuel, (\%)}
#'   \item{Description}{Scenario description}
#'   ...
#' }
#' @source Scott, J., & Burgan, R. E. 2005. A new set of standard fire behavior fuel models for use with Rothermel’s surface fire spread model. \emph{Gen. Tech. Rep. RMRS-GTR-153}. Fort Collins, CO: US Department of Agriculture, Forest Service, Rocky Mountain Research Station.
#' @seealso  \code{\link{nexus}}
"fuelMoisture"

#' Surface fuel models.
#'
#' Fuel models developed by Anderson (1982), Scott (1999), and Scott & Burgan (2005) for prediction of surface fire behavior.
#'@usage data(fuelModels)
#' @format A data frame with 69 observations of 8 variables:
#' \describe{
#'   \item{Fuel_Model_Type}{'S'tatic or 'D'ynamic fuel load transfer}
#'   \item{Load_litter}{load of litter fuel (0.1kg/m2)}
#'   \item{Load_1h}{load of 1-hr fuel (0.1kg/m2)}
#'   \item{Load_10h}{load of 10-hr fuel (0.1kg/m2)}
#'   \item{Load_100h}{load of 100-hr fuel (0.1kg/m2)}
#'   \item{Load_Live_Herb}{ load of herbaceous fuel (0.1kg/m2)}
#'   \item{Load_Live_Woody}{ load of woody fuel(0.1kg/m2)}
#'   \item{SA.V_litter}{load of litter fuel (0.1kg/m2)}
#'   \item{SA.V_1h}{load of 1-hr fuel (0.1kg/m2)}
#'   \item{SA.V_10h}{load of 10-hr fuel (0.1kg/m2)}
#'   \item{SA.V_100h}{load of 100-hr fuel (0.1kg/m2)}
#'   \item{SA.V_Live_Herb}{ load of herbaceous fuel (0.1kg/m2)}
#'   \item{SA.V_Live_Woody}{ load of woody fuel(0.1kg/m2)}
#'   \item{Fuel_Bed_Depth}{ load of woody fuel(0.1kg/m2)}
#'   \item{Mx_dead}{Dead fuel moisture of extincton (\%)}
#'   \item{Heat}{Heat content (kJ/kg)}
#'   \item{Description}{Fuel model name}
#'   \item{Source}{Scientific source}
#'   ...
#' }
#' @references Anderson, H.E. 1982. Aids to determining fuel models for estimating fire behavior. \emph{Gen. Tech. Rep. INT-GTR-122}. Ogden, UT: US Department of Agriculture, Forest Service, Intermountain Forest and Range Experimental Station.\cr
#' Scott, J.H. 1999. NEXUS: A system for assessing crown fire hazard. \emph{Fire Management Notes} \strong{59}(2):20 –24.\cr
#' Scott, J.H., & Burgan, R. E. 2005. A new set of standard fire behavior fuel models for use with Rothermel’s surface fire spread model. \emph{Gen. Tech. Rep. RMRS-GTR-153}. Fort Collins, CO: US Department of Agriculture, Forest Service, Rocky Mountain Research Station.
#' @seealso  \code{\link{nexus}}
"fuelModels"
