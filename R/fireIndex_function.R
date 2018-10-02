#' Fire weather indices based on static weather observations
#'
#' Methods to estimate fire weather indices using static weather observations.
#' @param temp a numeric vector of air temperatures (C)
#' @param rh a numeric vector of relative humidities (\%)
#' @param u a numeric vector of wind speeds (km/hr)
#' @param fuel a numeric vector of available fuel load (Mg/ha), defaults to 4.5
#' @param cure a numeric vector for proportion of cured grass (\%), defaults to 100
#' @details This function computes seven methods to estimate static fire weather indices: the Angstrom Index, the Chandler Burning Index, the Hot Dry Windy Index, the Fuel Moisture Index, the Fosberg Fire Weather Index,
#' the MacArthur Grassland Mark 4 Index, and the MacArthur Grassland Mark 5 Index. Each of these are static in that values are derived using a
#' daily weather summary and do not consider weather during prior days.
#' \code{temp}, \code{rh} and \code{u} are required for all methods.
#' The latter two indices also use \code{fuel}, and the Grassland Mark 4 Index uses \code{cure}. Defaults for \code{fuel} and \code{cure}
#' are provided, but can be specified by the user. Sharples (2009a, b) review all of the methods.
#' @return a data frame of static fire weather index values
#' @author Justin P Ziegler, \email{justin.ziegler@@colostate.edu}
#' @references
#' Sharples, J.J., McRae, R.H.D., Weber, R.O. and Gill, A.M., 2009a. A simple index for assessing fuel moisture content. \emph{Environmental Modelling & Software}, \strong{24}(5):637-646.\cr
#' Sharples, J.J., McRae, R.H.D., Weber, R.O. and Gill, A.M., 2009b. A simple index for assessing fire danger rating. \emph{Environmental Modelling & Software}. \strong{24}(6):764-774.
#' @examples
#' #Example using RAWS meteorological station data
#' data(rrRAWS)
#' rrRAWS.daily =   rrRAWS[format(strptime(rrRAWS$dateTime, "%m/%d/%Y %H:%M"), "%H:%M")=="14:35",]
#' fireIndex(temp=rrRAWS.daily$temp_c, u= rrRAWS.daily$windSpeed_kmh, rh = rrRAWS.daily$rh)
#' @export

fireIndex <- function(temp, u, rh, fuel = 4.5, cure = 100) {
  output <- data.frame(
    angstrom = 0, Chandler = 0, hotDryWindy = 0, fuelMoisture = 0, fosberg = 0, grasslandMk4 = 0,
    grasslandMk5 = 0
  )
  output <- output[rep(seq_len(nrow(output)), length(temp)), ]

  svp <- (6.107 * 10^(7.5 * temp / (237.3 + temp))) / 10
  vpd <- ((100 - rh) / 100) * svp
  output$hotDryWindy <- u * vpd * 0.278
  output$fuelMoisture <- max(1, u) / (10 - 0.25 * (temp - rh))
  m <- ffm(method = "simard", rh, temp)$fm1hr
  output$fosberg <- (1 - 2 * (m / 30) + 1.5 * (m / 30)^2 - 0.5 * (m / 30)^3) * sqrt(1 +
    (0.621371 * u)^2) / 0.3002
  output$grasslandMk4 <- exp(-1.523 + 1.027 * log(fuel) - (0.009432 * (100 - cure)^1.536) +
    0.0276 * temp - 0.2205 * sqrt(rh) + 0.6422 * sqrt(u))
  m2 <- (97.7 + 4.06 * rh) / temp + 6 - 0.00854 * rh + 3000 / cure - 30
  m2[!is.finite(m2) | m2 < 0] <- 100
  output$grasslandMk5 <- ifelse(m2 > 30, 0, ifelse(m2 < 18.8, 3.35 * fuel * exp(-0.0897 *
    m2 + 0.0403 * u), 0.299 * fuel * exp(-1.686 + 0.0403 * u) * (30 - m2)))
  output$chandler <- (((110 - 1.373 * rh) - 0.54 * (10.20 - temp)) * (124 * 10**(-0.0142 * rh))) / 60
  output$angstrom <- rh / 20 + (27 - temp) / 10
  return(output)
}
