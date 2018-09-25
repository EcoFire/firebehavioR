#' Fire weather indices based on cumulative weather observations
#'
#' Methods to estimate daily fire weather indices using cumulative weather observations.
#' @usage fireIndexKBDI(temp, precip, map, rh, u)
#' @param temp a numeric vector of daily air temperature (C)
#' @param precip a numeric vector of daily precipitation (mm)
#' @param map a single numeric value of mean annual precipitation (mm)
#' @param rh a numeric vector of relative humidities (\%)
#' @param u a numeric vector of wind speeds (km/hr)
#' @details This function computes up to 8 methods to estimate fire weather indices.
#' The number of computed indices depends on the supplied arguments.
#' If requisite arguments for specific methods are not supplied, \code{'fireIndexKBDI'}
#' will not output results for those methods (i.e., there will be fewer than 8 columns).
#' The requisite arguments for each method:
#'  \describe{
#'   \item{kbdi}{\code{'temp'}, \code{'precip'}, \code{'map'}}
#'   \item{drought factor}{\code{'temp'}, \code{'precip'}, \code{'map'}}
#'   \item{forestMark5}{\code{'temp'}, \code{'precip'}, \code{'map'},\code{'u'}, \code{'rh'}}
#'   \item{fosbergKBDI}{\code{'temp'}, \code{'precip'}, \code{'map'},\code{'u'}, \code{'rh'}}
#'   \item{fuelMoistureKBDI}{\code{'temp'}, \code{'precip'}, \code{'map'},\code{'u'}, \code{'rh'}}
#'   \item{nesterov}{\code{'temp'}, \code{'precip'}, \code{'rh'}}
#'   \item{nesterovMod}{\code{'temp'}, \code{'precip'}, \code{'rh'}}
#'   \item{zdenko}{\code{'temp'}, \code{'precip'}, \code{'rh'}}
#' }
#' @return a data frame of fire weather index values with a column for each valid method.
#' @author Justin P Ziegler, \email{justin.ziegler@@colostate.edu}
#' @references
#' Sharples, J.J., McRae, R.H.D., Weber, R.O. and Gill, A.M., 2009. A simple index for assessing fuel moisture content.
#' \emph{Environmental Modelling & Software}, \strong{24}(5):637-646.  
#' Goodrick, S.L., 2002. Modification of the Fosberg fire weather index to include drought. I\emph{International Journal of Wildland Fire}, \strong{11}(4), pp.205-211.  
#' Sharples, J.J., McRae, R.H.D., Weber, R.O. and Gill, A.M.. 2009. A simple index for assessing fire danger rating.
#' \emph{Environmental Modelling & Software}. \strong{24}(6):764-774.  
#' Keetch, J.J., Byram, G.M. 1968. A drought index for forest fire control. \emph{RP-SE-68}, US Department of Agriculture, Forest Service, Southeastern Forest Experiment Station.  
#' Groisman, P.Y., Sherstyukov, B.G., Razuvaev, V.N., Knight, R.W., Enloe, J.G., Stroumentova, N.S., Whitfield, P.H., Førland, E., Hannsen-Bauer, I., Tuomenvirta, H. and Aleksandersson, H., 2007. Potential forest fire danger over Northern Eurasia: changes during the 20th century. \emph{Global and Planetary Change}, \strong{56}(3-4):371-386.  
#' Skvarenina, J., Mindas, J., Holecy, J. and Tucek, J., 2003, May. Analysis of the natural and meteorological conditions during two largest forest fire events in the Slovak Paradise National Park. \emph{In Proceedings of the International Scientific Workshop on Forest Fires in the Wildland–Urban Interface and Rural Areas in Europe: an integral planning and management challenge}.  
#' @examples
#' #Example using RAWS meteorological station data
#' data(rrRAWS)
#' ff = rbind(
#' data.frame(ffm = ffm('simard',rrRAWS$rh, rrRAWS$temp_c)$fm1hr,method='simard'),
#' data.frame(ffm = ffm('wagner',rrRAWS$rh, rrRAWS$temp_c)$fm1hr,method='wagner'),
#' data.frame(ffm = ffm('anderson',rrRAWS$rh, rrRAWS$temp_c)$fm1hr,method='anderson')
#' )
#' ff$dateTime = rep(rrRAWS$dateTime,3)
#' par(mfrow=c(3,1))
#' hist(ff$ffm[ff$method=="simard"])
#' hist(ff$ffm[ff$method=="wagner"])
#' hist(ff$ffm[ff$method=="anderson"])
#' @export
fireIndexKBDI <- function(temp = NA, precip = NA, map = NA, rh = NA, u = NA) {
  output <- data.frame(
    kbdi = 0, droughtFactor = 0, forestMark5 = 0, fosbergKBDI = 0,
    fuelMoistureKBDI = 0, nesterov = 0, nesterovMod = 0, zdenko = 0
  )
  output <- output[rep(seq_len(nrow(output)), length(temp)), ]

  if (!is.na(temp[1]) & !is.na(precip[1]) & !is.na(map[1])) {
    kbdi <- c(400, rep(NA, length(temp) - 1))
    droughtFactor <- rep(0, length(temp))
    for (i in 2:length(temp)) {
      if (precip[i - 1] == 0) {
        raineffect <- max(precip[i], 0) - 0.2
      } else {
        raineffect <- max(precip[i], 0)
      }

      kbdi[i] <- max(kbdi[i - 1] - (raineffect * 100), 0)
      droughtFactor[i] <- kbdiTable$DF[temp[i] >= kbdiTable$MinTemp & temp[i] <
        kbdiTable$MaxTemp & kbdi[i] >= kbdiTable$MinDI & kbdi[i] <= kbdiTable$MaxDI &
        map >= kbdiTable$MinMAP & map <= kbdiTable$MaxMAP]
      kbdi[i] <- kbdi[i] + droughtFactor[i]
    }
    output$kbdi <- kbdi
    output$droughtFactor <- droughtFactor
  } else {
    output$kbdi <- rep(NA, length(temp))
    output$droughtFactor <- rep(NA, length(temp))
  }

  if (!is.na(temp[1]) & !is.na(precip[1]) & !is.na(map[1]) & !is.na(u[1] & !is.na(rh[1]))) {
    rainLag <- 0
    forestMark5 <- rep(0, length(temp))
    fm <- fireIndex(temp = temp, rh = rh, u = u)$fuelMoisture
    fuelMoistureKBDI <- fm * output$kbdi
    faf <- 0.72 + 0.000002 * output$kbdi^2
    ffwi <- fireIndex(temp = temp, u = u, rh = rh)$fosberg
    fosbergKBDI <- faf * ffwi
    for (i in 2:length(temp)) {
      rainLag <- ifelse(precip[i - 1] > 0, 0, rainLag + 1)
      df <- min((0.191 * (output$kbdi[i] + 104) * (rainLag + 1)^1.5) / ((3.52 * (rainLag +
        1)^1.5) + precip[i - 1] - 1), 10)
      forestMark5[i] <- 2 * exp(-0.45 + 0.987 * log(df + 0.001) - 0.0345 * rh[i] +
        0.0338 * temp[i] + 0.0234 * u[i])
    }
    output$forestMark5 <- forestMark5
    output$fuelMoistureKBDI <- fuelMoistureKBDI
    output$fosbergKBDI <- fosbergKBDI
  } else {
    output$forestMark5 <- rep(NA, length(temp))
    output$fuelMoistureKBDI <- rep(NA, length(temp))
    output$fosbergKBDI <- rep(NA, length(temp))
  }

  if (!is.na(temp[1]) & !is.na(precip[1]) & !is.na(rh[1])) {
    dp <- 243.04 * (log(rh / 100) + ((17.625 * temp) / (243.04 + temp))) / (17.625 -
      log(rh / 100) - ((17.625 * temp) / (243.04 + temp)))
    mod <- c(0, rep(NA, length(dp)))
    nesterov <- rep(0, length(temp))
    nesterovMod <- rep(0, length(temp))
    zdenko <- rep(0, length(temp))

    for (i in 2:length(dp)) {
      nesterov[i] <- ifelse(precip[i] > 3, 0, ifelse(temp[i] < 0, 0, nesterov[i -
        1] + (temp[i] * (temp[i] - dp[i]))))
      mod <- ifelse(precip[i] < 0.1, 1, ifelse(precip[i] < 1, 0.8, ifelse(precip[i] <
        3, 0.6, ifelse(precip[i] < 6, 0.4, ifelse(precip[i] < 15, 0.2, ifelse(precip[i] <
        19, 0.1, 0))))))
      nesterovMod[i] <- ifelse(precip[i] > 3, 0, ifelse(temp[i] < 0, 0, nesterovMod[i -
        1] + (temp[i] * temp[i] - dp[i] * mod)))
      zdenko[i] <- (zdenko[i - 1] + temp[i] - dp[i]) * mod
    }
    output$nesterov <- nesterov
    output$nesterovMod <- nesterovMod
    output$zdenko <- zdenko
  } else {
    output$nesterov <- rep(NA, length(temp))
    output$nesterovMod <- rep(NA, length(temp))
    output$zdenko <- rep(NA, length(temp))
  }
  output <- output[, colSums(is.na(output)) != nrow(output)]
  return(output)
}
