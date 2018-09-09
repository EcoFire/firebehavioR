#' Estimate fine fuel moisture
#'
#' Methods to estimate fine fuel moisture based on meteorological data.
#' @param method a character vector of specifying the method
#' \code{("pech"},\code{("simard"}, \code{"wagner"}, \code{"anderson"}, \code{"mcarthur"}, \code{"fbo"})
#' @param rh a numeric vector of relative humidities (\%)
#' @param temp a numeric vector of dry bulb temperatures (deg. C)
#' @param month a numeric vector of Gregorian month numbers (1-12)
#' @param hour a numeric vector of hours (1-24)
#' @param asp a character vector of aspects specified as cardinal directions, either \code{"N"}, \code{"S"}, \code{"E"}, or \code{"W"}
#' @param slp a numeric vector of topographic slopes (\%)
#' @param bla a character vector specifying the difference in elevation between the fine fuel's location and that of the meteorological data;
#' either within 305 m ('l', the default), or the meteorological data are > 305m below (\code{"b"}), or above (\code{"a"}) the fine fuel's location
#' @param shade a character vector specifying whether fine fuels are shaded, \code{"y"} or \code{"n"}
#' @details This function has four methods to estimate fine fuel moisture. If \code{method = "fbo"}, all arguments must be specified,
#' otherwise, only \code{method}, \code{rh} and \code{temp} are needed.
#' @return a data frame of litter, 1-hr, 10-hr, and 100-hr fuel moistures
#' @author Justin P Ziegler, \email{justin.ziegler@@colostate.edu}
#' @references
#' Viney, N.R. 1991. A review of fine fuel moisture modelling. \emph{International Journal of Wildland Fire}. \strong{1}(4):215–234.
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
#'
#' #The FBO method requires more inputs
#' data(rrRAWS)
#' rh = rrRAWS$rh
#' temp =rrRAWS$temp_c
#' month = as.numeric(format(strptime(rrRAWS$dateTime,"%m/%d/%Y %H:%M"),'%m'))
#' hour = as.numeric(format(strptime(rrRAWS$dateTime,"%m/%d/%Y %H:%M"),'%H'))
#' ffm(method = 'fbo', rh, temp, month, hour, asp = 'N', slp = 10, bla = 'l', shade = 'n')
#' @export

ffm <- function(method, rh, temp, month, hour, asp, slp, bla, shade) {
    if (method == "pech") {
        fm1hr = ifelse(rh <= 40, 0.136 * rh^1.07 + 0.00059 * exp(0.3 * rh), ifelse(rh <
            75, 0.2772 * rh - 4.013 + 0.18 * (21.1 - temp) * (1 - 54.6 * exp(-0.1 *
            rh)), 0.618 * rh^0.753 + 0.18 * (21.1 - temp) * abs(1 - 54.6^-0.1 * rh) +
            0.000454 * exp(0.1 * rh)))
    }
    if (method == "simard") {
        fm1hr = ifelse(round(rh, 0) > 49, 21.06 - 0.4944 * rh + 0.005565 * rh^2 -
            0.00063 * rh * temp, ifelse(round(rh, 0) >= 10, 1.76 + 0.1601 * rh -
            0.0266 * temp, 0.03 + 0.2626 * rh - 0.00104 * rh * temp))
    }
    if (method == "wagner") {
        fm1hr = 0.942 * rh^0.679 + 0.000499 * exp(0.1 * rh) + 0.18 * (21.1 - temp) *
            (1 - exp(-0.115 * rh))
    }
    if (method == "anderson") {
        fm1hr = 1.651 * rh^0.493 + 0.001972 * exp(0.092 * rh) + 0.101 * (23.9 - temp)
    }
    if (method == "mcarthur") {
        fm1hr = 5.658 + 0.04651 * rh + (0.0003151 * rh^3)/temp - 0.1854 * temp^0.77
    }
    if (method == "fbo") {
        if (length(slp) == 1) {
            slp = rep(slp, length(rh))
        }
        if (length(bla) == 1) {
            bla = rep(bla, length(rh))
        }
        if (length(asp) == 1) {
            asp = rep(asp, length(rh))
        }
        if (length(shade) == 1) {
            shade = rep(shade, length(rh))
        }

        slp = ifelse(slp < 31, "lo", "hi")
        shade[hour < 8 | hour > 19] = "y"
        hour[hour <= 8 | hour > 19] = 9
        refM = rep(99, length(temp))
        corM = rep(99, length(temp))

        for (i in 1:length(temp)) {
            refM[i] = fboTable[[1]][fboTable[[1]]$templo <= temp[i] & fboTable[[1]]$temphi >
                temp[i] & fboTable[[1]]$rhlo <= rh[i] & fboTable[[1]]$rhhi > rh[i],
                ]$refMoist
            corM[i] = fboTable[[2]][fboTable[[2]]$monthlo <= month[i] & fboTable[[2]]$monthhi >=
                month[i] & fboTable[[2]]$timelo <= hour[i] & fboTable[[2]]$timehi >=
                hour[i] & fboTable[[2]]$shaded == tolower(shade[i]) & fboTable[[2]]$aspect ==
                toupper(asp[i]) & fboTable[[2]]$slope == slp[i] & fboTable[[2]]$level ==
                bla[i], ]$correction
        }
        fm1hr = refM + corM
    }
    output = data.frame(fmLitter = fm1hr, fm1hr = fm1hr, fm10hr = fm1hr + 1, fm100hr = fm1hr +
        3)
    return(output)
}


