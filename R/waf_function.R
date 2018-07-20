#' Calculated wind adjustment factor
#'
#' Prediction of wind adjustment factor for sheltered and unsheltered fuels.
#' @usage waf(fuelDepth, forestHt, cr, cc, sheltered = "n")
#' @param fuelDepth a vector for depths of surface fuel bed (cm)
#' @param forestHt a vector of average stand tree heights (m)
#' @param cr a vector of crown ratios (\%)
#' @param cc a vector of canopy cover (\%)
#' @param sheltered a vector of either "y"es or "n"o as flags for using sheltered or unsheltered equations
#' @details This calculates the wind adjustment factor (ratio of 10-m open wind speed to wind speed at the height of a surface fire).
#' One of two equations are used, depending on user input. By default, this function assumes the surface fuel bed is unsheltered.
#' \code{fuelDepth} must be a positive value if the unsheletered variant is invoked.
#' \code{forestHt} must be a positive value if the sheletered variant is invoked.
#' There are two conditions to enable calculation for a sheltered fuelbed. First, if  user could enter values for \code{cr} and
#' \code{cc} that lead to a crown fill portion above 5\%, or the user could enter \code{"y"} for \code{sheltered}. In the latter
#' case, this function assumes a crown fill portion of 10\%.
#' @return a vector of wind adjustment factors
#' @author Justin P Ziegler, \email{justin.ziegler@@colostate.edu}
#' @references  Andrews, P.L. 2012. Modeling wind adjustment factor and midflame wind speed for Rothermel’s surface fire spread model. \emph{RMRS-GTR-266}. USDA Forest Service Rocky Mountain Research Station.
#' @examples
#' library(firebehavioR)
#'
#'
#' @export
  waf = function(fuelDepth=0, forestHt=0, cr=0, cc=0, sheltered='n') {
  cr = cr/100
  waf=data.frame(fuelDepth=fuelDepth,forestHt=forestHt,cr=cr,cc=cc,sheltered=sheltered)
  waf$f = ifelse(waf$cr>0 & waf$cc>0,waf$cc/300*waf$cr,ifelse(waf$sheltered=='y',.1,0))

waf$waf=
   ifelse(waf$f < 0.05,
   1.83/log((20.0 + 1.18*waf$fuelDepth/100)/(0.43*waf$fuelDepth/100)),
   ifelse(waf$f > 0.05,
   0.555/(sqrt(waf$f*3.28*waf$forestHt)* log((20.0 + 1.18*waf$forestHt)/(0.43*waf$forestHt))),
   1))
  return(waf)
  }
