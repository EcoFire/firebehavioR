#' Calculated wind adjustment factor
#'
#' Prediction of wind adjustment factor for sheltered and unsheltered fuels.
#' @usage waf(fuelDepth, forestHt, cr, cc, sheltered = "n")
#' @param fuelDepth a numeric vector of surface fuel bed depths (cm)
#' @param forestHt a numeric vector of average stand tree heights (m)
#' @param cr a numeric vector of crown ratios (\%)
#' @param cc a numeric vector of canopy cover (\%)
#' @param sheltered a character vector of either \code{"y"} or \code{"n"} (default) to use sheltered or not (unsheltered) equations
#' @details This calculates the wind adjustment factor (ratio of 20-ft open wind speed to wind speed at mid-surface flame height).
#' One of two equations are used, depending on user input: by default, this function assumes the surface fuel bed is unsheltered.
#' \code{fuelDepth} must be a positive value if the unsheletered variant is invoked.
#' \code{forestHt} must be a positive value if the sheletered variant is invoked.
#' There are two conditions to enable calculation for a sheltered fuelbed. First, if \code{cr} and
#' \code{cc} are entered and lead lead to a crown fill portion (CF) above 5\%, where , or the user could enter \code{"y"} for sheltered fuels. In the latter
#' case, this function assumes a crown fill portion of 10\%.
#' @return a vector of wind adjustment factors
#' @author Justin P Ziegler, \email{justin.ziegler@@colostate.edu}
#' @references  Andrews, P.L. 2012. Modeling wind adjustment factor and midflame wind speed for Rothermel’s surface fire spread model. \emph{RMRS-GTR-266}. USDA Forest Service Rocky Mountain Research Station.
#' @examples
#' #Sheltered fuelbed with a 10 m tall forest with unknown crown ratio and canopy cover
#' waf(forestHt = 10, sheltered = 'y')
#' #Sheltered fuelbed with known high crown ratio and canopy cover
#' waf(forestHt = 10, cr = 40, cc = 40)
#' #Sheltered fuelbed with known low crown ratio and canopy cover
#' waf(fuelDepth = 1, forestHt = 10, cr = 10, cc = 10)
#' #Because cr and cc are low, the previous solution is equivalent to an unsheletered fuelbed
#' waf(fuelDepth = 1)
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
  return(waf$waf)
  }
