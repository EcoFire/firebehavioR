#' Canopy Fire Initiation & Spread model
#'
#' Prediction of crown fire probability, crown fire rate of spread and seperation distance (Alexander and Cruz 2006). Seperation distance is distance ahead of main fire front required for a spot fire to form, seperate of a main fire.
#' @param fsg a vector or data frame of fuel stratum gap (m)
#' @param u10 a vector or data frame with open (at a height 10 m above the canopy) wind speed [m/min]
#' @param effm a vector or data frame with effective fine fuel moisture (\%)
#' @param sfc a vector or data frame of surface fuel consumed (kg/m2)
#' @param cbd a vector or data frame of canopy bulk density (kg/m3)
#' @param id a vector or data frame of spot ignition delay, the time during which a given firebrand generates, is transported aloft, and ignites a receptive fuelbed (min)
#' @return a data frame with probability of crown fire occurences (\%), type of fire, crown fire rate of spread (m/min), and critical spotting distance (m)
#' @author Justin P Ziegler, \email{justin.ziegler@@colostate.edu}
#' @references  Alexander M.E. & Cruz M.G. 2006. Evaluating a model for predicting active crown fire rate of spread using wildfire observations. \emph{Canadian Journal of Forest Research}. \strong{36}:2015-3028.
#' @examples
#' library(firebehavioR)
#'
#'
#' @export

cfis = function(fsg, u10, effm, sfc, cbd, id) {

  g_x = 4.236 + 0.02142*u10 - 0.71*fsg - 0.331*effm + ifelse( sfc<1, -4.613, ifelse (sfc>2,0,-1.856) )
    p_x = exp(g_x)/(1+ exp(g_x))

    crosa = (11.02*(.06*u10)^0.9)*(cbd^0.19)*exp(-0.17*effm)
crosp = crosa*exp(-0.3333*crosa*cbd)
    cac = crosa*cbd/3

    cros = ifelse(p_x>0.5,
           ifelse(cac<1,crosp,crosa),
           NA)
    sd = cros*(30 + id) - cros*(30 + (exp(-.115*30)/.115)-1/(.115))
    type = ifelse(p_x<.5,'surface', ifelse(cac<1,'passive','active'))
    return(data.frame( type = type, pCrown = round(p_x*100,2), cROS = round(cros,2), sepDist = round(sd,2)))
}





