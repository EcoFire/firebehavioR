#' Canopy Fire Initiation & Spread model
#'
#' Estimation of crown fire initiation likelihood and crown fire rate of spread using Cruz, Alexander & Wakimoto (2004a,b) and .
#' @param fsg a vector or data frame of fuel stratum gap [m]
#' @param u10 a vector or data frame with open (at a height 10 m above the canopy) wind speed [m/min]
#' @param effm a vector or data frame with stand density [trees/ha]
#' @param sfc a vector or data frame of surface fuel consumed [kg/m2]
#' @param cbd a vector or data frame of canopy bulk density [kg/m3]
#' @param id a vector or data frame of spot ignition delay, the time between which a firebrand is generated and the time it ignites on receptive fuels [min]
#' @return a data frame with probability of crown fire occurences ["%"], type of fire, crown fire rate of spread [m/min], and critical spotting distance [m]
#' @author Justin P Ziegler, \email{justin.ziegler@@colostate.edu}
#' @references Cruz M.G., Alexander M.E., Wakimoto R.H., 2004a. Modeling the likelihood of crown fire occurances in conifer forest stands. For. Sci. 50(4):640-658.
#' Development and testing of models for predicting crown fire rate of spread in conifer forest stands. Can. J. For. Res. 35:1626-1639.
#' Insert reference for spotting
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
    return(data.frame( type = type, pCrown = round(p_x*100,2), cros = round(cros,2), sepDist = round(sd,2)))
}
