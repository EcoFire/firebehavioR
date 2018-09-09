#' Canopy Fire Initiation & Spread model
#'
#' Prediction of crown fire probability, crown fire rate of spread and seperation distance (Alexander and Cruz 2006). Seperation distance is distance ahead of main fire front required for a spot fire to form, seperate of a main fire.
#' @param fsg a numeric vector of fuel stratum gaps (m)
#' @param u10 a numeric vector of 10-m open wind speeds (km/hr)
#' @param effm a numeric vector of effective fine fuel moistures (\%)
#' @param sfc a numeric vector of surface fuel consumed (Mg/ha)
#' @param cbd a numeric vector of canopy bulk densities (kg/m3)
#' @param id a numeric vector of spot ignition delays, the time during which a given firebrand generates, is transported aloft, and ignites a receptive fuelbed (min)
#' @return a data frame with type of fire, probability of crown fire occurences (\%), crown fire rate of spread (m/min), and critical spotting distance (m)
#' @author Justin P Ziegler, \email{justin.ziegler@@colostate.edu}
#' @references  Alexander M.E., Cruz M.G. 2006. Evaluating a model for predicting active crown fire rate of spread using wildfire observations. \emph{Canadian Journal of Forest Research}. \strong{36}:2015-3028.
#' @examples
#' data("coForest")
#' # show the data format:
#' head(coForest)
#' # Predict crown fire, using coForest
#' # measurements and assumed weather
#' # parameters
#' df.cfis = cfis(fsg = coForest$cbh_m, u10 = 20,
#'     effm = 6, sfc = coForest$sfl_kgm2*10, cbd = coForest$cbd_kgm3,
#'     id = 1)
#' print(df.cfis)
#'
#' # Examine differences between treatment
#' # statuses
#' aggregate(x = df.cfis$cROS, by = list(treatmentStatus = coForest$status),
#'     FUN = mean)
#' # Now, examine the sensitivity of fire
#' # type designations to wind speed by
#' # treatment status
#' coForest = coForest[rep(seq_len(nrow(coForest)),
#'     11), ]
#' coForest$u10 = sort(rep(10:20, 14))
#' coForest$type = cfis(coForest$cbh_m, coForest$u10,
#'     6, coForest$sfl_kgm2*10, coForest$cbd_kgm3,
#'     1)$type
#' table(u10 = coForest$u10, coForest$type,
#'     coForest$status)
#' @export
cfis = function(fsg, u10, effm, sfc, cbd, id) {
 if( any(fsg > 12) ) warning ('Canopy base height should not exceed 12 m')
 if( any(u10 > 80) ) warning ('Open wind speed should not exceed 80 km/hr')
 if( any(effm > 20) ) warning ('Fuel moisture should not exceed 20 percent')
 if( any(cbd > 1) ) warning ('Canopy bulk density should not exceed 1 kg/m3')

    sfc = sfc / 10
    g_x = 4.236 + 0.357 * u10 - 0.71 * fsg - 0.331 * effm + ifelse(sfc <
        1, -4.613, ifelse(sfc > 2, 0, -1.856))
    p_x = exp(g_x)/(1 + exp(g_x))

    crosa = (11.02 * u10^0.9) * (cbd^0.19) * exp(-0.17 * effm)
    crosp = crosa * exp(-0.3333 * crosa * cbd)
    cac = crosa * cbd/3

    cros = ifelse(p_x > 0.5, ifelse(cac < 1, crosp, crosa), NA)
    sd = cros * (30 + id) - cros * (30 + (exp(-0.115 * 30)/0.115) -
        1/(0.115))
    type = ifelse(p_x < 0.5, "surface", ifelse(cac < 1, "passive",
        "active"))
    return(data.frame(type = type, pCrown = round(p_x * 100,
        2), cROS = round(cros, 2), sepDist = round(sd, 2)))
}
