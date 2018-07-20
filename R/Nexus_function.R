#' Nexus Fire Behavior Modeling System
#'
#' Potential surface and crown fire behavior predicted by Scott and Reinhardt (2001).
#' @param surfFuel a vector or data frame of surface fuel attributes. Variable names are not important but the order is important.
#' \enumerate{
#'   \item the fuel model type, either 'S'tatic or D'ynamic herb load transfer
#'   \item litter load (Mg/ha)
#'   \item 1-hr load (Mg/ha)
#'   \item 10-hr load (Mg/ha)
#'   \item 100-hr load (Mg/ha)
#'   \item herb load (Mg/ha)
#'   \item woody load (Mg/ha)
#'   \item litter SAV (m2/m3)
#'   \item 1-hr SAV (m2/m3)
#'   \item 10-hr SAV (m2/m3)
#'   \item 100-hr SAV (m2/m3)
#'   \item herb SAV (m2/m3)
#'   \item woody SAV (m2/m3)
#'   \item fuel bed depth (cm)
#'   \item moisture of extinction (\%)
#'   \item heat content (J/g)
#'   }
#' @param moisture a vector or data frame of fuel moistures on a dry-weight basis (\%) for litter, 1-hr
#' 10-hr, 100-hr, herbaceous, and woody fuel classes, in order (6 values or columns)
#' @param crownFuel a vector or data frame with canopy fuel attributes consisting of: canopy
#' bulk density (kg/m3); foliar moisture content (\%); canopy base height (m); and canopy fuel load (kg/m2), in order
#' (4 values or columns)
#' @param enviro a vector or data frame of environmental variables including: topographic slope (\%);
#' open windspeed (km/hr);  wind direction, from uphill (deg.); and wind adjustment factor (0-1), in order (4 values or columns)
#' @param rosMult a numeric value for multiplying crown fire rate of spread, defaults to 1 (see details)
#' @param cfbForm a character string specifying how crown fraction burned is calculated. Options are "sr", "w", or "f" (default); see details.
#' @details This in an R build of the Nexus fire behavior modeling system (Scott & Reinhardt 2001) which links sub-models of surface fire rate of
#' spread (Rothermel 1972), crown fire initiation (Van Wagner 1977), and Rothermel's (1991) crown fire rate of spread. \cr
#' \code{rosMult} multiples the rate of spread for active or passive crown fires and is recommended a value of 1.7 when a user desires a maximum
#' crown fire rate of spread (Rothermel 1991). \cr
#' \code{cfbForm} selects the method to estimate crown fraction burned. This selection impacts estimates of passive
#' crown fraction burned, fireline intensity, and heat per unit area. Use "sr" for Scott and Reinhardt (2001), "w" for van Wagner (1993),
#' and "f" for Finney (1998).
#' @return a list with 6 data frames
#' \item{fireBehavior}{a data frame with fire behavior estimates including fire type, crown fraction burned (\%), rate of spread (m/min),
#' heat per unit area (kW/m2), fireline intensity (kW/m), flame length (m), direction of max spread (deg), scorch height (m),
#' torching index (m/min), crowning index (m/min), surfacing index (m/min), effective midflame wind (m/min), flame residence time (min) }
#' \item{detailSurface}{a data frame with some intermediate variables of surface fire behavior including: potential ROS (m/min); no wind,
#'  no slope ROS (m/min); slope factor (-); wind factor (-); characteristic dead fuel moisture (\%); characteristic live fuel moisture (\%);
#'  characteristic SAV (m2/m3); bulk density (kg/m3); packing ratio (-); relative packing ratio (-); reaction intensity (kW/m2); heat source (kW/m2);
#'  heat sink (kJ/m3)}
#' \item{detailCrown}{a data frame with some intermediate variables of crown fire behavior including: potential ROS (m/min);
#' no wind, no slope ROS (m/min); slope factor (-); wind factor (-); characteristic dead fuel moisture (\%);
#' characteristic live fuel moisture (\%); characteristic SAV (m2/m3); bulk density (kg/m3); packing ratio (-); relative packing ratio (-);
#' reaction intensity (kW/m2); heat source (kW/m2); heat sink (kJ/m3)}
#' \item{critInit}{a data frame of critical values for crown fire initiation including: fireline intensity (kW/m), flame length (m), surface ROS (m/min), Canopy base height (m)}
#' \item{critActive}{a data frame of critical values for active crown fire  including: canopy bulk density (kg/m3)", "ROS, crown (R'active) (m/min)}
#' \item{critCess}{a data frame of critical values for cessation of crown fire including: canopy base height (m), O'cessation (m/min)}
#' @author Justin P Ziegler, \email{justin.ziegler@@colostate.edu}
#' @references
#' Rothermel, R.C. 1972. A mathematical model for predicting fire spread in wildland fuels. \emph{INT-RP-115}. USDA Forest Service Intermountain Forest & Range Experimental Station.\cr
#' Van Wagner, C.E. 1977. Conditions for the start and spread of crown fire. \emph{Canadian Journal of Forest Research} \strong{7}:23–34.\cr
#' Rothermel, R.C., 1991. Predicting behavior and size of crown fires in the northern Rocky Mountains. \emph{INT-RP-438}. USDA Forest Service Intermountain Research Station.\cr
#' Van Wagner, C.E. 1993. Prediction of crown fire behavior in two stands of jack pine. \emph{Canadian Journal of Forest Research} \strong{23}:442–449.\cr
#' Finney, M.A. 1998. FARSITE: Fire area simulator — model development and evaluation. \emph{RMRS-RP-47}. USDA Forest Service Rocky Mountain Research Station. \cr
#' Scott, J.H., Reinhardt, E.D. 2001. Assessing crown fire potential by linking models of surface and crown fire behavior. \emph{RMRS-RP-29}. USDA Forest Service Rocky Mountain Research Station.\cr
#' @examples
#' library(firebehavioR)
#'
#'
#' @export

nexus = function(surfFuel, moisture, crownFuel, enviro, rosMult = 1, cfbForm = "f") {
  if (is.vector(surfFuel))  {surfFuel=data.frame(t(surfFuel)) }
  if (is.vector(moisture))  {moisture=data.frame(t(moisture))}
  if (is.vector(crownFuel)) {crownFuel=data.frame(t(crownFuel))}
  if (is.vector(enviro))    {enviro=data.frame(t(enviro))}
  if (nrow(surfFuel)==1)
  { surfFuel = rbind(surfFuel,surfFuel)
  moisture = rbind(moisture,moisture)
  crownFuel = rbind(crownFuel,crownFuel)
  enviro = rbind(enviro,enviro)
  flag=1
  }
enviro=enviro[,2]*16.66

    w = cbind(surfFuel[, 2], surfFuel[, 3], surfFuel[, 4], surfFuel[, 5], surfFuel[,
        6], surfFuel[, 7])/48.82744537
    delta = surfFuel[, 14] * 0.0328084
    mx.dead = surfFuel[, 15]/100
    s = cbind(surfFuel[, 8], surfFuel[, 9], surfFuel[, 10], surfFuel[, 11], surfFuel[,
        12], surfFuel[, 13]) * 0.3048
    lt = ifelse(moisture[, 5] <= 30, 1, ifelse(moisture[, 5] >= 120, 0, 1 - (moisture[,
        5] - 30)/90))
    s[, 2] = ifelse(surfFuel[, 1] == "D", ((w[, 2] * s[, 2]/32) * s[, 2] + ((w[,
        5] * lt) * s[, 5]/32) * s[, 5])/((w[, 2] * s[, 2]/32) + ((w[, 5] * lt) *
        s[, 5]/32)), s[, 2])
    w[, 2] = ifelse(surfFuel[, 1] == "D", w[, 2] + w[, 5] * lt, w[, 2])
    w[, 5] = ifelse(surfFuel[, 1] == "D", w[, 5] - w[, 5] * lt, w[, 5])
    givens = data.frame(deg2pi = 180/pi, beta.pr = 0.01725, C = 0.00222608285654313,
        E = 0.379512434370536, B = 1.43082563247299, bopt = 0.00734785937985982,
        rho = 0.552, f.dead = 0.679760888129804, f.1hr = 0.942211055276382, f.10hr = 0.0342336683417085,
        f.100hr = 0.0235552763819095, f.herb = 1, f.herb = 0.91210514954509, orv = 12.6743596286678,
        h = 8000, ns = 0.417396927909391, wn.dead = 0.130900461997487, wn.live = 0.086894,
        mx.dead = 0.25, wn.1hr = 0.130341, wn.10hr = 0.086894, wn.100hr = 0.217235,
        exp.w.1hr = 0.933326680078202, exp.w.10hr = 0.281941677764465, exp.w.100hr = 0.0100518357446336,
        w.1hr = 0.138, w.10hr = 0.092, w.100hr = 0.23, w.live = 0.092, exp.w1.live = 0.716531310573789,
        exp.w2.live = 0.91210514954509, f.live = 0.320239111870196, bbopt = 2.34762249904803,
        pp = 32, se = 0.01, ns.mineral = 0.174 * 0.01^-0.19, st = 0.0555)
    rosMult = rep(rosMult, nrow(surfFuel))
    cfbForm = ifelse(cfbForm == "sr", 1, ifelse(cfbForm == "w", 2,
        3))
    cfbForm = rep(cfbForm, nrow(surfFuel))
    mf.dead.FM10 = (givens$exp.w.1hr * moisture[, 2] * givens$wn.1hr + givens$exp.w.10hr *
        moisture[, 3] * givens$wn.10hr + givens$exp.w.100hr * moisture[, 4] * givens$wn.100hr)/as.vector(100 *
        crossprod(c(givens$exp.w.1hr, givens$exp.w.10hr, givens$exp.w.100hr), c(givens$wn.1hr,
            givens$wn.10hr, givens$wn.100hr)))
    w.dead.FM10 = (givens$w.1hr * givens$exp.w.1hr + givens$w.10hr * givens$exp.w.10hr +
        givens$w.100hr * givens$exp.w.100hr)/(givens$w.live * givens$exp.w1.live)
    fm.dead.FM10 = (moisture[, 2] * givens$f.1hr + moisture[, 3] * givens$f.10hr +
        moisture[, 4] * givens$f.100hr)/100
    fm.live.FM10 = moisture[, 5] * givens$f.herb/100
    mx.live.FM10 = apply(cbind(2.9 * as.vector(w.dead.FM10) * (1 - mf.dead.FM10/as.vector(givens$mx.dead)) -
        0.226), 1, FUN = max)
    m.ratio.FM10 = fm.dead.FM10/givens$mx.dead
    live.m.ratio.FM10 = apply(cbind(fm.live.FM10/mx.live.FM10, 1), 1, FUN = min)
    qig.FM10 = 250 + 11.16 * moisture[, 2:5]
    nm.dead.FM10 = 1 - 2.59 * m.ratio.FM10 + 5.11 * m.ratio.FM10^2 - 3.52 * m.ratio.FM10^3
    nm.live.FM10 = 1 - 2.59 * live.m.ratio.FM10 + 5.11 * live.m.ratio.FM10^2 - 3.52 *
        live.m.ratio.FM10^3
    fme = ((1000 * (1.5 - 0.00275 * crownFuel[, 2])^4)/(460 + 25.9 * crownFuel[,
        2]))/0.735907
    xi.FM10 = 0.0483170629985716
    ir.fm10 = givens$orv * givens$h * givens$ns * (nm.dead.FM10 * givens$wn.dead +
        nm.live.FM10 * givens$wn.live)
    e.qig.fm10 = givens$f.dead * (givens$f.1hr * givens$exp.w.1hr * qig.FM10[, 1] +
        givens$f.10hr * givens$exp.w.10hr * qig.FM10[, 2] + givens$f.100hr * givens$exp.w.100hr *
        qig.FM10[, 3]) + givens$f.live * (givens$f.herb * givens$exp.w2.live * qig.FM10[,
        4])
    r.active = 3/crownFuel[, 1]
    cos.f = cos(enviro[, 3]/givens$deg2pi)
    sf.fm10 = 5.275 * givens$beta.pr^-0.3 * (enviro[, 1]/100)^2
    sin.f = sin(enviro[, 3]/givens$deg2pi)
    nwns.CI = ((r.active * 3.281 * givens$rho * e.qig.fm10)/(ir.fm10 * xi.FM10 *
        3.34 * fme * rosMult)) - 1
    cos.CI = (-2 * cos.f * sf.fm10 + sqrt((4 * cos.f^2 * sf.fm10^2) - (4 * (sin.f^2 +
        cos.f^2) * (sf.fm10^2 - nwns.CI^2))))/(2 * (sin.f^2 + cos.f^2))
    sin.CI = (-2 * cos.f * sf.fm10 - sqrt((4 * cos.f^2 * sf.fm10^2) - (4 * (sin.f^2 +
        cos.f^2) * (sf.fm10^2 - nwns.CI^2))))/(2 * (cos.f^2 + cos.f^2))
    CI = ((apply(cbind(cos.CI, sin.CI, 0), 1, max)/(givens$C * (givens$beta.pr/givens$bopt)^-givens$E))^(1/givens$B)) *
        26.82248/(0.4 * 88)
    a = w * s
    wn = w * (1 - givens$st)
    a.live = apply(cbind(a[, 5] + a[, 6], 10e-10), 1, max)
    a.dead = apply(cbind(a[, 1] + a[, 2] + a[, 3] + a[, 4], 10e-10), 1, max)
    exp.w = cbind(exp(-138/s[, c(1:4)]), exp(-500/s[, c(5, 6)]), exp(-138/s[, c(5,
        6)]))
    f.dead = a.dead/(a.dead + a.live)
    f = cbind(a[, c(1:4)]/a.dead, a[, c(5:6)]/a.live)
    qig = 250 + 11.16 * moisture[, c(1:6)]
    f.live = a.live/(a.dead + a.live)
    s.tot = rowSums(s[, 1:4] * f[, 1:4]) * f.dead + rowSums(s[, 5:6] * f[, 5:6]) *
        f.live
    rhob = rowSums(w)/delta
    beta.pr = rhob/givens$pp
    mf.dead = rowSums(moisture[, 1:4] * f[, 1:4])
    nm.dead = 1 - 2.59 * (mf.dead/mx.dead * 0.01) + 5.11 * (mf.dead/mx.dead * 0.01)^2 -
        3.52 * (mf.dead/mx.dead * 0.01)^3
    Wprime = rowSums(w[, 1:4] * exp.w[, 1:4])/apply(cbind(rowSums(w[, 5:6] * exp.w[,
        5:6]), 10e-10), 1, max)
    m.dead = rowSums(exp.w[, 1:4] * moisture[, 1:4] * wn[, 1:4])/(100 * rowSums(exp.w[,
        1:4] * wn[, 1:4]))
    mx.live = apply(cbind((2.9 * Wprime * (1 - m.dead/mx.dead) - 0.226), mx.dead),
        1, FUN = max)
    mf.live = rowSums(moisture[, 5:6] * f[, 5:6])
    mf.live = apply(cbind((rowSums(moisture[, 5:6] * f[, 5:6])/100)/mx.live, 1),
        1, min)
    nm.live = 1 - 2.59 * mf.live + 5.11 * mf.live^2 - 3.52 * mf.live^3
    eqig = rowSums(f.dead * (f[, 1:4] * exp.w[, 1:4] * qig[, 1:4])) + rowSums(f.live *
        (f[, 5:6] * exp.w[, 7:8] * qig[, 5:6]))
    sf = 5.275 * beta.pr^-0.3 * (enviro[, 1]/100)^2
    xi = (((192 + 0.2595 * s.tot)^-1) * (exp((0.792 + 0.681 * s.tot^0.5) * (beta.pr +
        0.1))))
    c = (7.47 * exp(-0.133 * s.tot^0.55))
    b = (0.02526 * s.tot^0.54)
    bopt = 3.348 * s.tot^-0.8189
    bbopt = beta.pr/bopt
    e = (0.715 * exp(-0.000359 * s.tot))
    A = 133 * s.tot^-0.7913
    gamma.max = s.tot^1.5 * (495 + 0.0594 * s.tot^1.5)^-1
    orv = gamma.max * bbopt^A * exp(A * (1 - bbopt))
    wf = c * (3.2808 * enviro[, 4] * enviro[, 2])^b * bbopt^-e
    nwCI = c * (CI * 88 * enviro[, 4]/26.82248)^b * bbopt^-e
    nwnsCI = ((nwCI * sin(enviro[, 3]/givens$deg2pi))^2 + ((nwCI * cos.f + sf))^2)^0.5
    res.time = 384/s.tot
    ir = orv * surfFuel[, 16]/2.327795 * givens$ns.mineral * (nm.dead * rowSums(wn[,
        1:4] * f[, 1:4]) + nm.live * rowSums(wn[, 5:6] * f[, 5:6]))
    FI.init = (crownFuel[, 3] * (460 + 25.9 * crownFuel[, 2]) * 0.01)^1.5
    hpua = res.time * ir
    nwnsTI = apply(cbind(((60 * (FI.init/3.4591) * rhob * eqig)/(hpua * xi * ir)) -
        1, 0), 1, max)
    wf.ti = matrix(apply(cbind((4 * cos.f^2 * sf^2) - (4 * (sin.f^2 + cos.f^2) *
        (sf^2 - nwnsTI^2)), 0), 1, max))
    wf.ti = cbind(wf.ti, matrix(-2 * cos.f * sf + sqrt(wf.ti[, 1]))/(2 * (sin.f^2 +
        cos.f^2)), matrix(-2 * cos.f * sf - sqrt(wf.ti[, 1]))/(2 * (sin.f^2 + cos.f^2)))
    nwns.ROS = ir * xi/(rhob * eqig * 3.281)
    wsf = ((wf * sin(enviro[, 3]/givens$deg2pi))^2 + ((wf * cos.f + sf))^2)^0.5
    r.sa = nwns.ROS * (1 + nwnsCI)
    ros.init = (FI.init/3.4591)/hpua * 18.2881
    TI = ((apply(cbind(wf.ti[, 2], wf.ti[, 3], 0), 1, max)/(c * (beta.pr/bopt)^-e))^(1/b)) *
        26.82248/(enviro[, 4] * 88)
    r.sa.a = -log(0.1)/max(r.sa - ros.init, 0.00001)
    ros.surf = nwns.ROS * (1 + wsf)
    cfb.exp = ifelse(enviro[, 2] > TI, 1 - exp(-r.sa.a * (ros.surf - ros.init)),
        0)
    cfb.lin = ifelse(enviro[, 2] > TI, ifelse(enviro[, 2] > CI, 1, (ros.surf - ros.init)/(r.sa -
        ros.init)), 0)
    cfb.farsite = ifelse(enviro[, 2] > TI, 1 - exp(-(-log(0.1)/((r.active - ros.init) *
        0.9) * (ros.surf - ros.init))), 0)
    cfb.final = ifelse(cfbForm == "sr", cfb.lin, ifelse(cfbForm == "w", cfb.exp, cfb.farsite))
    wf.crown = givens$C * (3.2808 * enviro[, 2] * 0.4)^givens$B * givens$bbopt^-givens$E
    nwns.ROS.crown = (ir.fm10 * xi.FM10 * rosMult)/(givens$rho * e.qig.fm10 * 3.281) *
        3.34 * fme
    wsf.crown = ((wf.crown * sin(enviro[, 3]/givens$deg2pi))^2 + ((wf.crown * cos.f +
        sf.fm10))^2)^0.5
    ros.crown = nwns.ROS.crown * (1 + wsf.crown)
    type = ifelse(cfb.final < 0.9, ifelse(cfb.final < 0.1, ifelse(ros.surf <= ros.init &
        ros.crown > r.active, "conditional", "surface"), "passive"), "active")
    ROS.final = ifelse(cfbForm == "sr" | cfbForm == "w", ifelse(type == "active" | type ==
        "passive", ros.surf + cfb.final * (ros.crown - ros.surf) * rosMult, ros.surf),
        ifelse(type == "active", ros.crown * rosMult, ros.surf))
    hpua.tot = crownFuel[, 4] * 0.20482 * 18000 * 0.430265
    hpua.final = hpua + hpua.tot * cfb.final
    FI = hpua.final * 11.349 * ROS.final/60
    FL = (0.0775 * FI^0.46) + cfb.final * ((0.060957 * (FI/3.459)^(2/3)) - (0.0775 *
        FI^0.46))
    u.canopy = ifelse(wsf <= 0, 0, (((wsf/c)^(1/-e))/bbopt)^(1/(b/-e))/88)
    u.eff.crown = ifelse(wsf.crown == 0, 0, (((wsf.crown/givens$C)^(1/-givens$E))/givens$bbopt)^(1/(givens$B/-givens$E))/88)
    u.eff = (u.eff.crown - u.canopy) * cfb.final + u.canopy
    theta = ifelse(u.eff <= 0, 0, ifelse(wf * sin(enviro[, 3]/givens$deg2pi) >= 0,
        acos((wf * cos.f + sf)/wsf) * givens$deg2pi, 360 - acos((wf * cos.f + sf)/wsf) *
            givens$deg2pi))
    ScorchHt = (63/(60)) * ((FI/3.459143)^(7/6)/((FI/3.459143) + u.canopy^3)^0.5)
    nwnsOI = ((ros.init * 3.281 * givens$rho * e.qig.fm10)/(ir.fm10 * xi.FM10 * 3.34 *
        fme * rosMult)) - 1
    nwnsOI = cbind(nwnsOI, (-2 * cos.f * sf.fm10 + sqrt((4 * cos.f^2 * sf.fm10^2) -
        (4 * (sin.f^2 + cos.f^2) * (sf.fm10^2 - nwnsOI^2))))/(2 * (sin.f^2 + cos.f^2)),
        (-2 * cos.f * sf.fm10 - sqrt((4 * cos.f^2 * sf.fm10^2) - (4 * (sin.f^2 +
            cos.f^2) * (sf.fm10^2 - nwnsOI^2))))/(2 * (sin.f^2 + cos.f^2)))
    oi = ifelse(crownFuel[, 1] > 0, ((apply(cbind(nwnsOI[, 2], nwnsOI[, 3], 0), 1,
        max)/(givens$C * (givens$beta.pr/givens$bopt)^-givens$E))^(1/givens$B)) *
        26.82248/(0.4 * 88), 0)
    SI = ifelse(crownFuel[, 1] == 0, "NA", ifelse(nwnsOI[, 1] < 0, apply(cbind(0,
        CI), 1, max), apply(cbind(oi, CI), 1, max)))
    Crit_FL = (0.0775 * FI.init^0.46)
    ROS.final = ifelse(ROS.final <= 0, 0, ROS.final)
    hpua.final = ifelse(hpua.final <= 0, 0, hpua.final)
    FI = ifelse(ROS.final <= 0, 0, FI)
    FL = ifelse(ROS.final <= 0, 0, FL)
    theta = ifelse(ROS.final <= 0, 0, theta)
    ScorchHt = ifelse(ROS.final <= 0, 0, ScorchHt)
    res.time = ifelse(ROS.final <= 0, 0, res.time)
    fireBehavior = data.frame(type, cfb.final * 100, ROS.final, hpua.final * 11.35653,
        FI, FL, theta, ScorchHt/3.28084, TI, CI, SI, u.eff * 26.8224, res.time)
    names(fireBehavior) = c("Type of Fire", "Crown Fraction Burned [%]", "Rate of Spread [m/min]",
        "Heat per Unit Area [kW/m2]", "Fireline Intensity [kW/m]", "Flame Length [m]",
        "Direction of max spread [deg]", "Scorch height [m]", "Torching Index [m/min]",
        "Crowning Index [m/min]", "Surfacing Index [m/min]", "Effective Midflame Wind [m/min]",
        "Flame Residence Time [min]")
    detailSurface = data.frame(ros.surf, nwns.ROS, sf, wf, mf.dead, mf.live,
        s.tot * 3.28084, rhob * 16.0184634, beta.pr, bbopt, ir * 0.189146667, ir *
            xi * wsf * 0.189146667, rhob * eqig * 37.2589)
    names(detailSurface) = c("Potential ROS [m/min]", "No wind, no slope ROS [m/min]",
        "Slope factor [0-1]", "Wind factor [0-1]", "Characteristic dead fuel moisture [%]",
        "Characteristic live fuel moisture [%]", "Characteristic SAV [m2/m3]", "Bulk density [kg/m3]",
        "Packing ratio [-]", "Relative packing ratio [-]", "Reaction intensity [kW/m2]",
        "Heat source [kW/m2]", "Heat sink [kJ/m3]")
    detailCrown = data.frame(ros.crown, nwns.ROS.crown, sf.fm10, wf.crown,
        fm.dead.FM10, fm.live.FM10, 5788.491, givens$rho * 16.0184634, givens$beta.pr,
        givens$bbopt, ir.fm10 * 0.189146667, ir.fm10 * xi.FM10 * wsf.crown * 0.189146667,
        givens$rho * e.qig.fm10 * 37.2589)
    names(detailCrown) = c("Potential ROS [m/min]", "No wind, no slope ROS [m/min]",
        "Slope factor [0-1]", "Wind factor [0-1]", "Characteristic dead fuel moisture [%]",
        "Characteristic live fuel moisture [%]", "Characteristic SAV [m2/m3]", "Bulk density [kg/m3]",
        "Packing ratio [-]", "Relative packing ratio [-]", "Reaction intensity [kW/m2]",
        "Heat source [kW/m2]", "Heat sink [kJ/m3]")
    critInit = data.frame(FI.init, 0.0775 * FI.init^0.46, ros.init, (100 *
        (11.349 * ros.surf * hpua/60)^(2/3))/(460 + 25.9 * crownFuel[, 2]))
    names(critInit) = c("Fireline Intensity [kW/m)", "Flame length (m)", "Surface ROS [m/min]",
        "Canopy base height [m]")
    critActive = data.frame(3/ros.crown, r.active)
    names(critActive) = c("Canopy bulk density [kg/m3]", "ROS, crown (R'active) [m/min]")
    critCess = data.frame((100 * (11.349 * ros.crown * hpua/60)^(2/3))/(460 +
        25.9 * crownFuel[, 2]), oi)
    names(critCess) = c("Canopy base height [m]", "O'cessation [m/min]")
    if (flag==1) {
      fireBehavior=fireBehavior[1,]
      detailSurface=detailSurface[1,]
      detailCrown=detailCrown[1,]
      critInit=critInit[1,]
      critActive=critActive[1,]
      critCess=critCess[1,]
      }
    output = list(fireBehavior = fireBehavior, detailSurface = detailSurface,
        detailCrown = detailCrown, critInit = critInit,
        critActive = critActive, critCess = critCess)

    return(output)
}
