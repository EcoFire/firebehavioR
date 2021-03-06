% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cfis_function.R
\name{cfis}
\alias{cfis}
\title{Canopy Fire Initiation & Spread model}
\usage{
cfis(fsg, u10, effm, sfc, cbd, id)
}
\arguments{
\item{fsg}{a numeric vector of fuel stratum gaps (m)}

\item{u10}{a numeric vector of 10-m open wind speeds (km/hr)}

\item{effm}{a numeric vector of effective fine fuel moistures (\%)}

\item{sfc}{a numeric vector of surface fuel consumed (Mg/ha)}

\item{cbd}{a numeric vector of canopy bulk densities (kg/m3)}

\item{id}{a numeric vector of spot ignition delays, the time during which a given firebrand generates, is transported aloft, and ignites a receptive fuelbed (min)}
}
\value{
a data frame with type of fire, probability of crown fire occurrences (\%), crown fire rate of spread (m/min), and critical spotting distance (m)
}
\description{
Prediction of crown fire probability, crown fire rate of spread and separation distance (Alexander and Cruz 2006). Separation distance is distance ahead of main fire front required for a spot fire to form, separate of a main fire.
}
\examples{
data("coForest")
# show the data format:
head(coForest)
# Predict crown fire, using coForest
# measurements and assumed weather
# parameters
df.cfis = cfis(fsg = coForest$cbh_m, u10 = 20,
    effm = 6, sfc = coForest$sfl_kgm2*10, cbd = coForest$cbd_kgm3,
    id = 1)
print(df.cfis)

# Examine differences between treatment
# statuses
aggregate(x = df.cfis$cROS, by = list(treatmentStatus = coForest$status),
    FUN = mean)
# Now, examine the sensitivity of fire
# type designations to wind speed by
# treatment status
coForest = coForest[rep(seq_len(nrow(coForest)),
    11), ]
coForest$u10 = sort(rep(10:20, 14))
coForest$type = cfis(coForest$cbh_m, coForest$u10,
    6, coForest$sfl_kgm2*10, coForest$cbd_kgm3,
    1)$type
table(u10 = coForest$u10, coForest$type,
    coForest$status)
}
\references{
Alexander M.E., Cruz M.G. 2006. Evaluating a model for predicting active crown fire rate of spread using wildfire observations. \emph{Canadian Journal of Forest Research}. \strong{36}:2015-3028.
}
\author{
Justin P Ziegler, \email{justin.ziegler@colostate.edu}
}
