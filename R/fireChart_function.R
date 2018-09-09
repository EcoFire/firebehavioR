#' Fire Characteristics Chart
#'
#' Visualization of predicted fire behavior
#' @param name a character vector identifying names of predictions
#' @param hpua a numeric vector of heat per unit area (kJ/m2)
#' @param ros a numeric vector of rate of spread (m/min)
#' @return an object of class ggplot
#' @author Justin P Ziegler, \email{justin.ziegler@@colostate.edu}
#' @references
#' Andrews, P.L. & Rothermel, R.C. 1982. Charts for interpreting wildland fire behavior characteristics.
#' \emph{INT-GTR-131}. USDA Forest Service Intermountain Forest & Range Experimental Station.
#' @examples
#' fc = fireChart('fire',hpua = 15000, ros = 50)
#' print(fc)
#' @import ggplot2
#' @importFrom utils data
#' @export
fireChart <- function(name, hpua, ros) {
  fc <- ggplot(data = fireChartData, aes_string(
    x = fireChartData$h,
    y = fireChartData$r, z = fireChartData$fl
  )) + coord_cartesian(expand = F) +
    geom_contour(color = "black", lwd = 1, breaks = c(
      1.2,
      2.4, 3.4
    )) + theme_minimal() + annotate("text",
      x = 5000,
      y = c(6, 23, 49, 97), label = c(
        "1.2", "2.4", "3.4",
        "Flame length (m)"
      )
    ) + annotation_custom(bulldozer,
      xmin = 2000, ymin = 20, xmax = 3000, ymax = 25
    ) + annotation_custom(firefighter,
      xmin = 200, ymin = 5, xmax = 1200, ymax = 10
    ) + annotation_custom(tree,
      xmin = 4000, ymin = 30, xmax = 5000, ymax = 35
    ) + annotation_custom(plume,
      xmin = 6500, ymin = 50, xmax = 7500, ymax = 55
    ) + labs(x = bquote("Heat per unit area (kJ/" ~
    m^2 ~ ")"), y = "Rate of spread (m/min)") + geom_point(aes(
      x = hpua,
      y = ros
    ), pch = 24, fill = "red") + annotate("text",
      x = hpua, y = ros, label = name, vjust = -1
    )
  return(fc)
}
