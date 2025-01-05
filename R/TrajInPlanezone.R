#' Check if a Trajectory is inside or outside of a polygon considering the axis
#'
#'This function is based on TrajInPolygon function from trajr and which apply the function point.in.polygon from sp.
#'
#' @param Track A data frame containing points in two of x y or z axis of a trajectory.
#' @param Zone A data frame containing points in two of x y or z axis for a polygon.
#' @param plane the axis to be considered. Can be "zy", "yz", "zx", "xz", "xy" or "yx".
#'
#' @return A vector of 0's and 1's respectively for outside or inside the polygon.
#' @export TrajInPlanezone
#'
#' @examples Traj <- matrix(rnorm(2400,0,4), ncol=3,
#' dimnames  = list(c(1:800), c("x", "y", "z")))
#' Traj <- data.frame(Traj)
#' Polxy <- data.frame(x = c(5, 5, -5, -5), y = c(4, -4, -4, 4))
#' Polyz <- data.frame(y = c(4, 4, -4, -4), z = c(7, -7, -7, 7))
#' Polxz <- data.frame(x = c(5, 5, -5, -5), z = c(7, -7, -7, 7))
#'
#' Teste1 <- data.frame(TrajInPlanezone(Traj, Polxy, "yx"))
#' #View(Teste1)
#'
#' Teste2 <- data.frame(TrajInPlanezone(Traj, Zone = Polyz, plane = "yz"))
#' #View(Teste2)
#'
#' Teste3 <- data.frame(TrajInPlanezone(Traj, Zone = Polxz, plane = "xz"))
#' #View(Teste3)
TrajInPlanezone <- function (Track, Zone, plane) {
  if (!requireNamespace("sp", quietly = TRUE)) {
    stop("Package \"sp\" is needed for the TrajInPlanezone function to work. You can install it using install.packages('sp').",
         call. = FALSE)
  }
  if (!rlang::is_installed("plotrix")) {
    stop("Package {plotrix} is needed for the TrajInPlanezone function to work. You can install it using install.packages('sp').")
  }


  # Determine the numeric code for the plane

  space <- base::as.numeric(base::pmatch(plane,
                                         base::c("zy", "yz", "zx", "xz", "xy", "yx")))

  # Extract coordinates

  if(space == 1 | space == 2){
    poly <- grDevices::xyz.coords(x = c(1:length(Zone$y)), y = Zone$y, z = Zone$z)
    sp::point.in.polygon(Track$y, Track$z, poly$y, poly$z)
  } else if(space == 3 | space == 4){
    poly <- grDevices::xyz.coords(x = Zone$x, y = c(1:length(Zone$x)), z = Zone$z)
    sp::point.in.polygon(Track$x, Track$z, poly$x, poly$z)
  } else if(space == 5 | space == 6){
    poly <- grDevices::xyz.coords(x = Zone$x, y = Zone$y, z = c(1:length(Zone$x)))
    sp::point.in.polygon(Track$x, Track$y, poly$x, poly$y)
  } else {
    stop("Invalid plane orientation. Insert any plane as: 'zy', 'yz', 'zx', 'xz', 'xy', 'yx'",
         call. = FALSE)
  }
}
