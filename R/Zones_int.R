#' Create zones of interest
#'
#' @param nzonesd2 The number of zones, for 2D or the XY dimension for 3D analysis.
#' @param nzonesd3 The number of zones To be drawn in the Z and complementary axis. Note that the 3D zones will be a  combination of the X-Y and Z-complementary dimensions. Also is impotent to note that the 2D zones will be generated and analyzed.
#' @param faceZ The complementary axis which will be used to draw the zones in the 3D analysis. Input 1 for x, 2 for y.
#' @param maxX The maximum  value on the X-axis in which movement was recorded.
#' @param minX The minimum value on the X-axis in which the movement was recorded.
#' @param maxY The maximum value on the Y-axis in which movement was recorded.
#' @param minY The minimum value on the Y-axis in which the movement was recorded.
#' @param maxZ The maximum value on the Z-axis in which movement was recorded.
#' @param minZ The minimum value on the Z-axis in which the movement was recorded.
#' @param npts Number of vertices that define the polygon representing the zone of interest.
#' @param Circ_Arena If TRUE draws a circle to represent an arena where the movement was recorded. FALSE by default.
#'
#' @return A list containing the 1 (for 2d) or 2 (for 3d) data frames containing the vertices of the zones of interest. The polygon is made by pointing in the plot the points, then a pop-up allows editing the inputs of points.
#' @export Zones_int
#'
#' @examples
#' #Create 3 2D zones of interest in a arena with measuring 20x10
#' #and Cartesian origin in the center of the arena
#' #zons2D <- Zones_int(nzonesd2 = 3, maxX = 10, minX = -10, maxY = 5,
#' #                    minY = -5, npts = 4, Circ_Arena = TRUE)
#' #Create 2 zones of interest in the XY axis and 4 zones in the ZX axis in
#' #an arena with measuring 20x10x14 and Cartesian origin in the center of the arena
#' #zons3D <- Zones_int(nzonesd2 = 2, nzonesd3 = 4, faceZ = 1, maxX = 10,
#' #                    minX = -10, maxY = 5, minY = -5, maxZ = 7, minZ = -7, npts = 4)
Zones_int <- function(nzonesd2, nzonesd3 = 0, faceZ, maxX, minX, maxY, minY, maxZ, minZ, npts = 4, Circ_Arena = FALSE){
  X <- base::c(minX, maxX, maxX, minX, ((minX+maxX)/2))
  Y <- base::c(minY, minY, maxY, maxY, ((minY+maxY)/2))
  base::plot(X, Y, xlim = base::c(minX, maxX), ylim = base::c(minY, maxY))
  if(Circ_Arena){
    R <- stats::dist(matrix(c(maxX, minX, minY, minY), ncol = 2))
    plotrix::draw.circle(x=((minX+maxX)/2), y=((minY+maxY)/2), radius= R /2)
  }
  graphics::grid(nx = (base::abs(minX)+base::abs(maxX)), ny = (base::abs(minY)+base::abs(maxY)), lty = 2, col = "gray", lwd = 2)
  AA <- base::list()
  for (i in 1:nzonesd2) {
    pts <- graphics::locator(npts)
    graphics::polygon(pts)
    BB <- base::list(base::data.frame(pts))
    AA <- base::append(AA, BB)
  }

  for (i in 1:base::length(AA)) {
    AA[[i]] <- utils::edit(AA[[i]])
  }

  if(nzonesd3 > 0){
    CC <- base::list()

    if(faceZ == 1){
      Z <- base::c(minZ, minZ, maxZ, maxZ, ((minZ+maxZ)/2))
      base::plot(X, Z, xlim = c(minX, maxX), ylim = c(minZ, maxZ))
      graphics::grid(nx = (base::abs(minX)+base::abs(maxX)), ny = (base::abs(minZ)+base::abs(maxZ)), lty = 2, col = "gray", lwd = 2)

      for (i in 1:nzonesd3) {
        pts <- graphics::locator(npts)
        graphics::polygon(pts)
        DD <- base::data.frame(pts)
        base::colnames(DD) <- c("x","z")
        DD <- base::list(DD)
        CC <- base::append(CC, DD)
        CC
      }
    }else if(faceZ == 2){
      Z <- base::c(minZ, maxZ, maxZ, minZ, ((minZ+maxZ)/2))
      base::plot(Y, Z, xlim = c(minY*1.2, maxY*1.2), ylim = base::c(minZ*1.2, maxZ*1.2))
      graphics::grid(nx = (base::abs(minY)+base::abs(maxY)), ny = (base::abs(minZ)+base::abs(maxZ)), lty = 2, col = "gray", lwd = 2)

      for (i in 1:nzonesd3) {
        pts <- graphics::locator(npts)
        graphics::polygon(pts)
        DD <- base::data.frame(pts)
        base::colnames(DD) <- c("y","z")
        DD <- base::list(DD)
        CC <- base::append(CC, DD)
        CC
      }
    }else{
      stop("select complementar dimention to Z, can be 1 for 'x' or 2 for 'y'",
           call. = FALSE)
    }
    for (i in 1:length(CC)) {
      CC[[i]] <- utils::edit(CC[[i]])
    }
    WW <- base::list(AA,CC)
  }else{
    WW <- base::list(AA)
  }

  WW

}
