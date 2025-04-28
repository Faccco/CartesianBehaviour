#' By frame distances to a Point/Line/Plane
#'
#' Add the \link[CartesianBehaviour]{list_zones} output with the distances to a defined point/line/plane in each frame, necessary to perform \link[CartesianBehaviour]{over_time_ana}.
#'
#' @param list_sample The output of \link[CartesianBehaviour]{list_zones}.
#' @param Dist.reg The definition to evaluate the distance to a "point", "line" or "plane". A points define by a singular location a line by two point and a plane by three points. In 3D you need to point it in XY and (X/Y)Z axis, so point is 2 times, line is 4 and plane is 6.
#' @param faceZ An integer number indicating the dimension to consider for distance calculation (1 for 'x' or 2 for 'y').
#' @param minX The minimum coordinate in the X-axis defining the arena.
#' @param maxX The maximum coordinate in the X-axis defining the arena.
#' @param minY The minimum coordinate in the Y-axis defining the arena .
#' @param maxY The maximum coordinate in the Y-axis defining the arena.
#' @param minZ The minimum coordinate in the Z-axis defining the arena .
#' @param maxZ The maximum coordinate in the Z-axis defining the arena.
#' @param Circ_Arena A logical indicating whether do draw a circular boundary to position the point  of interest to be considered. FALSE by default.
#' @param reg A data frame with the coordinates to a point/line/plane. "NOT" by default to manually pin the point(s) of the point/line/plane.
#'
#' @return A list with the prior data and a column with the distance to a point/line/plane
#' @export
#'
#' @examples #Creating an trajectory using the function from the package trajr
#' if("trajr" %in% rownames(installed.packages()) == FALSE){
#' install.packages("trajr")}else{library(trajr)
#' }
#' Traj <- TrajGenerate(2000, random = TRUE, stepLength = .12,
#'                      angularErrorSd = .8, fps = 30)
#'
#' #Export the trajectory.
#' TrajEXP <- standardized_trj(Traj, Xaxi = "x", Yaxi = "y", time = "time",
#'                    id = 1, fps = 30)
#'
#' #Create the zones of interest
#' #Normally by Zones_in() but in this case by code directly
#' Z1 <- data.frame(x = c(-12,0,0,-12), y = c(10,10,0,0))
#' Z2 <- data.frame(x = c(0,12,12,0),   y = c(10,10,0,0))
#' Z3 <- data.frame(x = c(0,12,12,0),   y = c(0,0,-10,-10))
#' Z4 <- data.frame(x = c(-12,0,0,-12), y = c(0,0,-10,-10))
#' ZN <- list(Z1, Z2, Z3, Z4)
#' Zonas <- list(ZN)
#' rm(Z1, Z2, Z3, Z4, ZN)
#'
#' #Split by the zones
#'
#' TRAJ2D <- list_zones(TrajEXP, Zones = Zonas)
#'
#' #If you want to see how do the trajectory look like
#' #remove hash sign below and run
#' #plot(Traj$x,TrajEXP$y)
#'
#' #Add the distance to a point of interest by each frame this example
#' #pre-define the point but you can turn the Dist.reg parameter to
#' #"line" or "plane" and reg to "NOT", set the arena measures and
#' #manually pin the  point in the Plot tab
#' TRAJ2D <- reg_dist_by_frame(TRAJ2D, Dist.reg = "point",
#'                             reg = data.frame(x = 0, y = 0))
#' #View(TRAJ2D[[1]][["Total"]][[1]])
reg_dist_by_frame <- function(list_sample, Dist.reg = "plan", reg = "NOT", faceZ = 0, minX, maxX, minY, maxY, minZ, maxZ, Circ_Arena = F){

  rara <- base::as.numeric(base::pmatch(Dist.reg,
                                        base::c("point", "line", "plane")))

  if(rara == 3 & faceZ == 0){
    stop("Distance to a plane can only be definesd in 3 dimensions, please selec a complementar dimension to Z in faceZ or selec the line/point method in reg paramenter", call. = FALSE)
  }

  if(base::typeof(reg) == "character"){
    X <- base::c(minX, maxX, maxX, minX, ((minX+maxX)/2))
    Y <- base::c(minY, minY, maxY, maxY, ((minY+maxY)/2))
    base::plot(X, Y, xlim = base::c(minX*1.2, maxX*1.2), ylim = base::c(minY*1.2, maxY*1.2))
    if(Circ_Arena){
      R <- stats::dist(base::matrix(base::c(((minX+maxX)/2), maxX, minY, minY), ncol = 2))
      plotrix::draw.circle(x=((minX+maxX)/2), y=((minY+maxY)/2), radius= R /2)
    }
    graphics::grid(nx = (base::abs(minX)+base::abs(maxX)), ny = (base::abs(minY)+base::abs(maxY)), lty = 2, col = "gray", lwd = 2)
    AA <- base::list()

      regs <- graphics::locator(rara)
      AA <- base::append(AA, regs)

    if(faceZ != 0){

      if(faceZ == 1){
        Z <- base::c(minZ, minZ, maxZ, maxZ, ((minZ+maxZ)/2))
        base::plot(X, Z, xlim = c(minX*1.2, maxX*1.2), ylim = base::c(minZ*1.2, maxZ*1.2))
        graphics::grid(nx = (base::abs(minX)+base::abs(maxX)), ny = (base::abs(minZ)+base::abs(maxZ)), lty = 2, col = "gray", lwd = 2)

        regs <- graphics::locator(rara)
          z <- regs$y
          AA <- base::append(AA, base::data.frame(z))
          AA


      }else if(faceZ == 2){
        Z <- base::c(minZ, maxZ, maxZ, minZ, ((minZ+maxZ)/2))
        base::plot(Y, Z, xlim = base::c(minY*1.2, maxY*1.2), ylim = base::c(minZ*1.2, maxZ*1.2))
        graphics::grid(nx = (base::abs(minY)+base::abs(maxY)), ny = (base::abs(minZ)+base::abs(maxZ)), lty = 2, col = "gray", lwd = 2)

          regs <- graphics::locator(rara)
          z <- regs$y
          AA <- base::append(AA, base::data.frame(z))
          AA

          }else{
        base::stop("select complementar dimention to Z, can be 1 for 'x' or 2 for 'y'",
                   call. = FALSE)
      }

      names(AA) <- c("x","y","z")
      AA <- utils::edit(data.frame(AA))
      AA

    }else{
      AA <- utils::edit(base::data.frame(AA))
      AA
    }
  }else{
    AA <- reg
  }
Ani <- list_sample[[1]]
Anima <- Ani[[base::length(Ani)]]
Animal <- Anima[[1]]
  if(rara == 1){

    Animal$distPoint <- point_dist_by_frame(www = Animal , AA)

  }else if(rara == 2){

    Animal$distLine <- line_dist_by_frame(Anima = Animal, b = AA[1,], c = AA[2,])

    }else if(rara == 3){

      Animal$distPlane <- plane_dist_by_frame(x1 = AA[1,1], y1 = AA[1,2], z1 = AA[1,3], x2 = AA[2,1], y2 = AA[2,2], z2 = AA[2,3], x3 = AA[3,1], y3 = AA[3,2], z3 = AA[3,3], Anima = Animal)

  }
Animal -> Anima[[1]]
Anima -> Ani[[base::length(Ani)]]
Ani -> list_sample[[1]]


list_sample
}
