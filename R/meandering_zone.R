#' The meandering
#'
#' @param angle_zon The output of \link[CartesianBehaviour]{angle_zone} or \link[CartesianBehaviour]{g_angle_zone}.
#' @param dist_zone The output of \link[CartesianBehaviour]{dist_zone} or \link[CartesianBehaviour]{g_dist_zone}.
#' @param dist_reescale Will divide the distances to re-scale it.
#'
#' @return A data frame with the meandering of the subject(s).
#' @export
#'
#' @examples #Creating an trajectory using the function from the package trajr
#' if("trajr" %in% rownames(installed.packages()) == FALSE){
#' install.packages("trajr")}else{library(trajr)
#' }
#'
#' TRAJ <- list()
#' for(i in 1:3){
#' TRAJ[[i]] <- TrajGenerate(10000, random = TRUE, stepLength = .01,
#'                           angularErrorSd = .8, fps = 30)
#'}
#'
#' #Create the zones of interest
#' #Can be manually drawn by setting Zones = NA
#' #but in this case is made by code directly
#' Z1 <- base::data.frame(x = c(-12,0,0,-12), y = c(10,10,0,0))
#' Z2 <- base::data.frame(x = c(0,12,12,0),   y = c(10,10,0,0))
#' Z3 <- base::data.frame(x = c(0,12,12,0),   y = c(0,0,-10,-10))
#' Z4 <- base::data.frame(x = c(-12,0,0,-12), y = c(0,0,-10,-10))
#' ZN <- list(Z1, Z2, Z3, Z4)
#' Zonas <- list(ZN)
#' rm(Z1, Z2, Z3, Z4, ZN)
#'
#' #Perform all the mesures
#'
#'   #For one subject
#'   TRJ <- Allbasics(TRAJ[[1]], Xaxi = "x", Yaxi = "y", time = "time",
#'                     Zones = Zonas, threshold = 3,
#'                     Dist.reg = "point",
#'                     reg = data.frame(x = 0, y = 0))
#'
#'    #For multiple subjects
#'    TRAJS2D <- list() #create an empty list to be filed with the trajectories
#'    for (i in 1:length(TRAJ)) {
#'
#'    TRAJS <- Allbasics(TRAJ[[i]], Xaxi = "x", Yaxi = "y", time = "time",
#'                     Zones = Zonas, threshold = 3,
#'                     Dist.reg = "point",
#'                     reg = data.frame(x = 0, y = 0))
#'    Tj <- list(TRAJS)
#'    TRAJS2D <- append(TRAJS2D, Tj)
#'
#' }
#' rm(Tj, Zonas, TRAJS, i, TRAJ) #remove extra information
#'
#' #Create the data frame of the meandering
#' #inside of the zones for one subject
#' Angle.Zones <- angle_zone(TRJ)
#' Distance.Zones <- dist_zone(TRJ)
#'
#' Meandering.Speed.Zones <- meandering_zone(Angle.Zones,
#'                                           Distance.Zones)
#' #View(Meandering.Speed.Zones)
#'
#' #Create the data frame of the meandering
#' #of the zones for a group of subjects
#' Group.Angle.Zones <- g_angle_zone(TRAJS2D)
#' Group.Distance.Zones <- g_dist_zone(TRAJS2D)
#'
#' Group.Meandering.Speed.Zones <- meandering_zone(Group.Angle.Zones,
#'                                                 Group.Distance.Zones)
#' #View(Group.Meandering.Speed.Zones)
meandering_zone <- function(angle_zon, dist_zone, dist_reescale = 1){

  if("Animal" %in% base::colnames(dist_zone)){

    www <- base::data.frame()
    for(i in  1:base::nrow(dist_zone)){
      AAA <- base::data.frame(dist_zone[dist_zone$Animal == base::paste("Animal_", i, sep = ""),])
      AAA <- AAA[1:(base::ncol(AAA)-1)]
      BBB <- base::data.frame(angle_zon[angle_zon$Animal == base::paste("Animal_", i, sep = ""),])
      BBB <- BBB[1:(base::ncol(BBB)-1)]
      CCC <- BBB/(AAA/dist_reescale)
      www <- base::rbind(www, CCC)
      www
    }

    namezon <- base::c(base::paste("Meandering in Zone", 1:(base::ncol(www)-1), sep=""), "Total meandering")
    base::colnames(www) <- namezon
    www$Animal <- base::c(paste("Animal_", 1:base::nrow(www), sep = ""))
    www

  }else{

    distzon <- base::data.frame(dist_zone[1,])
    anglezon <- base::data.frame(angle_zon[1,])
    menadering <- anglezon/(distzon*dist_reescale)

    namezon <- base::c(base::paste("Meandering in Zone", 1:(base::ncol(distzon)-1), sep=""), "Mean meandering")
    base::colnames(menadering) <- namezon
    menadering
  }
}
