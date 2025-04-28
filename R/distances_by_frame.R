#' Distance by each frame
#'
#' Add the \link[CartesianBehaviour]{list_zones} output with the distances traveled each frame, necessary to perform: \link[CartesianBehaviour]{speeds_by_frame}, \link[CartesianBehaviour]{dist_zone} and \link[CartesianBehaviour]{g_dist_zone}.
#'
#' @param list_zones The output of \link[CartesianBehaviour]{list_zones}.
#'
#' @return A list of splitted data frame added with the distances by each frame.
#' @export distances_by_frame
#'
#' @examples #Creating an trajectory using the function from the package trajr
#' if("trajr" %in% rownames(installed.packages()) == FALSE){
#' install.packages("trajr")}else{library(trajr)}
#' Traj <- TrajGenerate(2000, random = TRUE, stepLength = .12,
#'                      angularErrorSd = .8, fps = 30)
#'
#' #Export the trajectory.
#' TrajEXP <- standardized_trj(Traj, Xaxi = "x", Yaxi = "y",
#'                     time = "time", id = 1, fps = 30)
#'
#' #Create the zones of interest
#' #Normally by Zones_in() but in this case by code directly
#' Z1 <- data.frame(x = c(-12,0,0,-12), y = c(10,10,0,0))
#' Z2 <- data.frame(x = c(0,12,12,0),   y = c(10,10,0,0))
#' Z3 <- data.frame(x = c(0,12,12,0),   y = c(0,0,-10,-10))
#' Z4 <- data.frame(x = c(-12,0,0,-12), y = c(0,0,-10,-10))
#' ZN <- list(Z1, Z2, Z3, Z4)
#' Zonas <- list(ZN)
#' rm(Z1, Z2, Z3, Z4, ZN, i, Traj)
#'
#' #Split by the zones
#' TRAJ2D <- list_zones(TrajEXP, Zones = Zonas)
#'
#' #Add the distance by each frame
#' TRAJ2D <- distances_by_frame(TRAJ2D)
#' #View(TRAJ2D[[1]][["Total"]][[1]])
distances_by_frame <- function(list_zones){
  list_zone <- list_zones[[1]]
  for (j in 1:base::length(list_zone)) {

    www <- list_zone[[j]]

    if(base::length(www) > 0){
      condition <- base::c(base::colnames(www[[1]]))
      if("z" %in% condition){
        for (i in 1:base::length(www)) {

          www[[i]]$dist1 <- base::c(0, base::Mod(base::diff(base::as.complex(www[[i]]$polaryz))))
          www[[i]]$dist2 <- base::c(0, base::Mod(base::diff(base::as.complex(www[[i]]$polar))))
          www[[i]]$polardist <- base::c(base::complex(real = base::as.numeric(www[[i]]$dist1), imaginary = base::as.numeric(www[[i]]$dist2)))

          www[[i]]$dist <- base::c(base::Mod(www[[i]]$polardist))

          remo <- base::c("dist1", "dist2", "polardist", "displacement")
          www[[i]] <- www[[i]][ , !(base::names(www[[i]]) %in% remo)]

          www

        }

        www

      }else {
        for (i in 1:base::length(www)){

          www[[i]]$dist <- base::c(0, base::Mod(base::diff(base::as.complex(www[[i]]$polar))))
          remo <- base::c("displacement")
          www[[i]] <- www[[i]][ , !(base::names(www[[i]]) %in% remo)]

          www

        }
      }
    }else{

      www

    }

    list_zone[[j]] <- www

  }

  list_zones[[1]] <- list_zone
  list_zones

}
