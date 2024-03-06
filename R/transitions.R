#' Number of transitions to enter a zone
#'
#' @param list_zones The output of \link[CartesianBehaviour]{list_zones} for one subject.
#'
#' @return A data frame containing the number of entrances in a defined zone of interest.
#' @export
#'
#' @examples #Creating an trajectory using the function from the package trajr
#' if("trajr" %in% rownames(installed.packages()) == FALSE){
#' install.packages("trajr")}else{library(trajr)
#' }
#' Traj <- TrajGenerate(10000, random = TRUE, stepLength = .01,
#'                      angularErrorSd = .8, fps = 30)
#'
#' #Create the zones of interest
#' #Can be manually drawn by setting Zones = NA,
#' #but in this case is made by code directly
#' Z1 <- data.frame(x = c(-12,0,0,-12), y = c(10,10,0,0))
#' Z2 <- data.frame(x = c(0,12,12,0),   y = c(10,10,0,0))
#' Z3 <- data.frame(x = c(0,12,12,0),   y = c(0,0,-10,-10))
#' Z4 <- data.frame(x = c(-12,0,0,-12), y = c(0,0,-10,-10))
#' ZN <- list(Z1, Z2, Z3, Z4)
#' Zonas <- list(ZN)
#' rm(Z1, Z2, Z3, Z4, ZN)
#'
#' #Perform all the measures
#' TRAJ2D <- Allbasics(Traj, Xaxi = "x", Yaxi = "y", time = "time",
#'                     Zones = Zonas, threshold = 3,
#'                     Dist.reg = "point",
#'                     reg = data.frame(x = 0, y = 0))
#'
#' #Create the data frame of the transitions of the subject
#' Transitions <- transitions(TRAJ2D)
#' #View(Transitions)
transitions <- function(list_zones){
  list_zone <- list_zones[[1]]
  final <- base::list()
  d <- list_zone[[base::length(list_zone)]]
  c <- base::as.numeric(d[[1]]$frame[1])

  for (i in 1:(base::length(list_zone)-1)){
    teste <- list_zone[[i]]

    if(base::length(teste) > 0){
      tisti <- teste[[1]]$frame[[1]]

      if(tisti == c){
        znx <- base::length(list_zone[[i]]) - 1
        final[[base::length(final) + 1]] <- znx

      }else{
        znx <- base::length(list_zone[[i]])
        final[[base::length(final) + 1]] <- znx
      }

    }else{
      znx <- 0
      final[[base::length(final) + 1]] <- znx
    }
  }

  final <- base::data.frame(final)

  namezon <- base::vector()
  for (i in 1:base::nrow(list_zones[[2]])) {
    namezon <- base::append(namezon, base::paste("Entries in Zone", list_zones[[2]][i,], sep=""))
  }
  base::colnames(final) <- namezon
  final

}
