#' Arch between three points
#'
#' @param list_zones  A data frame or list from \link[CartesianBehaviour]{expdados} or \link[CartesianBehaviour]{list_zones}, which contains x and y for the three points of interest.
#' @param sidesum Define if the interest is the absolute angle for each side of the arc of the three points or the minimum and maximum angles. TRUE for minimum and maximum, FALSE by default.
#'
#' @return A data frame or lit with the body arch.
#' @export body_arch_by_frame
#'
#' @examples #Creating an trajectory using the function from the package trajr
#' if("trajr" %in% rownames(installed.packages()) == FALSE){
#' install.packages("trajr")}else{library(trajr)}
#'
#' Traj <- TrajGenerate(10000, random = TRUE, stepLength = .01,
#'                      angularErrorSd = .8, fps = 30)
#'
#' Traj <- cbind(Traj, data.frame(headx = rnorm(Traj$x), heady = rnorm(Traj$y),
#'               tailx = rnorm(Traj$x), taily = rnorm(Traj$y)))
#'
#'
#' #Create the data frame of the archs between three points
#' Angles <- body_arch_by_frame(Traj)
#' #View(Angles)
body_arch_by_frame <- function(list_zones, sidesum = FALSE){

  if(!(is.null(base::colnames(list_zones)))){

  center <- base::complex(real = list_zones$x, imaginary = list_zones$y)
  head <- base::complex(real = list_zones$headx, imaginary = list_zones$heady)
  tail <- base::complex(real = list_zones$tailx, imaginary = list_zones$taily)

  superior <- head - center
  inferior <- tail - center

  superior <- base::Arg(superior)
  inferior <- base::Arg(inferior)

  archb <- base::list(superior, inferior)

  for (i in 1:2) {

    jojo <- base::which(archb[[i]] <= -base::pi)
    archb[[i]][jojo] <- archb[[i]][jojo] + 2 * base::pi
    jojo <- base::which(archb[[i]] > base::pi)
    archb[[i]][jojo] <- archb[[i]][jojo] - 2 * base::pi
    archb[[i]] <- archb[[i]] * 57.29578

  }

  arch <- base::abs(archb[[1]] - archb[[2]])

 arch2 <- 360 - arch

 if(sidesum){

   archT <- base::cbind(arch, arch2)
   arch1 <- base::apply(X = archT, MARGIN = 1, FUN = min)
   arch2 <- 360 - arch1

 }else{
   arch1 <- arch
 }

 list_zones$arch1 <- arch1
 list_zones$arch2 <- arch2

 list_zones

  }else{

    list_zone <- list_zones[[1]]

    for (j in 1:base::length(list_zone)) {

      www <- list_zone[[j]]

      if(base::length(www) > 0){

          for (i in 1:base::length(www)){

            center <- base::complex(real = www$x, imaginary = www$y)
            head <- base::complex(real = www$headx, imaginary = www$heady)
            tail <- base::complex(real = www$tailx, imaginary = www$taily)

            superior <- head - center
            inferior <- tail - center

            superior <- base::Arg(superior)
            inferior <- base::Arg(inferior)

            archb <- base::list(superior, inferior)

            for (i in 1:2) {

              jojo <- base::which(archb[[i]] <= -base::pi)
              archb[[i]][jojo] <- archb[[i]][jojo] + 2 * base::pi
              jojo <- base::which(archb[[i]] > base::pi)
              archb[[i]][jojo] <- archb[[i]][jojo] - 2 * base::pi
              archb[[i]] <- archb[[i]] * 57.29578

            }

            arch <- base::abs(archb[[1]] - archb[[2]])

            arch2 <- 360 - arch

            if(sidesum){

              archT <- base::cbind(arch, arch2)
              arch1 <- base::apply(X = archT, MARGIN = 1, FUN = min)
              arch2 <- 360 - arch1

            }

            www$arch1 <- arch1
            www$arch2 <- arch2

            www

          }
      }else{

        www

      }

      list_zone[[j]] <- www

    }

    list_zones[[1]] <- list_zone
    list_zones

  }

  list_zones

}
