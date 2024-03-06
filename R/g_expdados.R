#' Function to standardize a trajectory for a list
#'
#' A standardize data frame. Note that if Z is a vector its length must be the same as of the X and Y.
#'
#' @param list_subjects A list containing the main data frame containing trajectory data (X and Y coordinates), an identifier of the subject and a frame or time information.
#' @param dadoz A second list that contains the data frames with the z coordinates and the other identifiers.
#' @param Xaxi A character input representing the column in list_subjects presenting the X coordinates.
#' @param Yaxi A character input representing the column in dado presenting the Y coordinates.
#' @param Zaxi Can be the name of the column in dadoz, or a vector of the type double (numeric) containing the Z coordinates.
#' @param frames The column in dados representing frame information.
#' @param id Numeric identifier of the subject.
#' @param id_col The column in dados that contain the subject identifier of the observations. If there is no identifier column all the data in the data frame will be considered.
#' @param fps The frame rate or another value to convert the time marker into a desired one.
#' @param time The column in dados representing time information.
#'
#' @return A standardized list of data frames
#' @export
#'
#' @examples #Create an trajectory using the function from the package trajr
#' if("trajr" %in% rownames(installed.packages()) == FALSE){
#' install.packages("trajr")}else{library(trajr)}
#'
#' Traj <- list()
#' for(i in 1:10){
#' Traj[[i]] <- TrajGenerate(5000, random = TRUE, stepLength = .01,
#'                           angularErrorSd = .8, fps = 30)
#'
#'}
#'
#' #Export the trajectory.
#' TrajEXP <- g_expdados(Traj, Xaxi = "x", Yaxi = "y",
#'                       time = "time", id = 1, fps = 30)
#' #View(TrajEXP)
g_expdados <- function(list_subjects, dadoz = NULL, Xaxi = "x_cm", Yaxi = "y_cm",
                       Zaxi = NA, time = NA, frames = NA, id = 1, id_col = NULL, fps = 30){
  if(base::is.null(dadoz)){
    list_subjects <- base::lapply(list_subjects, FUN = expdados, dadoz = dadoz, Xaxi = Xaxi,
                                  Yaxi = Yaxi, Zaxi = Zaxi, time = time, frames = frames, id = id,
                                  id_col = id_col, fps = fps)
    list_subjects
  }else if(base::typeof(dadoz) == "list"){
    if(base::length(list_subjects) == base::length(dadoz)){
      for (i in 1:base::length(dadoz)) {

        list_subjectsz <- CartesianBehaviour::expdados(list_subjects[[i]], dadoz = dadoz[[i]], Xaxi = Xaxi,
                                   Yaxi = Yaxi, Zaxi = Zaxi, time = time, frames = frames, id = id,
                                   id_col = id_col, fps = fps)

        list_subjects[[i]] <- list_subjectsz

      }
      list_subjects
    }
  }else{stop("Incorrect input. The dadoz and list_subjects must have the same length.
              Check if you really have dadoz observations or else set dadoz = NULL",
             call. = FALSE)}
}
