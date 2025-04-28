#' Tracking positions from zebrafish (Danio rerio) in the Novel Tank Diving Test (NTT) of four tracking software
#'
#' A named list of data frames, each with columns:
#' \describe{
#'   \item{time}{Frame number or timestamp}
#'   \item{x}{X position (pixels or mm)}
#'   \item{y}{Y position (pixels or mm)}
#' }
#'
#' @format A list of 52 data frames Ethovision for Short‐fin(1:25) and Leopard(26:52) strains
#' @source Laboratory zebrafish tracking experiment
"Ethovision"

#' Tracking positions from ANY-maze for Short‐fin(1:25) and Leopard(26:52) strains
#'
#' @format A list of 52 data frames with columns \code{time}, \code{x}, \code{y}
"ANY_maze"

#' Tracking positions from ToxTrac for Short‐fin(1:25) and Leopard(26:52) strains
#'
#' @format A list of 47 data frames with columns \code{time}, \code{x}, \code{y}
"ToxTrac"

#' Tracking positions from TrackR for Short‐fin(1:21) and Leopard(22:47) strains
#'
#' @format A list of 50 data frames with columns \code{time}, \code{x}, \code{y}
"TrackR"
