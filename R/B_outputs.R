#' Our Meneu
#'
#'All the outputs possible. If you will use all outputs just type Allbasics( ) and set correctly all parameters. For multiple data frames list them and add g_ to any of the functions.
#'
#' @return A dataframe with the implemented outputs
#' @export
#'
#' @examples #NOT RUN
#' # B_outputs()
B_outputs <- function(){
  Gugu <- data.frame(
   Output = c("ABS Turn angle",
  "Total Distance",
  "Latency",
  "Speed",
  "Distance to Point",
  "Distance to line",
  "Distance to plane",
  "Transitions between zones",
  "Minimun speed",
  "Maximun speed",
  "Under speed episdes",
  "Under speed time",
  "Time in zone",
  "Meandering"),
  pre_necessary = c(
    "angles_by_frame",
    "distances_by_frame",
    "list_zones",
    "speeds_by_frame",
    "reg_dist_by_frame",
    "reg_dist_by_frame",
    "reg_dist_by_frame",
    "list_zones",
    "speeds_by_frame",
    "speeds_by_frame",
    "speeds_by_frame & threshold_speed_by_frame",
    "speeds_by_frame & threshold_speed_by_frame",
    "list_zones",
    "meandering_zone"),
  Over_tme = c(
    "over_time_ana(type = 'angle')",
    "over_time_ana(type = 'distance')",
    "Not possible",
    "over_time_ana(type = 'speed')",
    "over_time_ana(type = 'point')",
    "over_time_ana(type = 'line')",
    "over_time_ana(type = 'plane')",
    "Not developed",
    "Not developed",
    "Not developed",
    "over_time_ana(type = 'underspeed')",
    "over_time_ana(type = 'underspeed')",
    "Not developed",
    "over_time_ana(type = 'meandering')")

  )

  View(Gugu)
}
