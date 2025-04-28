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
  Gugu <- base::data.frame(
   Output = c("Absolute turn angle",
  "Total distance",
  "Latency",
  "Speed",
  "Distance to point",
  "Distance to line",
  "Distance to plane",
  "Transitions between zones",
  "Minimun speed",
  "Maximun speed",
  "Under speed episdes",
  "Under speed time",
  "Time in zone",
  "Meandering",
  "Body arch"),
  pre_necessary = c(
    "angles_by_frame()",
    "distances_by_frame()",
    "list_zones()",
    "speeds_by_frame()",
    "reg_dist_by_frame()",
    "reg_dist_by_frame()",
    "reg_dist_by_frame()",
    "list_zones()",
    "speeds_by_frame()",
    "speeds_by_frame()",
    "speeds_by_frame() & threshold_speed_by_frame()",
    "speeds_by_frame() & threshold_speed_by_frame()",
    "list_zones()",
    "meandering_zone()",
    "standardized_trj"),
  Over_time = c(
    "over_time_ana(type = 'angle')",
    "over_time_ana(type = 'distance')",
    "-",
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
    "over_time_ana(type = 'meandering')",
    "over_time_ana(type = 'arch')"),
  By_zone = c(
    "angle_zone()",
    "dist_zone()",
    "latency_zone()",
    "mean_speed_zone()",
    "Not developed",
    "Not developed",
    "Not developed",
    "transitions()",
    "MMspeed()",
    "MMspeed()",
    "under_sepisods()",
    "under_sepisods()",
    "times_zone()",
    "meandering_zone()",
    "Not developed")

  )

  utils::View(Gugu)
}
