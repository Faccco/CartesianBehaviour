# Internal function to define the times the zones of interest
#
# @param inzone The splitted data frame with the zones
#
# @return A data frames with the time in each of the incursion for the zones.
# @export TIME_ZONE
#
# @examples #See times.zone( ).
# @keywords internal
TIME_ZONE <-function(inzone, time.scale = "sec"){
  PERIOD <- base::data.frame(0)
  time <- base::as.numeric(base::pmatch(time.scale,
                                        base::c("second", "minute", "hour", "day")))
  if(base::length(inzone) > 0){
    for(i in 1:base::length(inzone)){
      www <- inzone[[i]]

      timeleng <- (base::max(base::as.numeric(www$time)) - base::min(base::as.numeric(www$time)))

      PERIOD[base::nrow(PERIOD) + 1,] <- base::c(timeleng)
      PERIOD
    }
    PERIOD <- PERIOD[2:base::nrow(PERIOD),]
    PERIOD <- base::as.matrix(PERIOD)
    if(base::is.na(time)){
      PERIOD <- PERIOD/time.scale
      PERIOD
    }else if(time == 1){
      PERIOD <- PERIOD/1
    }else if(time ==  2){
      PERIOD <- PERIOD/60
    }else if(time ==  3){
      PERIOD <- PERIOD/3600
    }else if(time ==  4){
      PERIOD <- PERIOD/86400
    }
  }else{
    PERIOD <- 0
    PERIOD
  }
  PERIOD
}
