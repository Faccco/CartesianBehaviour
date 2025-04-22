## code to prepare `zebrafish_tracking` dataset goes here

usethis::use_data(zebrafish_tracking, overwrite = TRUE)

library(usethis)
library(dplyr)

# Suppose Etovision etc. are already loaded into the environment

usethis::use_data(Ethovision, compress = "bzip2", overwrite = TRUE)

usethis::use_data(ToxTrac, compress = "bzip2", overwrite = TRUE)

usethis::use_data(ANY_maze, compress = "bzip2", overwrite = TRUE)

usethis::use_data(TrackR, compress = "bzip2", overwrite = TRUE)
