library(tidyr)
ColorShapes <- data.frame(subject = 1:12,
    Round_Mono = c(41L, 57L, 52L, 49L, 47L, 37L, 47L, 41L, 48L, 37L, 32L, 47L),
    Square_Mono = c(40L, 56L, 53L, 47L, 48L, 34L, 50L, 40L, 47L, 35L, 31L, 42L),
    Round_Color = c(41L, 56L, 53L, 47L, 48L, 35L, 47L, 38L, 49L, 36L, 31L, 42L),
    Square_Color = c(37L, 53L, 50L, 47L, 47L, 36L, 46L, 40L, 45L, 35L, 33L, 42L)) %>%
  gather(condition, time, Round_Mono:Square_Color) %>%
  separate(condition, c("Shape","Color"), sep = "_") %>%
  setNames(tolower(names(.)))

devtools::use_data(ColorShapes)


