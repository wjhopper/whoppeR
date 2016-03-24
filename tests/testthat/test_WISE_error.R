context("Testing WISE summary function for computing within-subject error variance")

library(tidyr)

data <- data.frame(subject = 1:12,
                   Round_Mono = c(41L, 57L, 52L, 49L, 47L, 37L, 47L, 41L, 48L, 37L, 32L, 47L),
                   Square_Mono = c(40L, 56L, 53L, 47L, 48L, 34L, 50L, 40L, 47L, 35L, 31L, 42L),
                   Round_Color = c(41L, 56L, 53L, 47L, 48L, 35L, 47L, 38L, 49L, 36L, 31L, 42L),
                   Square_Color = c(37L, 53L, 50L, 47L, 47L, 36L, 46L, 40L, 45L, 35L, 33L, 42L)) %>%
  gather(condition, time, Round_Mono:Square_Color) %>%
  separate(condition, c("Shape","Color"), sep = "_") %>%
  setNames(tolower(names(.)))

answer <- structure(list(shape = c("Round", "Round", "Square", "Square"),
  color = c("Color", "Mono", "Color", "Mono"),
  time = c(43.5833333333333, 44.5833333333333, 42.5833333333333, 43.5833333333333),
  normed_time = c(43.5833333333333,44.5833333333333, 42.5833333333333, 43.5833333333333),
  sd = c(1.21231059126652, 1.33143804689789, 1.46163047189214, 1.26131244777378),
  n = c(12L, 12L, 12L, 12L), sem = c(0.349963923104579, 0.384353057392904, 0.421936373201343, 0.36410954062721),
  CI_lower = c(42.8130679320127, 43.7373779577757, 41.6546576374143, 42.7819336377651),
  CI_upper = c(44.353598734654, 45.429288708891, 43.5120090292524, 44.3847330289016)),
  class = c("tbl_df","tbl", "data.frame"), row.names = c(NA, -4L),
  .Names = c("shape","color", "time", "normed_time", "sd", "n", "sem", "CI_lower","CI_upper"))

test_that("WISE error calculation is accurate", {
  data_summary <- WISEsummary(data, DV = "time", withinvars = c("shape","color"),
                       idvar = "subject")
  mapply(function(x,y) expect_equal(round(x, 10), round(x, 10)),
         data_summary[, c("time", "normed_time", "sd", "n", "sem", "CI_lower","CI_upper")],
         answer[, c("time", "normed_time", "sd", "n", "sem", "CI_lower","CI_upper")])
})
