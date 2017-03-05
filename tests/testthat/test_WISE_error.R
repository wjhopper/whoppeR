context("Testing WISE summary function for computing within-subject error variance")

ColorShape_summary <- structure(list(shape = c("Round", "Round", "Square", "Square"),
  color = c("Color", "Mono", "Color", "Mono"),
  time = c(43.5833333333333, 44.5833333333333, 42.5833333333333, 43.5833333333333),
  normed_time = c(43.5833333333333,44.5833333333333, 42.5833333333333, 43.5833333333333),
  sd = c(1.21231059126652, 1.33143804689789, 1.46163047189214, 1.26131244777378),
  n = c(12L, 12L, 12L, 12L), sem = c(0.349963923104579, 0.384353057392904, 0.421936373201343, 0.36410954062721),
  CI_lower = c(42.8130679320127, 43.7373779577757, 41.6546576374143, 42.7819336377651),
  CI_upper = c(44.353598734654, 45.429288708891, 43.5120090292524, 44.3847330289016)),
  class = c("tbl_df","tbl", "data.frame"), row.names = c(NA, -4L),
  .Names = c("shape","color", "time", "normed_time", "sd", "n", "sem", "CI_lower","CI_upper"))

load("../MemoryDrug_summary.rda")

test_that("WISE error calculation is accurate with 1 DV", {

  ColorShape_test <- WISEsummary(ColorShapes,
                                 dependentvars = "time",
                                 withinvars = c("shape", "color"),
                                 idvar = "subject")

  mapply(expect_equal,
         ColorShape_test[c("time_mean", "time_recentered_mean", "time_sem", "time_CI_lower","time_CI_upper")],
         ColorShape_summary[c("time", "normed_time", "sem", "CI_lower","CI_upper")])
  expect_equal(ColorShape_test$time_sem * sqrt(ColorShape_test$time_n),
               ColorShape_summary$sd)


  MemoryDrug_test <- WISEsummary(MemoryDrugs,
                                 dependentvars = "Recall",
                                 idvar = "Subject",
                                 betweenvars = c("Gender","Dosage"),
                                 withinvars = c("Task", "Valence"))
  mapply(expect_equal,
         MemoryDrug_test[c("Recall_mean", "Recall_recentered_mean", "Recall_n", "Recall_sem")],
         MemoryDrug_summary[c("Recall", "Recall_norm", "N", "se")])
  expect_equal((MemoryDrug_test$Recall_CI_upper - MemoryDrug_test$Recall_mean),
               MemoryDrug_summary$ci)

})

test_that("WISE error calculation is accurate with 2 DVs", {

  set.seed(11)
  ColorShapes$acc <- rbeta(nrow(ColorShapes), 2, 2)
  ColorShape_test <- WISEsummary(ColorShapes,
                                 dependentvars = c("time","acc"),
                                 withinvars = c("shape","color"),
                                 idvar = "subject")

  mapply(expect_equal,
         ColorShape_test[paste0("time", c("_mean", "_recentered_mean", "_sem", "_CI_lower","_CI_upper"))],
         ColorShape_summary[c("time", "normed_time", "sem", "CI_lower","CI_upper")])

  MemoryDrugs$RT <- rexp(nrow(MemoryDrugs),1)
  MemoryDrug_test <- WISEsummary(MemoryDrugs,
                                 dependentvars = c("Recall", "RT"),
                                 idvar = "Subject",
                                 betweenvars = c("Gender","Dosage"),
                                 withinvars = c("Task", "Valence"))
  mapply(expect_equal,
         MemoryDrug_test[c("Recall_mean", "Recall_recentered_mean", "Recall_n", "Recall_sem")],
         MemoryDrug_summary[c("Recall", "Recall_norm", "N", "se")])
  expect_equal((MemoryDrug_test$Recall_CI_upper - MemoryDrug_test$Recall_mean),
               MemoryDrug_summary$ci)

})
