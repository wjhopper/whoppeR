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

test_that("WISE error calculation is accurate", {

  ColorShape_test <- WISEsummary(ColorShapes, DV = "time", withinvars = c("shape","color"),
                       idvar = "subject")
  mapply(function(x,y) expect_equal(round(x, 10), round(x, 10)),
         ColorShape_test[, c("time", "normed_time", "sd", "n", "sem", "CI_lower","CI_upper")],
         ColorShape_summary[, c("time", "normed_time", "sd", "n", "sem", "CI_lower","CI_upper")])

  MemoryDrug_test <- WISEsummary(MemoryDrugs, DV = "Recall", idvar = "Subject",
                                 betweenvars = c("Gender","Dosage"),
                                 withinvars = c("Task", "Valence"))
  mapply(expect_equal,
         MemoryDrug_test[, c("Recall", "normed_Recall", "sd", "n", "sem")],
         MemoryDrug_summary[, c("Recall", "Recall_norm", "sd", "N", "se")])
  expect_equal((MemoryDrug_test$CI_upper - MemoryDrug_test$Recall),
               MemoryDrug_summary$ci)

})
