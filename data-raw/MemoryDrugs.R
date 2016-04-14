MemoryDrugs <- read.csv("data-raw/MemoryDrugs.csv", strip.white = TRUE)
AOV <- aov(Recall~(Task*Valence*Gender*Dosage)+Error(Subject/(Task*Valence)),
           data = MemoryDrugs)
cell_means <- model.tables(AOV,"means")[[1]]$`Task:Valence:Gender:Dosage`
devtools::use_data(MemoryDrugs)
