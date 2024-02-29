library(haven)
library(dplyr)

sentencing <- read_dta("Data/PCS Most Serious Sentence in JP, 2010 - 2019.dta")

sen_reduced <- sentencing |>
    select(JPR_ID, JP_MIN, JP_MIN_MON,
           DOSAGE, DOSAGEQ, MALE, OFF_RACER,
           OGS, OGSQ, PRSR, RECMIN, CRIMETYPE, TRIAL, 
           YEAR, COUNTY,
           JP_CC_BUG, DUP) |>
    filter(
        JP_CC_BUG != 1,
        DUP != 1
    ) |>
    select(-c(JP_CC_BUG, DUP))

readr::write_csv(sen_reduced, "Data/PCS-most-serious-sentence-2010-2019-pmm.csv")
