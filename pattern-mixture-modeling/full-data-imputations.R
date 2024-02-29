library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
library(mice, warn.conflicts = FALSE, quietly = TRUE)

df <- readr::read_csv("../Data/PCS-most-serious-sentence-2010-2019-pmm.csv",
                      show_col_types = FALSE)

county_ogs <- df |>
    group_by(COUNTY) |>
    summarize(
        COUNTY_OGS = mean(OGS, na.rm = TRUE),
        NCASES = n()
    )

df <- df |> mutate(
    YEAR = as.factor(YEAR),
    INCAR = case_when(
        JP_MIN == 0 ~ 0,
        JP_MIN > 0 ~ 1,
        TRUE ~ NA
    ),
    SEX = case_when(
        MALE == 1 ~ "Male",
        MALE == 0 ~ "Female",
        TRUE ~ NA
    ),
    OFF_RACE = case_when(
        OFF_RACER == 1 ~ "WHITE",
        OFF_RACER == 2 ~ "BLACK",
        OFF_RACER == 3 ~ "LATINO",
        OFF_RACER == 4 ~ "OTHER",
        TRUE ~ NA
    ),
    PRVREC = case_when(
        PRSR == 0 ~ "0",
        PRSR == 1 ~ "1/2/3",
        PRSR == 2 ~ "4/5",
        PRSR == 3 ~ "REVOC/RFEL",
        TRUE ~ NA
    ),
    CRIME = case_when(
        CRIMETYPE == 1 ~ "Persons",
        CRIMETYPE == 2 ~ "Property",
        CRIMETYPE == 3 ~ "Drug",
        CRIMETYPE == 4 ~ "DUI",
        CRIMETYPE == 5 ~ "Other",
        TRUE ~ NA
    ),
    COUNTYTYPE = case_when(
        COUNTY %in% c("Allegheny", "Philadelphia") ~ "Urban",
        COUNTY %in% c("Chester", "Montgomery", "Berks", "Dauphin", "Bucks", "Lancaster", "York", "Delaware", "Northampton", "Luzerne", "Lackawanna", "Westmoreland", "Lehigh", "Erie") ~ "Medium",
        TRUE ~ "Rural"
    )
) |>
    select(
        -c(OFF_RACER, PRSR, MALE, CRIMETYPE)
    ) |>
    left_join(county_ogs, by = "COUNTY") |>
    group_by(COUNTY) |>
    # slice_sample(n = 10000) |>
    ungroup()

df$OFF_RACE <- factor(df$OFF_RACE, levels = c("WHITE", "BLACK", "LATINO", "OTHER"))
df$PCASES <- df$NCASES / sum(county_ogs$NCASES)

df[,c("OGS", "OGSQ", "DOSAGE", "DOSAGEQ")] <- scale(df[,c("OGS", "OGSQ", "DOSAGE", "DOSAGEQ")], center = TRUE, scale = TRUE)

df[,"DOSAGEQ"] <- df[,"DOSAGE"]^2
df[,"OGSQ"] <- df[,"OGS"]^2

imps0 <- mice(df, m = 1, method = "rf", maxit = 0)

mthd <- imps0$method
mthd["JP_MIN_MON"] <-  "~I(JP_MIN / 30)"
mthd["INCAR"] <- "~I(ifelse(JP_MIN > 0, 1, 0))"
# mthd[c("RECMIN", "INCAR", "OFF_RACE", "PRVREC")] <- "cart"
mthd

pred_mat <- imps0$predictorMatrix
pred_mat[,"JPR_ID"] <- 0
pred_mat

imps <- futuremice(df, n.core = 5, m = 5, method = mthd, predictorMatrix = pred_mat, maxit = 10)

saveRDS(imps, "fits/mice-rf-full-data.rds")

