library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
library(mice, warn.conflicts = FALSE, quietly = TRUE)
library(GLMMadaptive)

source("modify_race_imps.R")
source("sample_rf_pd.R")
imps <- readRDS("fits/mice-rf-full-data.rds")

sens_scales <- c(0.33, 0.5, 0.75, 0.9, 1, 1.1, 1.25, 1.5, 2)

sens_res <- parallel::mclapply(sens_scales, mc.cores = length(sens_scales), function(s) {
    
    if (s != 1) {
        scale <- c(s, 1, 1, 1)
        imps2 <- modify_race_imps(imps, scale = scale)
    }
    else {
        imps2 <- imps
    }
    
    imp_list <- complete(imps2, "all", include = FALSE)
    
    fit_lnh_mi <- lapply(imp_list, function(imp) {
        fit_lnh <- mixed_model(JP_MIN ~ DOSAGE + DOSAGEQ + SEX + OFF_RACE + OGS + OGSQ +
                        OGS * PRVREC + RECMIN + CRIME + TRIAL,
                        data = imp,
                        family = hurdle.lognormal(),
                        random = ~ 1 | COUNTY,
                        zi_fixed = ~ DOSAGE + DOSAGEQ + SEX + OFF_RACE + OGSQ + OGS * PRVREC + RECMIN + CRIME + TRIAL,
                        zi_random = ~ 1 | COUNTY
                        )
        return(fit_lnh)
    }) 
    
    est_lnh_mi <- lapply(fit_lnh_mi, function(fit) {fit$coefficients}) |> 
        bind_rows() |>
        apply(2, mean)
    
    hu_lnh_mi <- lapply(fit_lnh_mi, function(fit) {fit$gammas}) |> 
        bind_rows() |> 
        apply(2, mean)
    
    res <- bind_rows(est_lnh_mi, hu_lnh_mi)
    res$mod <- c("lognormal", "logistic")
    res$scale <- s
    return(res)
})

# str(sens_res)

saveRDS(sens_res, "fits/sensitivity-analysis-results.rds")
