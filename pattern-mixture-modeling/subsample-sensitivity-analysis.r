library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
library(mice, warn.conflicts = FALSE, quietly = TRUE)
library(GLMMadaptive)

imps <- readRDS("fits/mice-rf-sub-data.rds")

sample_rf_pd <- function(dat, fit, scale = c(1, 1, 1, 1)) {
    pred_rf <- predict(fit, data = dat)
    ppred_rf <- pred_rf$predictions
    
    plab <- unlist(lapply(1:nrow(dat), function(i) {
        p_i <- ppred_rf[i,]
        p_i <- p_i * scale
        p_i <- 1/sum(p_i) * p_i
        sample(colnames(ppred_rf), size = 1, prob = p_i)
        }))
}

modify_race_imps <- function(imps, scale = c(1.5, 1, 1, 1)) {
    
    mis_ind <- which(is.na(df[s,"OFF_RACE"]))
    
    imp_list <- complete(imps, "all", include = FALSE)
    
    imp_list2 <- lapply(1:length(imp_list), function(i) {
        df <- imp_list[[i]]
        
        if (sum(is.na(df)) == 0) {
            fit_imp <- ranger::ranger(OFF_RACE ~ DOSAGE + DOSAGEQ + SEX + TRIAL +
                                          RECMIN + PRVREC + OGS + OGSQ + CRIME +
                                          YEAR + COUNTY + JP_MIN, 
                                 data = df[-mis_ind,],
                                 probability = TRUE)
            new_imp <- sample_rf_pd(df[mis_ind,], fit_imp, scale = scale)
            df[mis_ind, "OFF_RACE"] <- new_imp
        }
        
        df$.imp <- i-1
        df$.id <- 1:nrow(df)
        
        return(df)
    })
    
    imps2 <- imp_list2 |>
        bind_rows() |>
        as.mids()
    return(imps2)
}

sens_scales <- c(0.33, 0.5, 0.75, 0.9, 1, 1.1, 1.25, 1.5, 2)

sens_res <- parallel::mclapply(sens_scales, mc.cores = 5, function(s) {
    
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

saveRDS(sens_res, "fits/sensitivity-analysis-results-subsample-glmmadapt.rds")
