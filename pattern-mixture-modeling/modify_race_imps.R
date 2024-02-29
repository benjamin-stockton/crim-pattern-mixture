modify_race_imps <- function(imps, scale = c(1.5, 1, 1, 1)) {
    
    mis_ind <- which(imps$where[, "OFF_RACE"])
    
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



