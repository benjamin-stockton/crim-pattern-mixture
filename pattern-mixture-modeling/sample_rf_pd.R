

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