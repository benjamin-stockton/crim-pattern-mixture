library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
library(mice, warn.conflicts = FALSE, quietly = TRUE)
library(brms)

source("modify_race_imps.R")
source("sample_rf_pd.R")

options(future.globals.maxSize = 1000 * 1024^2)

imps <- readRDS("fits/mice-rf-sub-data.rds")

s <- 1

bf1 <- bf(JP_MIN ~ DOSAGE + DOSAGEQ + SEX*OFF_RACE + OGS + OGSQ +
                    OGS * PRVREC + PRVREC + RECMIN + CRIME + TRIAL +
                    (1 || COUNTY),
          hu ~ DOSAGE + DOSAGEQ + SEX * OFF_RACE + RECMIN + OGS + OGSQ + PRVREC + CRIME + TRIAL + (1 || COUNTY))

imp_list <- complete(imps, "all", include = FALSE)
df <- imp_list[[1]]

bprior2 <- get_prior(bf1,
               data = df,
               family = hurdle_lognormal(link = "identity", link_sigma = "log", link_hu = "logit"))
bprior2$class
bprior2 <- prior(normal(0, 100), class = "b") +
    prior(normal(0, 100), class = "b", dpar = "hu") +
    # prior(lkj(1), class = "cor") +
    prior(student_t(3, 0, 2.5), class = "Intercept")
    prior(student_t(3, 0, 2.5), class = "sd", group = "COUNTY", lb = 0) +
    prior(student_t(3, 0, 2.5), class = "sd", group = "YEAR", lb = 0) +
    prior(student_t(3, 0, 2.5), class = "Intercept", dpar = "hu") +
    prior(student_t(3, 0, 2.5), class = "sd", dpar = "hu", lb = 0)

rm(imp_list)
rm(df)

if (s != 1) {
    scale <- c(s, 1, 1, 1)
    imps2 <- modify_race_imps(imps, scale = scale)
} else {
    imps2 <- imps
}

fit_lnh_mi <- brm_multiple(bf1,
           data = imps2,
           family = hurdle_lognormal(link = "identity", link_sigma = "log", link_hu = "logit"),
           prior = bprior2,
           chains = 2, cores = floor(100/2), iter = 2000, refresh = 100,
           init = 0,
           control = list(adapt_delta = 0.82, max_treedepth = 12),
           threads = threading(floor(100/2)),
           # file = "brms_fit/brms-hurlde-si.fit",
           # file_refit = "on_change",
           save_model = paste0("stan/sub-brms-hurdle-lognormal-mi-scale-", s, ".stan"))

saveRDS(fit_lnh_mi, paste0("fits/sub-sensitivity-analysis-fit-scale-", s, ".rds"))

smry_lnh <- summary(fit_lnh_mi)
smry_lnh$fixed$term <- rownames(smry_lnh$fixed)

res <- smry_lnh$fixed |>
    mutate(
        est = Estimate,
        se = Est.Error,
        l95 = `l-95% CI`,
        u95 = `u-95% CI`,
        scale = s
    ) |>
    select(scale, term, est, se, l95, u95)

saveRDS(res, paste0("fits/sub-sensitivity-analysis-results-scale-", s, ".rds"))
