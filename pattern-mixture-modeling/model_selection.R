library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
library(ggplot2, warn.conflicts = FALSE, quietly = TRUE)
library(mice, warn.conflicts = FALSE, quietly = TRUE)
library(GLMMadaptive)
library(broom.mixed)
library(brms)
library(VIM)
library(bayesplot)
library(loo)
setwd("pattern-mixture-modeling")

theme_set(theme_bw())
color_scheme_set("brightblue")

df <- readr::read_csv("../Data/PCS-most-serious-sentence-2010-2019-pmm.csv",
                      show_col_types = FALSE)

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
    )
) |>
    select(
        -c(OFF_RACER, PRSR, MALE, CRIMETYPE)
    )

df$OFF_RACE <- factor(df$OFF_RACE, levels = c("WHITE", "BLACK", "LATINO", "OTHER"))

df |> 
    select(
        JP_MIN, OFF_RACE, DOSAGE, RECMIN, PRVREC, CRIME, TRIAL, SEX
    ) |> 
    aggr(sortby = "JP_MIN", plot = FALSE) |>
    plot(numbers = TRUE, prop = FALSE)

df[,c("OGS", "OGSQ", "DOSAGE", "DOSAGEQ")] <- scale(df[,c("OGS", "OGSQ", "DOSAGE", "DOSAGEQ")], center = TRUE, scale = TRUE)

df[,"DOSAGEQ"] <- df[,"DOSAGE"]^2
df[,"OGSQ"] <- df[,"OGS"]^2

s <- sample(1:nrow(df), size = 10000, replace = FALSE)

df[s,] |> 
    mutate(
        jp_min_l1p = log1p(JP_MIN),
        OGSCRIMEDUI = case_when(CRIME == "DUI" ~ OGS,
                                TRUE ~ 0),
        OGSCRIMEOther = case_when(CRIME == "Other" ~ OGS,
                                  TRUE ~ 0),
        OGSCRIMEPersons = case_when(CRIME == "Persons" ~ OGS,
                                    TRUE ~ 0),
        OGSCRIMEProperty = case_when(CRIME == "Property" ~ OGS,
                                     TRUE ~ 0),
        OGSPREV123 = case_when(PRVREC == "1/2/3" ~ OGS,
                                  TRUE ~ 0),
        OGSPREV45 = case_when(PRVREC == "4/5" ~ OGS,
                                    TRUE ~ 0),
        OGSPREVRFEL = case_when(PRVREC == "REVOC/RFEL" ~ OGS,
                                     TRUE ~ 0),
        
    ) |>
    select(starts_with("OGS"), jp_min_l1p) |>
    GGally::ggpairs()

df |>
    group_by(CRIME, PRVREC) |>
    summarize(
        mean_sen_len = mean(JP_MIN, na.rm = TRUE),
        sd_sen_len = sd(JP_MIN, na.rm = TRUE),
        p_incar = mean(JP_MIN == 0, na.rm = TRUE)
    ) |>
    print(n = 21)

imps0 <- mice(df[s,], m = 1, method = "rf", maxit = 0)

mthd <- imps0$method
mthd["JP_MIN_MON"] <-  "~I(JP_MIN / 30)"
mthd["INCAR"] <- "~I(ifelse(JP_MIN > 0, 1, 0))"
# mthd[c("RECMIN", "INCAR", "OFF_RACE", "PRVREC")] <- "cart"
mthd

pred_mat <- imps0$predictorMatrix
pred_mat[,"JPR_ID"] <- 0
pred_mat

# imps <- mice(df[s,], m = 5, method = mthd, predictorMatrix = pred_mat, maxit = 10)
# 
# plot(imps)
# 
# saveRDS(imps, "fits/mice-rf-subsample.rds")

imps <- readRDS("fits/mice-rf-subsample.rds")
imp_list <- complete(imps, "all", include = FALSE)

bf_me_c <- bf(JP_MIN ~ DOSAGE + SEX + OFF_RACE + OGS + PRVREC + RECMIN + CRIME 
              + TRIAL + (1 |COUNTY),
          hu ~ DOSAGE + SEX + OFF_RACE + RECMIN + OGS + PRVREC + CRIME + TRIAL 
          + (1 | COUNTY))
bf_me_yc <- bf(JP_MIN ~ DOSAGE + SEX + OFF_RACE + OGS + PRVREC + RECMIN + CRIME 
               + TRIAL + (1 | YEAR) + (1 |COUNTY),
                 hu ~ DOSAGE + SEX + OFF_RACE + RECMIN + OGS + PRVREC + CRIME 
               + TRIAL + (1 | COUNTY))
bf_sq_yc <- bf(JP_MIN ~ DOSAGE + DOSAGEQ + SEX + OFF_RACE + OGS + OGSQ + PRVREC 
               + RECMIN + CRIME + TRIAL + (1 | YEAR) + (1 |COUNTY),
                 hu ~ DOSAGE + DOSAGEQ + SEX + OFF_RACE + RECMIN + OGS + OGSQ 
               + PRVREC + CRIME + TRIAL + (1 | COUNTY))
bf_int_yc <- bf(JP_MIN ~ DOSAGE + DOSAGEQ + SEX * OFF_RACE + OGS * PRVREC + OGSQ +  
               + RECMIN + PRVREC * CRIME + TRIAL + (1 | YEAR) + (1 |COUNTY),
               hu ~ DOSAGE + DOSAGEQ + SEX * OFF_RACE + RECMIN + OGS * PRVREC + OGSQ + PRVREC * CRIME 
               + TRIAL + (1 | COUNTY))

bf_int_yc2 <- bf(JP_MIN ~ DOSAGE + DOSAGEQ + SEX * OFF_RACE + OGS + OGSQ +  
                     + RECMIN + PRVREC * CRIME + TRIAL + (1 | YEAR) + (1 |COUNTY),
                 hu ~ DOSAGE + DOSAGEQ + SEX * OFF_RACE + RECMIN + OGS + OGSQ + PRVREC * CRIME 
                 + TRIAL + (1 | COUNTY))

bf_int_yc3 <- bf(JP_MIN ~ DOSAGE + DOSAGEQ + SEX * OFF_RACE + OGS * PRVREC + OGSQ +  
                    + RECMIN + CRIME + TRIAL + (1 | YEAR) + (1 |COUNTY),
                hu ~ DOSAGE + DOSAGEQ + SEX * OFF_RACE + RECMIN + OGS * PRVREC + OGSQ + CRIME 
                + TRIAL + (1 | COUNTY))

bf_int_yc4 <- bf(JP_MIN ~ DOSAGE + DOSAGEQ + SEX * OFF_RACE + OGS + OGSQ +  
                     + RECMIN + PRVREC * CRIME + TRIAL + (1 | YEAR) + (1 |COUNTY),
                 hu ~ DOSAGE + DOSAGEQ + SEX * OFF_RACE + RECMIN + OGS + OGSQ + PRVREC * CRIME 
                 + TRIAL + (1 | COUNTY))
bf_int_yc5 <- bf(JP_MIN ~ DOSAGE + DOSAGEQ + SEX + OFF_RACE + OGS + OGSQ +  
                     + RECMIN + PRVREC * CRIME + TRIAL + (1 | YEAR) + (1 |COUNTY),
                 hu ~ DOSAGE + DOSAGEQ + SEX + OFF_RACE + RECMIN + OGS + OGSQ + PRVREC * CRIME 
                 + TRIAL + (1 | COUNTY))
bf_int_ycs <- bf(JP_MIN ~ s(DOSAGE) + SEX + OFF_RACE + OGS + OGSQ +  
                     + RECMIN + PRVREC * CRIME + TRIAL + (1 | YEAR) + (1 |COUNTY),
                 hu ~ s(DOSAGE) + SEX + OFF_RACE + RECMIN + OGS + OGSQ + PRVREC * CRIME 
                 + TRIAL + (1 | COUNTY))
bf_int_ycs2 <- bf(JP_MIN ~ DOSAGE + DOSAGEQ + SEX + OFF_RACE + OGS + OGSQ +  
                     + RECMIN + PRVREC * CRIME + TRIAL + (1 | YEAR) + (1 |COUNTY),
                 hu ~ DOSAGEQ + SEX + OFF_RACE + RECMIN + OGS + OGSQ + PRVREC * CRIME 
                 + TRIAL + (1 | YEAR) + (1 | COUNTY))

bprior2 <- get_prior(bf_sq_yc,
                     data = df[s,],
                     family = hurdle_lognormal(link = "identity", link_sigma = "log", link_hu = "logit"))
bprior2$class
bprior2 <- prior(normal(0, 100), class = "b") +
    prior(normal(0, 100), class = "b", dpar = "hu") +
    # prior(lkj(1), class = "cor") +
    prior(student_t(3, 0, 2.5), class = "Intercept")
prior(inv_gamma(2.001, 1.001), class = "sd", group = "COUNTY", lb = 0) +
    prior(inv_gamma(2.001, 1.001), class = "sd", group = "YEAR", lb = 0) +
    prior(logistic(0, 1), class = "Intercept", dpar = "hu") +
    prior(inv_gamma(2.001, 1.001), class = "sd", dpar = "hu", lb = 0)

# bprior2$prior[1] <- "normal(0, 25)"
bprior2

fit_brm1 <- brm(bf_me_c,
             data = imp_list[[1]],
             family = hurdle_lognormal(link = "identity", link_sigma = "log", link_hu = "logit"),
             prior = bprior2,
             chains = 2, cores = 15, iter = 2000, refresh = 200,
             init = 0,
             control = list(adapt_delta = 0.82, max_treedepth = 10))

summary(fit_brm1)
plot(fit_brm1, ask = FALSE, N = 3)
plot(conditional_effects(fit_brm1), ask = FALSE)

loo1 <- loo(fit_brm1)

fit_brm2 <- brm(bf_me_yc,
                data = imp_list[[1]],
                family = hurdle_lognormal(link = "identity", link_sigma = "log", link_hu = "logit"),
                prior = bprior2,
                chains = 2, cores = 15, iter = 2000, refresh = 200,
                init = 0,
                control = list(adapt_delta = 0.82, max_treedepth = 10))

summary(fit_brm2)
plot(fit_brm2, ask = FALSE, N = 3)
plot(conditional_effects(fit_brm2), ask = FALSE)

loo2 <- loo(fit_brm2)

fit_brm3 <- brm(bf_sq_yc,
                data = imp_list[[1]],
                family = hurdle_lognormal(link = "identity", link_sigma = "log", link_hu = "logit"),
                prior = bprior2,
                chains = 2, cores = 15, iter = 2000, refresh = 200,
                init = 0,
                control = list(adapt_delta = 0.82, max_treedepth = 10))

summary(fit_brm3)
plot(fit_brm3, ask = FALSE, N = 3)
plot(conditional_effects(fit_brm3), ask = FALSE)

loo3 <- loo(fit_brm3)

fit_brm4 <- brm(bf_int_yc,
                data = imp_list[[1]],
                family = hurdle_lognormal(link = "identity", link_sigma = "log", link_hu = "logit"),
                prior = bprior2,
                chains = 2, cores = 15, iter = 3000, refresh = 200,
                init = 0,
                control = list(adapt_delta = 0.85, max_treedepth = 10))

# summary(fit_brm4)
# plot(fit_brm4, ask = FALSE, N = 3)
# plot(conditional_effects(fit_brm4), ask = FALSE)

loo4 <- loo(fit_brm4)

fit_brm5 <- brm(bf_int_yc2,
                data = imp_list[[1]],
                family = hurdle_lognormal(link = "identity", link_sigma = "log", link_hu = "logit"),
                prior = bprior2,
                chains = 2, cores = 15, iter = 3000, refresh = 200,
                init = 0,
                control = list(adapt_delta = 0.85, max_treedepth = 10))
loo5 <- loo(fit_brm5)
fit_brm6 <- brm(bf_int_yc3,
                data = imp_list[[1]],
                family = hurdle_lognormal(link = "identity", link_sigma = "log", link_hu = "logit"),
                prior = bprior2,
                chains = 2, cores = 15, iter = 3000, refresh = 200,
                init = 0,
                control = list(adapt_delta = 0.85, max_treedepth = 10))
loo6 <- loo(fit_brm6)
fit_brm7 <- brm(bf_int_yc4,
                data = imp_list[[1]],
                family = hurdle_lognormal(link = "identity", link_sigma = "log", link_hu = "logit"),
                prior = bprior2,
                chains = 2, cores = 15, iter = 3000, refresh = 200,
                init = 0,
                control = list(adapt_delta = 0.85, max_treedepth = 10))
loo7 <- loo(fit_brm7)
fit_brm8 <- brm(bf_int_yc5,
                data = imp_list[[1]],
                family = hurdle_lognormal(link = "identity", link_sigma = "log", link_hu = "logit"),
                prior = bprior2,
                chains = 2, cores = 15, iter = 3000, refresh = 200,
                init = 0,
                control = list(adapt_delta = 0.85, max_treedepth = 10))
loo8 <- loo(fit_brm8)
fit_brm9 <- brm(bf_int_ycs,
                data = imp_list[[1]],
                family = hurdle_lognormal(link = "identity", link_sigma = "log", link_hu = "logit"),
                prior = bprior2,
                chains = 2, cores = 15, iter = 3000, refresh = 200,
                init = 0,
                control = list(adapt_delta = 0.85, max_treedepth = 10))
loo9 <- loo(fit_brm9)
fit_brm10 <- brm(bf_int_ycs2,
                data = imp_list[[1]],
                family = hurdle_lognormal(link = "identity", link_sigma = "log", link_hu = "logit"),
                prior = bprior2,
                chains = 2, cores = 15, iter = 3000, refresh = 200,
                init = 0,
                control = list(adapt_delta = 0.85, max_treedepth = 10))
loo10 <- loo(fit_brm10)
loo_compare(loo1, loo2, loo3, loo4, loo5, loo6, loo7, loo8, loo9, loo10)

w4 <- waic(fit_brm4)
w7 <- waic(fit_brm7)
w8 <- waic(fit_brm8)

loo_compare(w4, w7, w8)


start <- Sys.time()
fit_lnh <- mixed_model(JP_MIN ~ DOSAGE + DOSAGEQ + SEX * OFF_RACE + OGS + OGSQ + PRVREC * CRIME + RECMIN + TRIAL,
                       random = ~ 1 | YEAR,
                       zi_fixed = ~ DOSAGE + DOSAGEQ + SEX * OFF_RACE + RECMIN + OGS + OGSQ + PRVREC * CRIME + TRIAL,
                       # zi_random = ~ 1 | YEAR,
                       data = df,
                       family = hurdle.lognormal(),
                       penalized = TRUE,
                       iter_EM = 0)
print(Sys.time() - start)

saveRDS(fit_lnh, "fits/fulldata-glmmadapt-model.rds")

summary(fit_lnh)
marginal_coefs(fit_lnh)
confint(fit_lnh, "fixed-eff")
confint(fit_lnh, "zero_part")

par(mar = c(2.5, 2.5, 0, 0), mgp = c(1.1, 0.5, 0), cex.axis = 0.7, cex.lab = 0.8)
y <- df$JP_MIN
y <- y[which(!is.na(y))]
y[y > 0] <- log(y[y > 0])
x_vals <- seq(min(y)-1, max(y), length.out = 500)
out <- simulate(fit_lnh, nsim = 30, acount_MLEs_var = FALSE)
ind <- out > sqrt(.Machine$double.eps)
out[ind] <- log(out[ind])
rep_y <- apply(out, 2, function (x, x_vals) ecdf(x)(x_vals), x_vals = x_vals)
matplot(x_vals, rep_y, type = "l", lty = 1, col = "lightgrey", 
        xlab = "Response Variable", ylab = "Empirical CDF")
lines(x_vals, ecdf(y)(x_vals))
legend("bottomright", c("log replicated data", "log observed data"), lty = 1, 
       col = c("lightgrey", "black"), bty = "n", cex = 0.8)


start <- Sys.time()
fit_ph <- mixed_model(JP_MIN ~ DOSAGE + DOSAGEQ + SEX * OFF_RACE + OGS + OGSQ + PRVREC * CRIME + RECMIN + TRIAL,
                       random = ~ 1 | YEAR,
                       zi_fixed = ~ DOSAGE + DOSAGEQ + SEX * OFF_RACE + RECMIN + OGS + OGSQ + PRVREC * CRIME + TRIAL,
                       # zi_random = ~ 1 | YEAR,
                       data = df,
                       family = hurdle.poisson(),
                       penalized = TRUE,
                       iter_EM = 0)
print(Sys.time() - start)

saveRDS(fit_ph, "fits/fulldata-glmmadapt-pois-model.rds")

summary(fit_ph)
marginal_coefs(fit_ph)
confint(fit_ph, "fixed-eff")
confint(fit_ph, "zero_part")

par(mar = c(2.5, 2.5, 0, 0), mgp = c(1.1, 0.5, 0), cex.axis = 0.7, cex.lab = 0.8)
y <- df$JP_MIN
y <- y[which(!is.na(y))]
y[y > 0] <- log(y[y > 0])
x_vals <- seq(min(y)-1, max(y), length.out = 500)
out <- simulate(fit_ph, nsim = 30, acount_MLEs_var = FALSE)
ind <- out > sqrt(.Machine$double.eps)
out[ind] <- log(out[ind])
rep_y <- apply(out, 2, function (x, x_vals) ecdf(x)(x_vals), x_vals = x_vals)
matplot(x_vals, rep_y, type = "l", lty = 1, col = "lightgrey", 
        xlab = "Response Variable", ylab = "Empirical CDF")
lines(x_vals, ecdf(y)(x_vals))
legend("bottomright", c("log replicated data", "log observed data"), lty = 1, 
       col = c("lightgrey", "black"), bty = "n", cex = 0.8)