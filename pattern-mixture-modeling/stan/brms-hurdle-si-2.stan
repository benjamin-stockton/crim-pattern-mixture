// generated with brms 2.20.4
functions {
 /* compute correlated group-level effects
  * Args:
  *   z: matrix of unscaled group-level effects
  *   SD: vector of standard deviation parameters
  *   L: cholesky factor correlation matrix
  * Returns:
  *   matrix of scaled group-level effects
  */
  matrix scale_r_cor(matrix z, vector SD, matrix L) {
    // r is stored in another dimension order than z
    return transpose(diag_pre_multiply(SD, L) * z);
  }
  /* hurdle poisson log-PDF of a single response
   * Args:
   *   y: the response value
   *   lambda: mean parameter of the poisson distribution
   *   hu: hurdle probability
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real hurdle_poisson_lpmf(int y, real lambda, real hu) {
    if (y == 0) {
      return bernoulli_lpmf(1 | hu);
    } else {
      return bernoulli_lpmf(0 | hu) +
             poisson_lpmf(y | lambda) -
             log1m_exp(-lambda);
    }
  }
  /* hurdle poisson log-PDF of a single response
   * logit parameterization of the hurdle part
   * Args:
   *   y: the response value
   *   lambda: mean parameter of the poisson distribution
   *   hu: linear predictor for hurdle part
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real hurdle_poisson_logit_lpmf(int y, real lambda, real hu) {
    if (y == 0) {
      return bernoulli_logit_lpmf(1 | hu);
    } else {
      return bernoulli_logit_lpmf(0 | hu) +
             poisson_lpmf(y | lambda) -
             log1m_exp(-lambda);
    }
  }
  /* hurdle poisson log-PDF of a single response
   * log parameterization for the poisson part
   * Args:
   *   y: the response value
   *   eta: linear predictor for poisson part
   *   hu: hurdle probability
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real hurdle_poisson_log_lpmf(int y, real eta, real hu) {
    if (y == 0) {
      return bernoulli_lpmf(1 | hu);
    } else {
      return bernoulli_lpmf(0 | hu) +
             poisson_log_lpmf(y | eta) -
             log1m_exp(-exp(eta));
    }
  }
  /* hurdle poisson log-PDF of a single response
   * log parameterization for the poisson part
   * logit parameterization of the hurdle part
   * Args:
   *   y: the response value
   *   eta: linear predictor for poisson part
   *   hu: linear predictor for hurdle part
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real hurdle_poisson_log_logit_lpmf(int y, real eta, real hu) {
    if (y == 0) {
      return bernoulli_logit_lpmf(1 | hu);
    } else {
      return bernoulli_logit_lpmf(0 | hu) +
             poisson_log_lpmf(y | eta) -
             log1m_exp(-exp(eta));
    }
  }
  // hurdle poisson log-CCDF and log-CDF functions
  real hurdle_poisson_lccdf(int y, real lambda, real hu) {
    return bernoulli_lpmf(0 | hu) + poisson_lccdf(y | lambda) -
           log1m_exp(-lambda);
  }
  real hurdle_poisson_lcdf(int y, real lambda, real hu) {
    return log1m_exp(hurdle_poisson_lccdf(y | lambda, hu));
  }
}
data {
  int<lower=1> N;  // total number of observations
  array[N] int Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int<lower=1> Kc;  // number of population-level effects after centering
  int<lower=1> K_hu;  // number of population-level effects
  matrix[N, K_hu] X_hu;  // population-level design matrix
  int<lower=1> Kc_hu;  // number of population-level effects after centering
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  array[N] int<lower=1> J_1;  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_1;
  vector[N] Z_1_2;
  vector[N] Z_1_3;
  vector[N] Z_1_4;
  vector[N] Z_1_5;
  vector[N] Z_1_6;
  vector[N] Z_1_7;
  int<lower=1> NC_1;  // number of group-level correlations
  // data for group-level effects of ID 2
  int<lower=1> N_2;  // number of grouping levels
  int<lower=1> M_2;  // number of coefficients per level
  array[N] int<lower=1> J_2;  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_2_1;
  // data for group-level effects of ID 3
  int<lower=1> N_3;  // number of grouping levels
  int<lower=1> M_3;  // number of coefficients per level
  array[N] int<lower=1> J_3;  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_3_hu_1;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  matrix[N, Kc_hu] Xc_hu;  // centered version of X_hu without an intercept
  vector[Kc_hu] means_X_hu;  // column means of X_hu before centering
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
  for (i in 2:K_hu) {
    means_X_hu[i - 1] = mean(X_hu[, i]);
    Xc_hu[, i - 1] = X_hu[, i] - means_X_hu[i - 1];
  }
}
parameters {
  vector[Kc] b;  // regression coefficients
  real Intercept;  // temporary intercept for centered predictors
  vector[Kc_hu] b_hu;  // regression coefficients
  real Intercept_hu;  // temporary intercept for centered predictors
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  matrix[M_1, N_1] z_1;  // standardized group-level effects
  cholesky_factor_corr[M_1] L_1;  // cholesky factor of correlation matrix
  vector<lower=0>[M_2] sd_2;  // group-level standard deviations
  array[M_2] vector[N_2] z_2;  // standardized group-level effects
  vector<lower=0>[M_3] sd_3;  // group-level standard deviations
  array[M_3] vector[N_3] z_3;  // standardized group-level effects
}
transformed parameters {
  matrix[N_1, M_1] r_1;  // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_1] r_1_1;
  vector[N_1] r_1_2;
  vector[N_1] r_1_3;
  vector[N_1] r_1_4;
  vector[N_1] r_1_5;
  vector[N_1] r_1_6;
  vector[N_1] r_1_7;
  vector[N_2] r_2_1;  // actual group-level effects
  vector[N_3] r_3_hu_1;  // actual group-level effects
  real lprior = 0;  // prior contributions to the log posterior
  // compute actual group-level effects
  r_1 = scale_r_cor(z_1, sd_1, L_1);
  r_1_1 = r_1[, 1];
  r_1_2 = r_1[, 2];
  r_1_3 = r_1[, 3];
  r_1_4 = r_1[, 4];
  r_1_5 = r_1[, 5];
  r_1_6 = r_1[, 6];
  r_1_7 = r_1[, 7];
  r_2_1 = (sd_2[1] * (z_2[1]));
  r_3_hu_1 = (sd_3[1] * (z_3[1]));
  lprior += normal_lpdf(b | 0, 25);
  lprior += student_t_lpdf(Intercept | 3, -2.3, 2.5);
  lprior += normal_lpdf(b_hu | 0, 25);
  lprior += logistic_lpdf(Intercept_hu | 0, 1);
  lprior += student_t_lpdf(sd_1 | 3, 0, 2.5)
    - 7 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += lkj_corr_cholesky_lpdf(L_1 | 1);
  lprior += student_t_lpdf(sd_2 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_3 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] hu = rep_vector(0.0, N);
    mu += Intercept + Xc * b;
    hu += Intercept_hu + Xc_hu * b_hu;
    for (n in 1:N) {
      // add more terms to the linear predictor
      mu[n] += r_1_1[J_1[n]] * Z_1_1[n] + r_1_2[J_1[n]] * Z_1_2[n] + r_1_3[J_1[n]] * Z_1_3[n] + r_1_4[J_1[n]] * Z_1_4[n] + r_1_5[J_1[n]] * Z_1_5[n] + r_1_6[J_1[n]] * Z_1_6[n] + r_1_7[J_1[n]] * Z_1_7[n] + r_2_1[J_2[n]] * Z_2_1[n];
    }
    for (n in 1:N) {
      // add more terms to the linear predictor
      hu[n] += r_3_hu_1[J_3[n]] * Z_3_hu_1[n];
    }
    for (n in 1:N) {
      target += hurdle_poisson_log_logit_lpmf(Y[n] | mu[n], hu[n]);
    }
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(to_vector(z_1));
  target += std_normal_lpdf(z_2[1]);
  target += std_normal_lpdf(z_3[1]);
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
  // actual population-level intercept
  real b_hu_Intercept = Intercept_hu - dot_product(means_X_hu, b_hu);
  // compute group-level correlations
  corr_matrix[M_1] Cor_1 = multiply_lower_tri_self_transpose(L_1);
  vector<lower=-1,upper=1>[NC_1] cor_1;
  // extract upper diagonal of correlation matrix
  for (k in 1:M_1) {
    for (j in 1:(k - 1)) {
      cor_1[choose(k - 1, 2) + j] = Cor_1[j, k];
    }
  }
}
