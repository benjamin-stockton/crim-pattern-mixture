// generated with brms 2.20.4
functions {
  /* hurdle lognormal log-PDF of a single response
   * Args:
   *   y: the response value
   *   mu: mean parameter of the lognormal distribution
   *   sigma: sd parameter of the lognormal distribution
   *   hu: hurdle probability
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real hurdle_lognormal_lpdf(real y, real mu, real sigma, real hu) {
    if (y == 0) {
      return bernoulli_lpmf(1 | hu);
    } else {
      return bernoulli_lpmf(0 | hu) + lognormal_lpdf(y | mu, sigma);
    }
  }
  /* hurdle lognormal log-PDF of a single response
   * logit parameterization of the hurdle part
   * Args:
   *   y: the response value
   *   mu: mean parameter of the lognormal distribution
   *   sigma: sd parameter of the lognormal distribution
   *   hu: linear predictor for the hurdle part
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real hurdle_lognormal_logit_lpdf(real y, real mu, real sigma, real hu) {
    if (y == 0) {
      return bernoulli_logit_lpmf(1 | hu);
    } else {
      return bernoulli_logit_lpmf(0 | hu) + lognormal_lpdf(y | mu, sigma);
    }
  }
  // hurdle lognormal log-CCDF and log-CDF functions
  real hurdle_lognormal_lccdf(real y, real mu, real sigma, real hu) {
    return bernoulli_lpmf(0 | hu) + lognormal_lccdf(y | mu, sigma);
  }
  real hurdle_lognormal_lcdf(real y, real mu, real sigma, real hu) {
    return log1m_exp(hurdle_lognormal_lccdf(y | mu, sigma, hu));
  }
  /* integer sequence of values
   * Args:
   *   start: starting integer
   *   end: ending integer
   * Returns:
   *   an integer sequence from start to end
   */
  array[] int sequence(int start, int end) {
    array[end - start + 1] int seq;
    for (n in 1 : num_elements(seq)) {
      seq[n] = n + start - 1;
    }
    return seq;
  }
  // compute partial sums of the log-likelihood
  real partial_log_lik_lpmf(array[] int seq, int start, int end,
                            data vector Y, data matrix Xc, vector b,
                            real Intercept, real sigma, data matrix Xc_hu,
                            vector b_hu, real Intercept_hu,
                            data array[] int J_1, data vector Z_1_1,
                            vector r_1_1, data array[] int J_2,
                            data vector Z_2_hu_1, vector r_2_hu_1) {
    real ptarget = 0;
    int N = end - start + 1;
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] hu = rep_vector(0.0, N);
    mu += Intercept + Xc[start : end] * b;
    hu += Intercept_hu + Xc_hu[start : end] * b_hu;
    for (n in 1 : N) {
      // add more terms to the linear predictor
      int nn = n + start - 1;
      mu[n] += r_1_1[J_1[nn]] * Z_1_1[nn];
    }
    for (n in 1 : N) {
      // add more terms to the linear predictor
      int nn = n + start - 1;
      hu[n] += r_2_hu_1[J_2[nn]] * Z_2_hu_1[nn];
    }
    for (n in 1 : N) {
      int nn = n + start - 1;
      ptarget += hurdle_lognormal_logit_lpdf(Y[nn] | mu[n], sigma, hu[n]);
    }
    return ptarget;
  }
}
data {
  int<lower=1> N; // total number of observations
  vector[N] Y; // response variable
  int<lower=1> K; // number of population-level effects
  matrix[N, K] X; // population-level design matrix
  int<lower=1> Kc; // number of population-level effects after centering
  int<lower=1> K_hu; // number of population-level effects
  matrix[N, K_hu] X_hu; // population-level design matrix
  int<lower=1> Kc_hu; // number of population-level effects after centering
  int grainsize; // grainsize for threading
  // data for group-level effects of ID 1
  int<lower=1> N_1; // number of grouping levels
  int<lower=1> M_1; // number of coefficients per level
  array[N] int<lower=1> J_1; // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_1;
  // data for group-level effects of ID 2
  int<lower=1> N_2; // number of grouping levels
  int<lower=1> M_2; // number of coefficients per level
  array[N] int<lower=1> J_2; // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_2_hu_1;
  int prior_only; // should the likelihood be ignored?
}
transformed data {
  matrix[N, Kc] Xc; // centered version of X without an intercept
  vector[Kc] means_X; // column means of X before centering
  matrix[N, Kc_hu] Xc_hu; // centered version of X_hu without an intercept
  vector[Kc_hu] means_X_hu; // column means of X_hu before centering
  array[N] int seq = sequence(1, N);
  for (i in 2 : K) {
    means_X[i - 1] = mean(X[ : , i]);
    Xc[ : , i - 1] = X[ : , i] - means_X[i - 1];
  }
  for (i in 2 : K_hu) {
    means_X_hu[i - 1] = mean(X_hu[ : , i]);
    Xc_hu[ : , i - 1] = X_hu[ : , i] - means_X_hu[i - 1];
  }
}
parameters {
  vector[Kc] b; // regression coefficients
  real Intercept; // temporary intercept for centered predictors
  real<lower=0> sigma; // dispersion parameter
  vector[Kc_hu] b_hu; // regression coefficients
  real Intercept_hu; // temporary intercept for centered predictors
  vector<lower=0>[M_1] sd_1; // group-level standard deviations
  array[M_1] vector[N_1] z_1; // standardized group-level effects
  vector<lower=0>[M_2] sd_2; // group-level standard deviations
  array[M_2] vector[N_2] z_2; // standardized group-level effects
}
transformed parameters {
  vector[N_1] r_1_1; // actual group-level effects
  vector[N_2] r_2_hu_1; // actual group-level effects
  real lprior = 0; // prior contributions to the log posterior
  r_1_1 = sd_1[1] * z_1[1];
  r_2_hu_1 = sd_2[1] * z_2[1];
  lprior += normal_lpdf(b | 0, 100);
  lprior += student_t_lpdf(Intercept | 3, 0, 2.5);
  lprior += student_t_lpdf(sigma | 3, 0, 3.4)
            - 1 * student_t_lccdf(0 | 3, 0, 3.4);
  lprior += normal_lpdf(b_hu | 0, 100);
  lprior += student_t_lpdf(Intercept_hu | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_1 | 3, 0, 2.5)
            - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_2 | 3, 0, 2.5)
            - 1 * student_t_lccdf(0 | 3, 0, 2.5);
}
model {
  // likelihood including constants
  if (!prior_only) {
    target += reduce_sum(partial_log_lik_lpmf, seq, grainsize, Y, Xc, b,
                         Intercept, sigma, Xc_hu, b_hu, Intercept_hu, J_1,
                         Z_1_1, r_1_1, J_2, Z_2_hu_1, r_2_hu_1);
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(z_1[1]);
  target += std_normal_lpdf(z_2[1]);
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
  // actual population-level intercept
  real b_hu_Intercept = Intercept_hu - dot_product(means_X_hu, b_hu);
}

