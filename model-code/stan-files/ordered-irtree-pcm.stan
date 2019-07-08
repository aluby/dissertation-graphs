functions {
  real pcm(int y, real theta, vector beta) {
    vector[rows(beta) + 1] unsummed;
    vector[rows(beta) + 1] probs;
    unsummed = append_row(rep_vector(0.0, 1), theta - beta);
    probs = softmax(cumulative_sum(unsummed));
    return categorical_lpmf(y + 1 | probs);
  }
}
data {
  int<lower=1> I;                // # items
  int<lower=1> J;                // # persons
  int<lower=1> N1;                // # responses
  int<lower=1> N2;                // # responses
  int<lower=1> N3;                // # responses
  int<lower=1,upper=I> ii1[N1];    // i for n
  int<lower=1,upper=J> jj1[N1];    // j for n
  int<lower=1,upper=I> ii2[N2];    // i for n
  int<lower=1,upper=J> jj2[N2];    // j for n
  int<lower=1,upper=I> ii3[N3];    // i for n
  int<lower=1,upper=J> jj3[N3];    // j for n
  int<lower=0> y1[N1];             // response for n; y = 0, 1 ... m_i
  int<lower=0> y2[N2];             // response for n; y = 0, 1 ... m_i
  int<lower=0> y3[N3];             // response for n; y = 0, 1 ... m_i
  int<lower=1> K;               // # splits in tree
  matrix[I,2] V;                // item covariates
}
transformed data {
  int m;                         // # parameters per item (same for all items)
    m = max(y2);
}
parameters {
  matrix[K+2, I] b_tilde;
  matrix[K, J] theta_tilde;
  cholesky_factor_corr[K] L_OmegaT;
  vector<lower=0>[K] sigmaT;
  cholesky_factor_corr[K+2] L_OmegaB;
  vector<lower=0>[K+2] sigmaB;
  matrix[2, K+2] beta;
}
transformed parameters {
  matrix[I, K+2] b;
  matrix[J, K] theta;
  b = (diag_pre_multiply(sigmaB, L_OmegaB) * b_tilde)';
  theta = (diag_pre_multiply(sigmaT, L_OmegaT) * theta_tilde)';
}
model {
  L_OmegaB ~ lkj_corr_cholesky(4);
  sigmaB ~ cauchy(0, 2.5);
  to_vector(b_tilde) ~ normal(to_vector((V*beta)'),1);
  L_OmegaT ~ lkj_corr_cholesky(4);
  sigmaT ~ cauchy(0, 2.5);
  to_vector(theta_tilde) ~ normal(0,1);
  to_vector(beta) ~ normal(0,5);
  y1 ~ bernoulli_logit(theta[jj1,1] - b[ii1,1]);
  for (n in 1:N2)
    target += pcm(y2[n], theta[jj2[n], 2], b[ii2[n], 2:3]');
  for (n in 1:N3)
    target += pcm(y3[n], theta[jj3[n], 3], b[ii3[n], 4:5]');
} 
generated quantities {
  corr_matrix[K+2] OmegaB;
  corr_matrix[K] OmegaT;
  OmegaB = L_OmegaB * L_OmegaB';
  OmegaT = L_OmegaT * L_OmegaT';
}
