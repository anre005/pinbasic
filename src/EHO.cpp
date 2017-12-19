#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

double eho(NumericVector param , NumericVector numbuys, NumericVector numsells) {
  double n_par = -numbuys.length() * (param[3] + param[2]);

  NumericVector m = pmin(numbuys, numsells) + 0.5 * pmax(numbuys, numsells);

  double helper_sum1 = param[4] + param[3];
  double helper_sum2 = param[4] + param[2];

  double xs = param[3]/helper_sum1;
  double xb = param[2]/helper_sum2;
  double log_xs = log(xs);
  double log_xb = log(xb);

  double exp_mu = exp(-param[4]);
  NumericVector xs_helper = exp((numsells - m) * log_xs);
  NumericVector xb_helper = exp((numbuys - m) * log_xb);

  double prob_no = 1.0 - param[0];
  double prob_good = param[0] * (1.0 - param[1]);
  double prob_bad = param[0] * param[1];

  double part1 = n_par +  (log_xb + log_xs) * sum(m) +
                  log(param[4] + param[2]) * sum(numbuys) +
                  log(param[4] + param[3]) * sum(numsells);

  double part2 = sum(log(prob_no * xs_helper * xb_helper +
                          prob_good * exp_mu * xs_helper * exp(-m * log_xb) +
                          prob_bad * exp_mu * xb_helper * exp(-m * log_xs)));

  double ll = part1 + part2;
  return(ll);
}