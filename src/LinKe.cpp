#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

double linke(NumericVector& param, NumericVector& numbuys, NumericVector& numsells) {
  double rat1 = param[4]/param[3];
  double rat2 = param[4]/param[2];

  double rat1log1p = log1p(rat1);
  double rat2log1p = log1p(rat2);

  double const1 = log(param[4] + param[3]);
  double const2 = log(param[4] + param[2]);

  double prob_no = 1.0 - param[0];
  double prob_good = param[0] * (1.0 - param[1]);
  double prob_bad = param[0] * param[1];

  const NumericVector& e1 = -param[4] - numsells * rat1log1p;
  const NumericVector& e2 = -param[4] - numbuys * rat2log1p;
  const NumericVector& e3 = e2 + e1 + 2.0 * param[4];

  const NumericVector& e_max0 = pmax(e1, e2);
  const NumericVector& e_max = pmax(e_max0, e3);

  double part1 = -numbuys.length() * (param[2] + param[3]) + sum(numbuys) * const2 +
                 sum(numsells) * const1 + sum(e_max);

  double part2 = sum(log(prob_no * exp(e3-e_max) + prob_good * exp(e1 - e_max) + prob_bad * exp(e2 - e_max)));

  double ll = part1 + part2;

  return(ll);
}
