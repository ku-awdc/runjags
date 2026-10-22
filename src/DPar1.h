/*

	This file is Copyright (C) 2002-10 Martyn Plummer, 
	from the source for JAGS version 3.3, licensed under GPL-2
	
	The only modifications are the change in namespace to prevent
	clashes, and change in name to dpar1

*/

#ifndef DPAR1_H_
#define DPAR1_H_

#include <distribution/RScalarDist.h>

namespace jags {
namespace runjags {

/**
 * <pre>
 * x ~ dpar(alpha, c);
 * f(x|alpha,c) = alpha * c^alpha * x^-(alpha + 1); x > c
 * </pre>
 * @short Pareto distribution
 */
class DPar1 : public RScalarDist {
 public:
  DPar1();

  double d(double x, PDFType type,
	   std::vector<double const *> const &parameters, bool give_log) const;
  double p(double q, std::vector<double const *> const &parameters, bool lower,
	   bool give_log) const;
  double q(double p, std::vector<double const *> const &parameters, bool lower,
	   bool log_p) const;
  double r(std::vector<double const *> const &parameters, RNG *rng) const;
  double l(std::vector<double const*> const &parameters) const;
  double u(std::vector<double const*> const &parameters) const;
  /** 
   * Checks that alpha > 0, c > 0
   */
  bool checkParameterValue(std::vector<double const *> const &parameters) const;
  bool isSupportFixed(std::vector<bool> const &fixmask) const;
};

}  // namespace runjags
}  // namespace jags

#endif /* DPAR1_H_ */
