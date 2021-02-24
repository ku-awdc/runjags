/*
	Copyright (C) 2013 Matthew Denwood <matthewdenwood@mac.com>
	
	This header is based on the pareto distribution in JAGS version 3.3,
	and specifies the generalised pareto distribution
	
    This file is part of runjags
	Original DPar.h file is Copyright (C) 2002-10 Martyn Plummer, 
	from the source for JAGS version 3.3, licensed under GPL-2

    runjags is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    runjags is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with runjags  If not, see <http://www.gnu.org/licenses/>.
	
 */

#ifndef DGENPAR_H_
#define DGENPAR_H_

// Checks the JAGS version and sets necessary macros:
#include "../jagsversions.h"

#ifndef INCLUDERSCALARDIST
#include <distribution/RScalarDist.h>
namespace jags {
#else
#include "jags/RScalarDist.h"
#endif  /* INCLUDERSCALARDIST */


namespace runjags {

/**
 * <pre>
 * x ~ dgenpar(sigma, mu, xi);
	// when xi != 0:
 * f(x|sigma,mu,xi) = 1/sigma * (1 + xi * (x-mu)/sigma)^-(1/xi +1) ; x >= mu where xi >= 0, mu <= x <= (mu - sigma/xi) for xi < 0
	// when xi == 0:
	f(x|sigma,mu,xi) = 1/sigma * exp(-(x-mu)/sigma) ; ; x >= mu where xi >= 0, mu <= x <= (mu - sigma/xi) for xi < 0
 * </pre>
 * @short Generalised Pareto distribution
 */
class DGenPar : public RScalarDist {
 public:
  DGenPar();

  double d(double x, PDFType type,
	   std::vector<double const *> const &parameters, bool give_log) const;
  double p(double q, std::vector<double const *> const &parameters, bool lower,
	   bool give_log) const;
  double q(double p, std::vector<double const *> const &parameters, bool lower,
	   bool log_p) const;
  double r(std::vector<double const *> const &parameters, RNG *rng) const;
  double l(std::vector<double const*> const &parameters) const;
  double u(std::vector<double const*> const &parameters) const;
  bool checkParameterValue(std::vector<double const *> const &parameters) const;
  bool isSupportFixed(std::vector<bool> const &fixmask) const;
};

}  // namespace runjags

#ifndef INCLUDERSCALARDIST
}  // namespace jags
#endif  /* INCLUDERSCALARDIST */

#endif /* DGENPAR_H_ */
