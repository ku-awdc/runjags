/*
	Copyright (C) 2013 Matthew Denwood <matthewdenwood@mac.com>
	
	This code is based on the pareto distribution in JAGS version 3.3,
	and specifies the pareto type 2 distribution
	
    This file is part of runjags
	Original DPar.cc file is Copyright (C) 2002-10 Martyn Plummer, 
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

#include "DPar2.h"
#include <util/nainf.h>
#include <rng/RNG.h>

#include <cmath>
#include <cfloat>

using std::vector;
using std::exp;
using std::log;
using std::pow;

#define ALPHA(par) (*par[0])
#define SIGMA(par) (*par[1])
#define MU(par) (*par[2])

#ifndef INCLUDERSCALARDIST
namespace jags {
#endif  /* INCLUDERSCALARDIST */

namespace runjags {

DPar2::DPar2()
    : RScalarDist("dpar2", 3, DIST_SPECIAL)
{}

bool DPar2::checkParameterValue (vector<double const *> const &par) const
{
  return (ALPHA(par) > 0 && SIGMA(par) > 0);
}

double 
DPar2::d(double x, PDFType type,
vector<double const *> const &par, bool give_log) const
{
  double alpha = ALPHA(par);
  double sigma = SIGMA(par);
  double mu = MU(par);

  if (x < mu)
    return give_log ? JAGS_NEGINF : 0;

// Pareto:
/*  if (give_log)
    return log(alpha) + alpha * log(c) - (alpha + 1) * log(x);
  else
    return alpha * exp(alpha * log(c) - (alpha + 1) * log(x));
*/
    
  if (give_log)
  	return (log(alpha)-log(sigma)) - (alpha+1) * log(((sigma+x)-mu)/sigma);
  else
    return alpha/sigma * pow((((sigma+x)-mu)/sigma),-(alpha + 1));
  
}

double 
DPar2::p(double x, vector<double const *> const &par, bool lower, bool give_log)
  const
{
  double alpha = ALPHA(par);
  double sigma = SIGMA(par);
  double mu = MU(par);
  
  if (x < mu)
    return give_log ? JAGS_NEGINF : 0;

// Pareto:
/*  double logq = alpha * log(c/x);
  if (!lower) {
    return give_log ? logq : exp(logq);
  }
  else {
    return give_log ? log(1 - exp(logq)) : 1 - exp(logq);
  }
*/
  // Survival function:
  double logq = -alpha * log(1 + (x-mu)/sigma);
  if (!lower) {
    return give_log ? logq : exp(logq);
  }
  else {
    return give_log ? log(1 - exp(logq)) : 1 - exp(logq);
  }
}

double 
DPar2::q(double p, vector<double const *> const &par, bool lower, 
	bool log_p) const
{
    if ( (log_p  && p > 0) || (!log_p && (p < 0 || p > 1)) )          
	return JAGS_NAN;
    
    double alpha = ALPHA(par);
    double sigma = SIGMA(par);
    double mu = MU(par);

    double logp;
	double x;

    if (!lower) {
	if (log_p)
	    logp = p;
	else
	    logp = log(p);
    }
    else {
	if (log_p)
	    logp = log(1 - exp(p)); 
	else
	    logp = log(1 - p);
    }
  	
// Pareto:
//	x = exp(log(C(par)) - logp/ALPHA(par));
	x = ((exp(logp / -alpha) - 1) * sigma) + mu;
		
    return x;
}

double DPar2::r(vector<double const *> const &par, RNG *rng) const
{
    return q(rng->uniform(), par, false, false);
}

double DPar2::l(vector<double const*> const &par) const
{
    return MU(par);
}

double DPar2::u(vector<double const*> const &par) const
{
  return JAGS_POSINF;
}

}  // namespace runjags

#ifndef INCLUDERSCALARDIST
}  // namespace jags
#endif  /* INCLUDERSCALARDIST */
