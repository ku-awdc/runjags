/*
	Copyright (C) 2013 Matthew Denwood <matthewdenwood@mac.com>
	
	This code is based on the pareto distribution in JAGS version 3.3,
	and specifies DuMouchel's prior distribution
	
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

#include "DMouchel.h"
#include <util/nainf.h>
#include <rng/RNG.h>

#include <cmath>
#include <cfloat>

using std::vector;
using std::exp;
using std::log;
using std::pow;

#define SIGMA(par) (*par[0])

#ifndef INCLUDERSCALARDIST
namespace jags {
#endif  /* INCLUDERSCALARDIST */

namespace runjags {

DMouchel::DMouchel()
    : RScalarDist("dmouch", 1, DIST_POSITIVE)
{
}

bool DMouchel::checkParameterValue (vector<double const *> const &par) const
{
  return SIGMA(par) > 0;
}

double 
DMouchel::d(double x, PDFType type,
vector<double const *> const &par, bool give_log) const
{

// Pareto:
/*  if (give_log)
    return log(alpha) + alpha * log(c) - (alpha + 1) * log(x);
  else
    return alpha * exp(alpha * log(c) - (alpha + 1) * log(x));
*/
  
  if (x < 0)
    return give_log ? JAGS_NEGINF : 0;
    
  if (give_log)
  	return log(SIGMA(par)) - 2*log(x+SIGMA(par));
  else
    return SIGMA(par) / pow((x + SIGMA(par)),2);
  
}

double 
DMouchel::p(double x, vector<double const *> const &par, bool lower, bool give_log)
  const
{

// Pareto:
/*  double logq = alpha * log(c/x);
  if (!lower) {
    return give_log ? logq : exp(logq);
  }
  else {
    return give_log ? log(1 - exp(logq)) : 1 - exp(logq);
  }
*/

  if (x < 0)
    return give_log ? JAGS_NEGINF : 0;

  double logq = -log(1 + x/SIGMA(par));
  if (!lower) {
    return give_log ? logq : exp(logq);
  }
  else {
    return give_log ? log(1 - exp(logq)) : 1 - exp(logq);
  }
}

double 
DMouchel::q(double p, vector<double const *> const &par, bool lower, 
	bool log_p) const
{
    if ( (log_p  && p > 0) || (!log_p && (p < 0 || p > 1)) )          
	return JAGS_NAN;
    
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

	x = SIGMA(par) * (exp(-logp) - 1);
	
	
    return x;
}

double DMouchel::r(vector<double const *> const &par, RNG *rng) const
{
    return q(rng->uniform(), par, false, false);
}

}  // namespace runjags

#ifndef INCLUDERSCALARDIST
}  // namespace jags
#endif  /* INCLUDERSCALARDIST */
