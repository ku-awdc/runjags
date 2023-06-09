/*
	Copyright (C) 2013 Matthew Denwood <matthewdenwood@mac.com>
	
	This code is based on the pareto distribution in JAGS version 3.3,
	and specifies the Lomax distribution
	
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

#include "Constrain.h"
#include <util/nainf.h>
#include <rng/RNG.h>

#include <cmath>
#include <cfloat>

using std::vector;
using std::exp;
using std::log;
using std::pow;

#define LOWER(par) (*par[0])
#define UPPER(par) (*par[1])

#define ALPHA(par) (*par[0])
#define SIGMA(par) (*par[1])

#ifndef INCLUDERSCALARDIST
namespace jags {
#endif  /* INCLUDERSCALARDIST */

namespace runjags {

Constrain::Constrain()
    : RScalarDist("constrain", 3, DIST_POSITIVE)
{}

bool Constrain::checkParameterValue (vector<double const *> const &par) const
{
  return true;
}

double 
Constrain::d(double x, PDFType type,
vector<double const *> const &par, bool give_log) const
{
  double val = x;
  double alpha = LOWER(par);
  double sigma = UPPER(par);

  if (val < alpha)
    return give_log ? JAGS_NEGINF : 0.0;

  if (val > sigma)
    return give_log ? JAGS_NEGINF : 0.0;
  
  return give_log ? 0.0 : 1.0;
  
}

double 
Constrain::p(double x, vector<double const *> const &par, bool lower, bool give_log)
  const
{
  double alpha = ALPHA(par);
  double sigma = SIGMA(par);
  
  if (x < 0)
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
  double logq = -alpha * log(1 + x/sigma);
  if (!lower) {
    return give_log ? logq : exp(logq);
  }
  else {
    return give_log ? log(1 - exp(logq)) : 1 - exp(logq);
  }
}

double 
Constrain::q(double p, vector<double const *> const &par, bool lower, 
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

	x = SIGMA(par) * (exp(logp/-ALPHA(par)) - 1);
	
    return x;
}

double Constrain::r(vector<double const *> const &par, RNG *rng) const
{
    return 0.0;
}

}  // namespace runjags

#ifndef INCLUDERSCALARDIST
}  // namespace jags
#endif  /* INCLUDERSCALARDIST */
