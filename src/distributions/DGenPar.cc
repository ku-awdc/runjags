/*
	Copyright (C) 2013 Matthew Denwood <matthewdenwood@mac.com>
	
	This code is based on the pareto distribution in JAGS version 3.3,
	and specifies the generalised pareto distribution

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

#include "DGenPar.h"
#include <util/nainf.h>
#include <rng/RNG.h>

#include <cmath>
#include <cfloat>

using std::vector;
using std::exp;
using std::log;
using std::pow;

#define SIGMA(par) (*par[0])
#define MU(par) (*par[1])
#define XI(par) (*par[2])

#ifndef INCLUDERSCALARDIST
namespace jags {
#endif  /* INCLUDERSCALARDIST */

namespace runjags {

DGenPar::DGenPar()
    : RScalarDist("dgenpar", 3, DIST_SPECIAL)
{}

bool DGenPar::checkParameterValue (vector<double const *> const &par) const
{
  return (SIGMA(par) > 0);
}

double 
DGenPar::d(double x, PDFType type,
vector<double const *> const &par, bool give_log) const
{
  double xi = XI(par);
  double sigma = SIGMA(par);
  double mu = MU(par);
  
  if (xi < 0){  	
	  if (x < mu || (mu - sigma/xi) < x)
	      return give_log ? JAGS_NEGINF : 0;	
  }else{
	  if(x < mu)
	      return give_log ? JAGS_NEGINF : 0;	  		
  }

// Pareto:
/*  if (give_log)
    return log(alpha) + alpha * log(c) - (alpha + 1) * log(x);
  else
    return alpha * exp(alpha * log(c) - (alpha + 1) * log(x));
*/
  
  // The PDF depends on the value of xi - if 0 then we have a simpler equation not involving xi (http://www.mathwave.com/articles/generalized_pareto_distribution.html):  
  if (std::abs(xi) < 0.000000001){
	  if (give_log)
	  	return log(1/sigma) + -(x-mu)/sigma;
	  else
	    return 1/sigma * exp(-(x-mu)/sigma);
  }else{
	  if (give_log)
	  	return log(1/sigma) + -(1/xi +1)*log(1 + xi * (x-mu)/sigma);
	  else
	    return 1/sigma * pow(1 + xi * (x-mu)/sigma,-(1/xi +1));   
  }
  
}

double 
DGenPar::p(double x, vector<double const *> const &par, bool lower, bool give_log)
  const
{
  double xi = XI(par);
  double sigma = SIGMA(par);
  double mu = MU(par);
  
  if (xi < 0){  	
	  if (x < mu || (mu - sigma/xi) < x)
	      return give_log ? JAGS_NEGINF : 0;	
  }else{
	  if(x < mu)
	      return give_log ? JAGS_NEGINF : 0;	  		
  }

// Pareto:
/*  double logq = alpha * log(c/x);
  if (!lower) {
    return give_log ? logq : exp(logq);
  }
  else {
    return give_log ? log(1 - exp(logq)) : 1 - exp(logq);
  }
*/
  double logq;
  // The survival function depends on the value of xi - if 0 then we have a simpler equation not involving xi (wikipedia):
  if (std::abs(xi) < 0.000000001){
	  logq = (mu-x)/sigma;
  }else{
	  logq = -log((xi*((x-mu)/sigma))+1)/xi;
  }

  if (!lower) {
    return give_log ? logq : exp(logq);
  }
  else {
    return give_log ? log(1 - exp(logq)) : 1 - exp(logq);
  }
}

double 
DGenPar::q(double p, vector<double const *> const &par, bool lower, 
	bool log_p) const
{
    if ( (log_p  && p > 0) || (!log_p && (p < 0 || p > 1)) )          
	return JAGS_NAN;
    
    double xi = XI(par);
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
	
    // The survival function depends on the value of xi - if 0 then we have a simpler equation not involving xi:
    if (std::abs(xi) < 0.000000001){
   	  x = mu-(logp*sigma);
    }else{
  	  x = ((exp(-logp*xi) -1)/xi) * sigma + mu;
    }

    return x;
}

double DGenPar::r(vector<double const *> const &par, RNG *rng) const
{
    return q(rng->uniform(), par, false, false);
}

double DGenPar::l(vector<double const*> const &par) const
{
    return MU(par);
}

double DGenPar::u(vector<double const*> const &par) const
{
  if (XI(par) < 0){  	
  	  return JAGS_POSINF;
  }else{
   	  return (MU(par) - SIGMA(par)/XI(par));
  }
	
}

bool DGenPar::isSupportFixed(vector<bool> const &fixmask) const
{
    return fixmask[1]; //Fixed if MU is fixed
}

}  // namespace runjags

#ifndef INCLUDERSCALARDIST
}  // namespace jags
#endif  /* INCLUDERSCALARDIST */

