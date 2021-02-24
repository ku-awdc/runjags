/*

	This file is Copyright (C) 2002-10 Martyn Plummer, 
	from the source for JAGS version 3.3, licensed under GPL-2
	
	The only modifications are the change in namespace to prevent
	clashes, change in name to dpar1, and bug fix for q function
	(line 81)

*/

#include "DPar1.h"
#include <util/nainf.h>
#include <rng/RNG.h>

#include <cmath>
#include <cfloat>

using std::vector;
using std::exp;
using std::log;

#define ALPHA(par) (*par[0])
#define C(par) (*par[1])

#ifndef INCLUDERSCALARDIST
namespace jags {
#endif  /* INCLUDERSCALARDIST */

namespace runjags {

DPar1::DPar1()
    : RScalarDist("dpar1", 2, DIST_SPECIAL)
{}

bool DPar1::checkParameterValue (vector<double const *> const &par) const
{
  return (ALPHA(par) > 0 && C(par) > 0);
}

double 
DPar1::d(double x, PDFType type,
vector<double const *> const &par, bool give_log) const
{
  double alpha = ALPHA(par);
  double c = C(par);

  if (x < c)
    return give_log ? JAGS_NEGINF : 0;

  if (give_log){
    return log(alpha) + alpha * log(c) - (alpha + 1) * log(x);
  }else{
    return alpha * exp(alpha * log(c) - (alpha + 1) * log(x));
  }
}

double 
DPar1::p(double x, vector<double const *> const &par, bool lower, bool give_log)
  const
{
  double alpha = ALPHA(par);
  double c = C(par);
  
  if (x < c)
    return give_log ? JAGS_NEGINF : 0;
  
  double logq = alpha * log(c/x);
  if (!lower) {
    return give_log ? logq : exp(logq);
  }
  else {
    return give_log ? log(1 - exp(logq)) : 1 - exp(logq);
  }
}

double 
DPar1::q(double p, vector<double const *> const &par, bool lower, 
	bool log_p) const
{
    if ( (log_p  && p > 0) || (!log_p && (p < 0 || p > 1)) )          
	return JAGS_NAN;
    
    double logp;

    if (!lower) {
		if (log_p){
		    logp = p;
		}else{
		    logp = log(p);
		}
	} else {
		if (log_p){
		    logp = log(1 - exp(p)); 
		}else{
		    logp = log(1 - p);
	    }
	}
    return exp(log(C(par)) - logp/ALPHA(par));
}

double DPar1::r(vector<double const *> const &par, RNG *rng) const
{
    return C(par) * exp(rng->exponential()/ALPHA(par));
}

double DPar1::l(vector<double const*> const &par) const
{
    return C(par);
}

double DPar1::u(vector<double const*> const &par) const
{
  return JAGS_POSINF;
}

bool DPar1::isSupportFixed(vector<bool> const &fixmask) const
{
    return fixmask[1]; //Fixed if C is fixed
}

}  // namespace runjags

#ifndef INCLUDERSCALARDIST
}  // namespace jags
#endif  /* INCLUDERSCALARDIST */
