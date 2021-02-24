/*

	This file is Copyright (C) 2002-10 Martyn Plummer, 
	from the source for JAGS version 3.3, licensed under GPL-2
	
	The namespace has been modified to prevent clashes
	This file is not required for JAGS version >= 4 and will be removed
	from runjags once JAGS version 3 becomes obsolete

*/

#include "DPQFunction.h"

#ifdef INCLUDERSCALARDIST

using std::vector;
using std::string;

namespace runjags {

    DPQFunction::DPQFunction(string const &name, RScalarDist const *dist) 
	: ScalarFunction(name, dist->npar() + 1), _dist(dist)
    {}
    
    RScalarDist const *DPQFunction::dist() const
    {
	return _dist;
    }
    
    bool DPQFunction::checkArgs(vector<double const *> const &args) const
    {
	vector<double const *> param(_dist->npar());
	for (unsigned int i = 0; i < param.size(); ++i) {
	    param[i] = args[i+1];
	}
	
	return _dist->checkParameterValue(param);
    }
}

#endif  /* INCLUDERSCALARDIST */
