/*

	This file is Copyright (C) 2002-10 Martyn Plummer, 
	from the source for JAGS version 3.3, licensed under GPL-2
	
	The namespace has been modified to prevent clashes
	This file is not required for JAGS version >= 4 and will be removed
	from runjags once JAGS version 3 becomes obsolete

*/

#include "PFunction.h"

#ifdef INCLUDERSCALARDIST

using std::vector;
using std::string;

namespace runjags {

    PFunction::PFunction(RScalarDist const *dist)
	: DPQFunction(string("p") + dist->name().substr(1), dist)
    {}
    
    double PFunction::evaluate(vector<double const *> const &args) const
    {
	double x = *args[0];
	vector<double const *> param(args.size() - 1);
	for (unsigned int i = 1; i < args.size(); ++i) {
	    param[i-1] = args[i];
	}
	
	return dist()->p(x, param, true, false);
    }

    bool 
    PFunction::checkParameterValue(vector<double const *> const &args) const
    {
	if (dist()->discrete()) {
	    double x = *args[0];
	    if (x != static_cast<int>(x))
		return false;
	}

	return checkArgs(args);
    }
}

#endif  /* INCLUDERSCALARDIST */
