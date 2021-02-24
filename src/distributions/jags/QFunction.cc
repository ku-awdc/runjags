/*

	This file is Copyright (C) 2002-10 Martyn Plummer, 
	from the source for JAGS version 3.3, licensed under GPL-2
	
	The namespace has been modified to prevent clashes
	This file is not required for JAGS version >= 4 and will be removed
	from runjags once JAGS version 3 becomes obsolete

*/

#include "QFunction.h"

#ifdef INCLUDERSCALARDIST

using std::vector;
using std::string;

namespace runjags {

    QFunction::QFunction(RScalarDist const *dist)
	: DPQFunction(string("q") + dist->name().substr(1), dist)
    {}
    
    double QFunction::evaluate(vector<double const *> const &args) const
    {
	double x = *args[0];
	vector<double const *> param(args.size() - 1);
	for (unsigned int i = 1; i < args.size(); ++i) {
	    param[i-1] = args[i];
	}
	
	return dist()->q(x, param, true, false);
    }

    bool QFunction::checkParameterValue(vector<double const*> const &args) const
    {
	return checkArgs(args);
    }

}

#endif  /* INCLUDERSCALARDIST */
