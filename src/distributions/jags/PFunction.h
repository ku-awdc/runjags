/*

	This file is Copyright (C) 2002-10 Martyn Plummer, 
	from the source for JAGS version 3.3, licensed under GPL-2
	
	The namespace has been modified to prevent clashes
	This file is not required for JAGS version >= 4 and will be removed
	from runjags once JAGS version 3 becomes obsolete

*/

// Checks the JAGS version and sets necessary macros:
#include "../../jagsversions.h"

#ifdef INCLUDERSCALARDIST

#include "RScalarDist.h"

#ifndef P_FUNCTION_H_
#define P_FUNCTION_H_

#include "DPQFunction.h"

namespace runjags {

    class PFunction : public DPQFunction
    {
    public:
	PFunction(RScalarDist const *dist);
	bool checkParameterValue(std::vector<double const *> const &args) const;
	double evaluate(std::vector <double const *> const &args) const;
    };

}

#endif /* P_FUNCTION_H_ */

#endif  /* INCLUDERSCALARDIST */
