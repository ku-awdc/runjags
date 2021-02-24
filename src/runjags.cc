/*
	Copyright (C) 2013 Matthew Denwood <matthewdenwood@mac.com>
	
	This code creates a JAGS module from the distributions provided

    This file is part of runjags

	This code is based on the source code for JAGS and the following tutorial:
	Wabersich, D., Vandekerckhove, J., 2013. Extending JAGS: A tutorial on 
	adding custom distributions to JAGS (with a diffusion model example). 
	Behavior Research Methods.
	JAGS is Copyright (C) 2002-10 Martyn Plummer, licensed under GPL-2

	This version of the module is compatible with JAGS version 3 and 4.
	Some necessary modifications are controlled using the INCLUDERSCALARDIST
	macro which is defined by makevars if JAGS version 3 is detected.
	Once JAGS version 3 becomes obsolete the redundant code will be
	removed from runjags.
	
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

// Checks the JAGS version and sets necessary macros:
#include "jagsversions.h"


#ifndef INCLUDERSCALARDIST
	// For JAGS version >=4

#include <module/Module.h>

#include <function/DFunction.h>
#include <function/PFunction.h>
#include <function/QFunction.h>

//#include "functions/myfun.h"
#include "distributions/DLomax.h"
#include "distributions/DMouchel.h"
#include "distributions/DPar1.h"
#include "distributions/DPar2.h"
#include "distributions/DPar3.h"
#include "distributions/DPar4.h"
#include "distributions/DGenPar.h"
#include "distributions/DHalfCauchy.h"

using std::vector;

namespace jags {
namespace runjags {

	class runjagsModule : public Module {
	  public:
	    runjagsModule();
	    ~runjagsModule();
		
		void Rinsert(RScalarDist *dist);
	};

runjagsModule::runjagsModule() : Module("runjags")
{
  // insert is the standard way to add a distribution or function
  // insert(new myfun);
  // insert(new mydist);
  
  // Rinsert (copied from jags) adds the distribution, as well as d,f,q functions simultaneously:
  Rinsert(new DPar1);
  Rinsert(new DPar2);
  Rinsert(new DPar3);
  Rinsert(new DPar4);
  Rinsert(new DLomax);
  Rinsert(new DMouchel);
  Rinsert(new DGenPar);
  Rinsert(new DHalfCauchy);

}


// From jags:
void runjagsModule::Rinsert(RScalarDist *dist)
{
	insert(dist);    
	insert(new DFunction(dist));
	insert(new PFunction(dist));
	insert(new QFunction(dist));
}


runjagsModule::~runjagsModule()
{
  vector<Function*> const &fvec = functions();
  for (unsigned int i = 0; i < fvec.size(); ++i) {
    delete fvec[i];
  }
  vector<Distribution*> const &dvec = distributions();
  for (unsigned int i = 0; i < dvec.size(); ++i) {
    delete dvec[i];
  }
}

}  // namespace runjags
}  // namespace jags

jags::runjags::runjagsModule _runjags_module;



#else
	// For JAGS version <=3

#include <Module.h>

#include "distributions/jags/DFunction.h"
#include "distributions/jags/QFunction.h"
#include "distributions/jags/PFunction.h"

//#include "functions/myfun.h"
#include "distributions/DLomax.h"
#include "distributions/DMouchel.h"
#include "distributions/DPar1.h"
#include "distributions/DPar2.h"
#include "distributions/DPar3.h"
#include "distributions/DPar4.h"
#include "distributions/DGenPar.h"
#include "distributions/DHalfCauchy.h"

using std::vector;

namespace runjags {

	class runjagsModule : public Module {
	  public:
	    runjagsModule();
	    ~runjagsModule();
		
		void Rinsert(RScalarDist *dist);
	};

runjagsModule::runjagsModule() : Module("runjags")
{
  // insert is the standard way to add a distribution or function
  // insert(new myfun);
  // insert(new mydist);
  
  // Rinsert (copied from jags) adds the distribution, as well as d,f,q functions simultaneously:
  Rinsert(new DPar1);
  Rinsert(new DPar2);
  Rinsert(new DPar3);
  Rinsert(new DPar4);
  Rinsert(new DLomax);
  Rinsert(new DMouchel);
  Rinsert(new DGenPar);
  Rinsert(new DHalfCauchy);

}


// From jags:
void runjagsModule::Rinsert(RScalarDist *dist)
{
	insert(dist);    
	insert(new DFunction(dist));
	insert(new PFunction(dist));
	insert(new QFunction(dist));
}


runjagsModule::~runjagsModule()
{
  vector<Function*> const &fvec = functions();
  for (unsigned int i = 0; i < fvec.size(); ++i) {
    delete fvec[i];
  }
  vector<Distribution*> const &dvec = distributions();
  for (unsigned int i = 0; i < dvec.size(); ++i) {
    delete dvec[i];
  }
}

}  // namespace runjags
runjags::runjagsModule _runjags_module;


#endif  /* INCLUDERSCALARDIST */



