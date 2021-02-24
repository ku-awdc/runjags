/*
	Copyright (C) 2013 Matthew Denwood <matthewdenwood@mac.com>
	
	This code allows the distributions provided by the runjags module
	to be tested from within R.  For the R code necessary to run this 
	function see the tests/checkmodule.R file
	
    This file is part of runjags

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

#include <R.h>
#include <Rmath.h>

// Checks the JAGS version and sets necessary macros:
#include "jagsversions.h"

// Trivial function to check which version of JAGS the binary was compiled against:
extern "C" {
void getjagsversions(int *forced, int *assumed, int *detected, int *used) // All pointers as everything from R is a vector
{
	forced[0] = (int) JAGS_MAJOR_FORCED;
	assumed[0] = (int) JAGS_MAJOR_ASSUMED;
	detected[0] = (int) JAGS_MAJOR;
	used[0] = (int) JAGS_MAJOR_USED;
}
}
// library(runjags); runjags:::dynloadmodule(); .C('getjagsversions', PACKAGE='runjags', forced=integer(1), assumed=integer(1), detected=integer(1), used=integer(1))
	

#ifndef INCLUDERSCALARDIST

//#if JAGS_MAJOR < 4
//#error "JAGS version 4 required"
//#endif

	// For JAGS version >= 4

#include <module/Module.h>

#include <version.h>

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

#include <iostream>
#include <vector>

using std::vector;

/*
// This RNG class works as far as I can tell, but throws a memory issue if I pass to the distributions - not important anyway!
class RNG
	{
	public:
		RNG(){
			GetRNGstate(); // Must be done for random number generation			
		}
		
		double uniform(){
			double val = runif(0,1);			
			return val;
		}
		
		double exponential(){
			double val = rexp(1);
			return val;
		}
		
		~RNG(){
			PutRNGstate(); // Must be done for random number generation
		}
	};	
*/
	
namespace jags{
namespace runjags{
	
int runjagstestingfun(int disttype, int dpqr, int uselogi, int loweri, int N, double x[], int npars, double parameters[], double *values){
	
	// Cast ints to bool in C++ ... will hopefully fix issues with (endianness or c/c++ issues?) problems on sparc
	bool uselog = uselogi;
	bool lower = loweri;
	
	// Virtual class to be replaced with specific class (and check the number of parameters - return a status 1 if wrong):
	RScalarDist *dist;
	if(disttype==1){
		if(npars!=2) return 1;
		dist = new DPar1();
	}else if(disttype==2){
		if(npars!=3) return 1;
		dist = new DPar2();
	}else if(disttype==3){
		if(npars!=3) return 1;
		dist = new DPar3();
	}else if(disttype==4){
		if(npars!=4) return 1;
		dist = new DPar4();
	}else if(disttype==5){
		if(npars!=2) return 1;
		dist = new DLomax();
	}else if(disttype==6){
		if(npars!=1) return 1;
		dist = new DMouchel();
	}else if(disttype==7){
		if(npars!=3) return 1;
		dist = new DGenPar();
	}else if(disttype==8){
		if(npars!=1) return 1;
		dist = new DHalfCauchy();
	}else{
		// Return status 2 if distribution not recognised:
		return 2;
	}
	
	// Have to setup a fake vector to transfer to a const vector:
	std::vector<double *> fake;
	for(int i=0; i<npars; i++){
		fake.push_back(&parameters[i]);
	}
	const std::vector<const double *> pars (fake.begin(), fake.end());  

	// Return status 3 if parameters not valid:
	if(dist->checkParameterValue(pars)!=1){
		delete dist;
		return 3;
	}
	
	if(dpqr==1){
		PDFType pdft = PDF_FULL;
		for(int i=0; i<N; i++){
			values[i] = dist->d(x[i],pdft,pars,uselog);
		}
	}else if(dpqr==2){
		for(int i=0; i<N; i++){
			values[i] = dist->p(x[i],pars,lower,uselog);
		}
	}else if(dpqr==3){
		for(int i=0; i<N; i++){
			values[i] = dist->q(x[i],pars,lower,uselog);
		}
	}else if(dpqr==4){
		/*
		// This doesn't work, but it's not important as all r functions pass straight through to q anyway:
		RNG *rngt = new RNG();
		std::cout << rngt->uniform() << std::endl;
		for(int i=0; i<N; i++){
			// For some reason this gives a segfault:
			values[i] = dist->r(pars,rngt);
		}
		delete rngt;
		*/
		// Return status 5 if trying to use r functions:
		delete dist;
		return 5;
	}else{
		// Return status 4 if function type not valid:
		delete dist;
		return 4;
	}
		
	// Cleanup and exit status 0:
	delete dist;	  
	return 0;	
}

}  // namespace runjags
}  // namespace jags

extern "C" {

void testrunjags(int *disttype, int *dpqr, int *uselog, int *lower, int *N, double *x, int *npars, double *parameters, double *values, int *status) // All pointers as everything from R is a vector
{

	status[0] = jags::runjags::runjagstestingfun(disttype[0], dpqr[0], uselog[0], lower[0], N[0], x, npars[0], parameters, values);
	
}

}

#else
	// For JAGS version <= 3

#include "distributions/jags/RScalarDist.h"
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

#include <iostream>
#include <vector>

using std::vector;

/*
// This RNG class works as far as I can tell, but throws a memory issue if I pass to the distributions - not important anyway!
class RNG
	{
	public:
		RNG(){
			GetRNGstate(); // Must be done for random number generation			
		}
		
		double uniform(){
			double val = runif(0,1);			
			return val;
		}
		
		double exponential(){
			double val = rexp(1);
			return val;
		}
		
		~RNG(){
			PutRNGstate(); // Must be done for random number generation
		}
	};	
*/
	
namespace runjags{
	
int runjagstestingfun(int disttype, int dpqr, int uselogi, int loweri, int N, double x[], int npars, double parameters[], double *values){
	
	// Cast ints to bool in C++ ... will hopefully fix issues with (endianness or c/c++ issues?) problems on sparc
	bool uselog = uselogi;
	bool lower = loweri;
	
	// Virtual class to be replaced with specific class (and check the number of parameters - return a status 1 if wrong):
	RScalarDist *dist;
	if(disttype==1){
		if(npars!=2) return 1;
		dist = new DPar1();
	}else if(disttype==2){
		if(npars!=3) return 1;
		dist = new DPar2();
	}else if(disttype==3){
		if(npars!=3) return 1;
		dist = new DPar3();
	}else if(disttype==4){
		if(npars!=4) return 1;
		dist = new DPar4();
	}else if(disttype==5){
		if(npars!=2) return 1;
		dist = new DLomax();
	}else if(disttype==6){
		if(npars!=1) return 1;
		dist = new DMouchel();
	}else if(disttype==7){
		if(npars!=3) return 1;
		dist = new DGenPar();
	}else if(disttype==8){
		if(npars!=1) return 1;
		dist = new DHalfCauchy();
	}else{
		// Return status 2 if distribution not recognised:
		return 2;
	}
	
	// Have to setup a fake vector to transfer to a const vector:
	std::vector<double *> fake;
	for(int i=0; i<npars; i++){
		fake.push_back(&parameters[i]);
	}
	const std::vector<const double *> pars (fake.begin(), fake.end());  

	// Return status 3 if parameters not valid:
	if(dist->checkParameterValue(pars)!=1){
		delete dist;
		return 3;
	}
	
	if(dpqr==1){
		PDFType pdft = PDF_FULL;
		for(int i=0; i<N; i++){
			values[i] = dist->d(x[i],pdft,pars,uselog);
		}
	}else if(dpqr==2){
		for(int i=0; i<N; i++){
			values[i] = dist->p(x[i],pars,lower,uselog);
		}
	}else if(dpqr==3){
		for(int i=0; i<N; i++){
			values[i] = dist->q(x[i],pars,lower,uselog);
		}
	}else if(dpqr==4){
		/*
		// This doesn't work, but it's not important as all r functions pass straight through to q anyway:
		RNG *rngt = new RNG();
		std::cout << rngt->uniform() << std::endl;
		for(int i=0; i<N; i++){
			// For some reason this gives a segfault:
			values[i] = dist->r(pars,rngt);
		}
		delete rngt;
		*/
		// Return status 5 if trying to use r functions:
		delete dist;
		return 5;
	}else{
		// Return status 4 if function type not valid:
		delete dist;
		return 4;
	}
		
	// Cleanup and exit status 0:
	delete dist;	  
	return 0;	
}
}  // namespace runjags

extern "C" {

void testrunjags(int *disttype, int *dpqr, int *uselog, int *lower, int *N, double *x, int *npars, double *parameters, double *values, int *status) // All pointers as everything from R is a vector
{

	status[0] = runjags::runjagstestingfun(disttype[0], dpqr[0], uselog[0], lower[0], N[0], x, npars[0], parameters, values);
	
}

}  

#endif  /* INCLUDERSCALARDIST */
