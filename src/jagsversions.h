/*
	Copyright (C) 2015 Matthew Denwood <matthewdenwood@mac.com>
	
	This header file sorts the necessary macros for compiling against 
	JAGS versions 3 vs 4
	
    This file is part of runjags

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

#ifndef JAGS_VERSIONS_H_
#define JAGS_VERSIONS_H_

// Get the installed version of JAGS - undefined for JAGS <= 3:
#include <version.h>
#ifndef JAGS_MAJOR
#define JAGS_MAJOR 3
#endif // JAGS_MAJOR

// Which version of JAGS to assume:
// JAGS_MAJOR_FORCED will always be set - but may be just 0 or may be set from an environmental variable:
#if JAGS_MAJOR_FORCED > 0
#define JAGS_MAJOR_USED JAGS_MAJOR_FORCED
#else
#define JAGS_MAJOR_USED JAGS_MAJOR
#endif

// Will be defined on both platforms - if >0 (windows only) need to make sure that the version of ljags- passed matches what version.h says:
#if JAGS_MAJOR_ASSUMED > 0
#if JAGS_MAJOR_USED != JAGS_MAJOR_ASSUMED
#error "Error detecting the JAGS version - you need to set the 'JAGS_MAJOR_VERSION' environmental variable to the major version of JAGS you have installed - see the INSTALL instruction file"
#endif // JAGS_MAJOR_USED != JAGS_MAJOR_ASSUMED
#else
#define JAGS_MAJOR_ASSUMED 0
#endif // JAGS_MAJOR_ASSUMED

// Check version of JAGS is OK:
#if JAGS_MAJOR_USED > 4
#warning "Compiling against a later version of JAGS than has been tested for this version of runjags ... you should probably update the runjags package!"
#endif

#if JAGS_MAJOR_USED < 3
#error "This version of the runjags package requires compilation against JAGS version 3 or later"
#endif

// Set version specific macros:
#if JAGS_MAJOR_USED == 3
#define INCLUDERSCALARDIST
#endif

#endif // JAGS_VERSIONS_H_
