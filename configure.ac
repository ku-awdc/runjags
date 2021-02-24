######################################################################################################
###
###  This file is based on that in the rjags package (version 3-10) with minor modifications
###  [configure stuff relating to modules removed as runjags installs within the package directory]
###  Matthew Denwood, 5th August 2013
###  Original file is Copyright (C) Martyn Plummer, licensed under GPL-2
###
######################################################################################################


AC_INIT([runjags], [2])

if test -z "${R_HOME}"; then
   AC_MSG_ERROR("R_HOME is not defined")
fi
CC=`"${R_HOME}/bin/R" CMD config CC`
CXX=`"${R_HOME}/bin/R" CMD config CXX`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXXFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`

AC_PREFIX_PROGRAM(jags)

#Set optional include path with option --with-jags-include or
#environment variable JAGS_INCLUDE

AC_ARG_WITH([jags-include],
            AC_HELP_STRING([--with-jags-include=INCLUDE_PATH],
                           [the location of JAGS header files]),
            [jags_include_path=$withval])
if test [ -n "$jags_include_path" ] ; then
   JAGS_INCLUDE=${jags_include_path}
else
   if test [ -z "${JAGS_INCLUDE}" ] ; then
      if test [ "$prefix" = "NONE" ]; then
         AC_MSG_ERROR("Location of JAGS headers not defined. Use configure arg '--with-jags-include' or environment variable 'JAGS_INCLUDE'")
      else
         JAGS_INCLUDE=${prefix}/include/JAGS
      fi
   fi
fi

AC_LANG_PUSH(C++)
OCPPFLAGS=${CPPFLAGS}
CPPFLAGS=-I${JAGS_INCLUDE}
AC_CHECK_HEADER(Console.h, [],
             [AC_MSG_FAILURE("Problem with header file ${JAGS_INCLUDE}/Console.h ")])
CPPFLAGS=${OCPPFLAGS}
AC_LANG_POP()


# There is no consensus on where 64- versus 32-bit libraries should be
# installed.  We take a hint from R, assuming that if R is installed
# in, say, /usr/LIBnn/R then we can find the jags library in
# ${prefix}/LIBnn.
#
# This is a reasonable assumption if both JAGS and R have been
# properly packaged, but may not be true if either has been manually
# installed. The user can set the environment variable LIBnn to
# override this.

if test -z "${LIBnn}"; then 
   LIBnn=`"${R_HOME}/bin/R" CMD config LIBnn`
fi

#Set optional library path with option --with-jags-lib or
#environment variable JAGS_LIBS

AC_ARG_WITH([jags-lib],
            AC_HELP_STRING([--with-jags-lib=LIB_PATH],
                           [the location of the JAGS library]),
            [jags_lib_path=$withval])
if test [ -n "$jags_lib_path" ] ; then
   JAGS_LIB=$jags_lib_path
else
   if test [ -z "${JAGS_LIB}" ] ; then
      if test [ "${prefix}" = "NONE" ] ; then 
         AC_MSG_ERROR("Location of JAGS library not defined. Use configure arg '--with-jags-lib' or environment variable 'JAGS_LIB'")
      else
         JAGS_LIB=${prefix}/${LIBnn}
      fi
   fi
fi

# Support for private installations of JAGS. If the jags library is not
# on the linker path then it helps to hard-code the location of JAGS into
# the DSO for rjags. Suggested by Doug Bates, April 2011.

AC_ARG_ENABLE([rpath],
            AC_HELP_STRING([--enable-rpath],
                           [Hard link the location of the JAGS library]))
if test [ "x$enable_rpath" = "xyes"]; then
   JAGS_RPATH="-Wl,-rpath ${JAGS_LIB}"
fi

OLDFLAGS=${LDFLAGS}
AC_LANG_PUSH(C)
LDFLAGS="-L${JAGS_LIB}"
AC_CHECK_LIB(jags, jags_version, [],
             [AC_MSG_ERROR("Cannot load JAGS library in ${JAGS_LIB}")])

AC_LANG_POP()
LDFLAGS=${OLDFLAGS}


AC_SUBST(JAGS_INCLUDE)
AC_SUBST(JAGS_LIB)
AC_SUBST(JAGS_RPATH)

AC_OUTPUT(src/Makevars)
