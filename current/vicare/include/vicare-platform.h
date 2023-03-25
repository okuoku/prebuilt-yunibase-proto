/*
   Part of: Vicare Scheme
   Contents: platform specific header file
   Date: Sat Jun 22, 2013

   Abstract



   Copyright (C) 2013, 2017 Marco Maggi <marco.maggi-ipsu@poste.it>

   This program is free software:  you can redistribute it and/or modify
   it under the terms of the  GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or (at
   your option) any later version.

   This program is  distributed in the hope that it  will be useful, but
   WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
   MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
   General Public License for more details.

   You should  have received a  copy of  the GNU General  Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef VICARE_PLATFORM_H
#define VICARE_PLATFORM_H 1


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#include <stdint.h>
#include <limits.h>


/** --------------------------------------------------------------------
 ** Platform inspection.
 ** ----------------------------------------------------------------- */

#if (4 == 8)
#  undef  IK_64BIT_PLATFORM
#  define IK_32BIT_PLATFORM	1
#  define wordsize		4
#  define IK_WORDSIZE		4
#else
#  undef  IK_32BIT_PLATFORM
#  define IK_64BIT_PLATFORM	1
#  define wordsize		8
#  define IK_WORDSIZE		8
#endif

/* The value of "wordshift" is selected in such a way that:

     length_in_bytes = number_of_words * wordsize
		     = number_of_words << wordshift

   this	 allows us,  for example,  to take  the fixnum	representing the
   number of items  in a vector and consider it directly  as size of the
   vector's data area in bytes. */
#if IK_32BIT_PLATFORM
#  define wordshift		2
#  define IK_WORDSHIFT		2
#else
#  define wordshift		3
#  define IK_WORDSHIFT		3
#endif


/** --------------------------------------------------------------------
 ** Machine words.
 ** ----------------------------------------------------------------- */

#if IK_32BIT_PLATFORM
#  define IK_SWORD_MAX		((int32_t)+2147483647L)
#  define IK_SWORD_MIN		((int32_t)-2147483648L)
#  define IK_UWORD_MAX		((uint32_t)0xFFFFFFFFL)		/* = 4294967295 */
#else
#  define IK_SWORD_MAX		((int64_t)+9223372036854775807L)
#  define IK_SWORD_MIN		((int64_t)-9223372036854775808L)
#  define IK_UWORD_MAX		((uint64_t)0xFFFFFFFFFFFFFFFFL)	/* = 18446744073709551615 */
#endif

#if IK_32BIT_PLATFORM
typedef int32_t			ik_sword_t; /* signed machine word */
typedef uint32_t		ik_uword_t; /* UNsigned machine word */
#else
typedef int64_t			ik_sword_t;
typedef uint64_t		ik_uword_t;
#endif


/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */

#endif  /* defined VICARE_PLATFORM_H */

/* end of file */
