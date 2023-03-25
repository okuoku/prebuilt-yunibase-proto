/* vector.h                                        -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2021  Takashi Kato <ktakashi@ymail.com>
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *  $Id: $
 */
#ifndef SAGITTARIUS_PRIVATE_VECTOR_H_
#define SAGITTARIUS_PRIVATE_VECTOR_H_

#include "sagittariusdefs.h"
#include "clos.h"

#include <sagittarius/vectors.h>

SG_CLASS_DECL(Sg_VectorClass);
#define SG_CLASS_VECTOR (&Sg_VectorClass)

struct SgVectorRec
{
  SG_HEADER;
  unsigned long literalp: 1;
  long size             : (SIZEOF_LONG*CHAR_BIT-1);
  SgObject elements[1];
};

#define SG_VECTOR(obj)       ((SgVector*)obj)
#define SG_VECTORP(obj)      SG_XTYPEP(obj, SG_CLASS_VECTOR)
#define SG_VECTOR_SIZE(obj)   	  (SG_VECTOR(obj)->size)
#define SG_VECTOR_ELEMENTS(obj)   (SG_VECTOR(obj)->elements)
#define SG_VECTOR_ELEMENT(obj, i) (SG_VECTOR(obj)->elements[i])

#define SG_LITERAL_VECTORP(obj)				\
  (SG_VECTORP(obj) && SG_VECTOR(obj)->literalp)
#define SG_VECTOR_SET_LITERAL(obj)					\
  (SG_VECTOR(obj)->literalp = 1)

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_VectorReverseX(SgObject vec, long start, long end);

SG_CDECL_END

#endif /* SAGITTARIUS_VECTOR_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
