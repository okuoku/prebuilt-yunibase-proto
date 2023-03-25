/* identifier.h                                    -*- mode:c; coding:utf-8; -*-
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
#ifndef SAGITTARIUS_PRIVATE_IDENTIFIER_H_
#define SAGITTARIUS_PRIVATE_IDENTIFIER_H_

#include "sagittariusdefs.h"
#include "clos.h"

SG_CLASS_DECL(Sg_IdentifierClass);
#define SG_CLASS_IDENTIFIER (&Sg_IdentifierClass)

struct SgIdentifierRec
{
  SG_HEADER;
  SgSymbol  *name;
  SgObject   envs;
  SgLibrary *library;
  int        pending;
  SgObject   identity;
};

#define SG_IDENTIFIER(obj)   ((SgIdentifier*)(obj))
#define SG_IDENTIFIERP(obj)  SG_XTYPEP(obj,SG_CLASS_IDENTIFIER)
#define SG_IDENTIFIER_NAME(obj)     (SG_IDENTIFIER(obj)->name)
#define SG_IDENTIFIER_ENVS(obj)     (SG_IDENTIFIER(obj)->envs)
#define SG_IDENTIFIER_LIBRARY(obj)  (SG_IDENTIFIER(obj)->library)
#define SG_IDENTIFIER_PENDING(obj)  (SG_IDENTIFIER(obj)->pending)
#define SG_IDENTIFIER_IDENTITY(obj) (SG_IDENTIFIER(obj)->identity)

/* for my sake, remove this after 0.7.8. */
#define Sg_MakeIdentifier(name, env, lib)	\
  Sg_MakeGlobalIdentifier(name, SG_LIBRARY(lib))

/* for C translator */
#define SG_INIT_IDENTIFIER(id, n, e, i, l, p)	\
  do {						\
    SG_IDENTIFIER(id)->pending = (p);		\
    SG_IDENTIFIER(id)->name = (n);		\
    SG_IDENTIFIER(id)->envs = (e);		\
    SG_IDENTIFIER(id)->identity = (i);		\
    SG_IDENTIFIER(id)->library = (l);		\
  } while (0)

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeGlobalIdentifier(SgObject name, SgLibrary *library);
SG_EXTERN SgObject Sg_MakeRawIdentifier(SgObject name, SgObject envs,
					SgObject identity,
					SgLibrary *library, int pendingP);

SG_CDECL_END

#endif /* SAGITTARIUS_IDENTIFIER_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
