/* closure.h                                       -*- mode:c; coding:utf-8; -*-
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
#ifndef SAGITTARIUS_PRIVATE_CLOSURE_H_
#define SAGITTARIUS_PRIVATE_CLOSURE_H_

#include "sagittariusdefs.h"
#include "subr.h"

struct SgClosureRec
{
  SgProcedure common;
  SgObject    code;		/* code builder */
  SgObject    frees[1];
};

#define SG_CLOSURE(obj)   ((SgClosure*)(obj))
#define SG_CLOSUREP(obj)  (SG_PROCEDUREP(obj) && (SG_PROCEDURE_TYPE(obj)) == SG_PROC_CLOSURE)
#define SG_CLOSURE_MAX_STACK(obj) (SG_CODE_BUILDER(SG_CLOSURE(obj)->code)->maxStack)

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeClosure(SgObject code, SgObject *frees);
SG_EXTERN SgObject Sg_VMMakeClosure(SgObject code, int self, SgObject *frees);
SG_EXTERN int      Sg_ClosureTransparent(SgObject closure);

SG_CDECL_END

#endif /* SAGITTARIUS_CLOSURE_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End
*/
