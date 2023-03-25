/* reader.h                                        -*- mode:c; coding:utf-8; -*-
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
#ifndef SAGITTARIUS_PRIVATE_READER_H_
#define SAGITTARIUS_PRIVATE_READER_H_

#include "sagittariusdefs.h"
#include "clos.h"

SG_CLASS_DECL(Sg_SharedRefClass);
SG_CLASS_DECL(Sg_ReadContextClass);
#define SG_CLASS_SHARED_REF   (&Sg_SharedRefClass)
#define SG_CLASS_READ_CONTEXT (&Sg_ReadContextClass)

typedef struct SgSharedRefRec
{
  SG_HEADER;
  SgObject index;
} SgSharedRef;

enum {
  SG_READ_SOURCE_INFO = 1L,
  SG_CHANGE_VM_MODE   = 1L<<1,	/* change VM mode */
  SG_READ_NO_CASE     = 1L<<2,	/* case insensitive */
  SG_READ_CASE        = 1L<<3,	/* case sensitive (for presavation) */
};

/* <read-context> for custom load */
typedef struct SgReadContextRec
{
  SG_HEADER;
  SgHashTable *graph; /* for shared object*/
  int          graphRef;
  long         firstLine;
  long         parsingLineFrom;
  long         parsingLineTo;
  int          escapedp;	/* for |.|, ugly */
  int          flags;
} SgReadContext;
#define SG_STATIC_READ_CONTEXT			\
  {{ SG_CLASS2TAG(SG_CLASS_READ_CONTEXT) }, NULL, FALSE, 0, 0, 0, 0, 0}

#define SG_SHAREDREF_P(obj) SG_XTYPEP(obj, SG_CLASS_SHARED_REF)
#define SG_SHAREDREF(obj)   ((SgSharedRef*)(obj))

#define SG_READ_CONTEXT_P(obj) SG_XTYPEP(obj, SG_CLASS_READ_CONTEXT)
#define SG_READ_CONTEXT(obj)   ((SgReadContext*)(obj))

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_Read(SgObject port, int readSharedObject);
SG_EXTERN SgObject Sg_ReadWithContext(SgObject port, SgReadContext *ctx);
SG_EXTERN SgObject Sg_ReadDelimitedList(SgObject port, SgChar delim,
					int sharedP);
SG_EXTERN readtable_t* Sg_DefaultReadTable();
SG_EXTERN readtable_t* Sg_PortReadTable(SgPort *port);
SG_EXTERN readtable_t* Sg_CopyReadTable(readtable_t *src);
SG_EXTERN void     Sg_SetPortReadTable(SgPort *port, readtable_t *table);
SG_EXTERN readtable_t* Sg_EnsureCopiedReadTable(SgPort *port);

/* for Scheme */
/* returns 2 values */
SG_EXTERN SgObject Sg_GetMacroCharacter(SgChar c, readtable_t *table);
SG_EXTERN void     Sg_SetMacroCharacter(SgChar c, SgObject proc, int nontermP,
					readtable_t *table);
SG_EXTERN SgObject Sg_GetDispatchMacroCharacter(SgChar c, SgChar subc,
						readtable_t *table);
SG_EXTERN int      Sg_MakeDispatchMacroCharacter(SgChar c, int nontermP,
						 readtable_t *table);
SG_EXTERN void     Sg_SetDispatchMacroCharacter(SgChar c, SgChar subc,
						SgObject proc,
						readtable_t *table);
SG_EXTERN void     Sg_EnsureLibraryReadTable(SgLibrary *library);

SG_EXTERN int      Sg_ConstantLiteralP(SgObject o);
SG_EXTERN SgObject Sg_AddConstantLiteral(SgObject o);

SG_EXTERN int      Sg_DelimitedCharP(SgChar c, SgPort *p);

/* misc */
SG_EXTERN SgObject Sg_MakeDefaultReadContext();
SG_EXTERN SgObject Sg_MakeReadContextForLoad();
SG_EXTERN SgObject Sg_ApplyDirective(SgPort *port, SgObject name,
				     SgReadContext *ctx);


SG_CDECL_END

#endif /* SAGITTARIUS_READER_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End
*/
