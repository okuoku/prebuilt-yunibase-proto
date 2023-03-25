/* macro.h                                         -*- mode:c; coding:utf-8; -*-
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
#ifndef SAGITTARIUS_PRIVATE_MACRO_H_
#define SAGITTARIUS_PRIVATE_MACRO_H_

#include "sagittariusdefs.h"
#include "clos.h"

SG_CLASS_DECL(Sg_SyntaxClass);
SG_CLASS_DECL(Sg_MacroClass);
#define SG_CLASS_SYNTAX (&Sg_SyntaxClass)
#define SG_CLASS_MACRO  (&Sg_MacroClass)

/* syntax object */
struct SgSyntaxRec
{
  SG_HEADER;
  SgSymbol *name;
  SgObject  proc;
};

#define SG_SYNTAX(obj)      ((SgSyntax*)(obj))
#define SG_SYNTAXP(obj)     SG_XTYPEP(obj, SG_CLASS_SYNTAX)
#define SG_SYNTAX_NAME(obj) (SG_SYNTAX(obj)->name)
#define SG_SYNTAX_PROC(obj) (SG_SYNTAX(obj)->proc)

#define SG_BUILTIN_SYNTXP(obj) (SG_SYNTAXP(obj) && !SG_SYNTAX(obj)->userDefined)
#define SG_USER_DEFINED_SYNTXP(obj) (SG_SYNTAXP(obj) && SG_SYNTAX(obj)->userDefined)

struct SgMacroRec
{
  SG_HEADER;
  SgObject   name;
  SgClosure *transformer;
  SgObject   data;
  SgObject   env;		/* macro defined time p1env */
  SgCodeBuilder *compiledCode;	/* for cache */
};

#define SG_MACRO(obj)    ((SgMacro*)(obj))
#define SG_MACROP(obj)   SG_XTYPEP(obj, SG_CLASS_MACRO)

/* for C translator */
#define SG_INIT_MACRO(m, n, t, d, e, c)	\
  do {					\
    SG_MACRO(m)->name = (n);		\
    SG_MACRO(m)->transformer = (t);	\
    SG_MACRO(m)->data = (d);		\
    SG_MACRO(m)->env = (e);		\
    SG_MACRO(m)->compiledCode =(c);	\
  } while (0)

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeSyntax(SgSymbol *name, SgObject proc);
SG_EXTERN SgObject Sg_MakeMacro(SgObject name, SgClosure *transformer,
				SgObject data, SgObject env,
				SgCodeBuilder *compiledCode);

SG_EXTERN SgObject Sg_UnwrapSyntax(SgObject form);

SG_CDECL_END

#endif /* SAGITTARIUS_MACRO_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
