/* string.h                                        -*- mode:c; coding:utf-8; -*-
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
#ifndef SAGITTARIUS_PRIVATE_STRING_H_
#define SAGITTARIUS_PRIVATE_STRING_H_

#include "sagittariusdefs.h"
#include "clos.h"
#include <sagittarius/strings.h>

SG_CLASS_DECL(Sg_StringClass);
#define SG_CLASS_STRING (&Sg_StringClass)

struct SgStringRec
{
  SG_HEADER;
  unsigned long immutablep: 1;
  long size             : (SIZEOF_LONG*CHAR_BIT-1);
  SgChar value[1];
};

/* construction flag */
typedef enum {
  SG_LITERAL_STRING,
  SG_HEAP_STRING,
  SG_IMMUTABLE_STRING,
} SgStringType;

typedef enum {
  SG_STRING_SCAN_INDEX,		/* return index */
  SG_STRING_SCAN_BEFORE,
  SG_STRING_SCAN_AFTER,
  SG_STRING_SCAN_BEFORE2,
  SG_STRING_SCAN_AFTER2,
  SG_STRING_SCAN_BOTH
} SgStringScanType;

#define READ_STRING_MAX_SIZE  2048
#define SG_STRINGP(obj)       SG_XTYPEP(obj, SG_CLASS_STRING)
#define SG_STRING(obj)         	((SgString*)(obj))
#define SG_IMMUTABLE_STRINGP(obj)			\
  (SG_STRINGP(obj) && SG_STRING(obj)->immutablep)

#define SG_STRING_SIZE(obj)     (SG_STRING(obj)->size)
#define SG_STRING_VALUE(obj)    (SG_STRING(obj)->value)
#define SG_STRING_VALUE_AT(obj, index)    (SG_STRING(obj)->value[index])

#define SG_MAKE_STRING(str)					\
  SG_STRING(Sg_MakeString(UC(str), SG_IMMUTABLE_STRING, -1))

#define Sg_String(str)					\
  SG_STRING(Sg_MakeString(str, SG_IMMUTABLE_STRING, -1))

#define Sg_HeapString(str)				\
  SG_STRING(Sg_MakeString(str, SG_HEAP_STRING, -1))


#define SG_STRING_ALLOC_SIZE(size) (sizeof(SgString)+sizeof(SgChar)*size)

#ifdef HAVE_ALLOCA
#define SG_ALLOC_TEMP_STRING(var, size)					\
  do {									\
    (var) = SG_STRING(alloca(SG_STRING_ALLOC_SIZE(size)));		\
    SG_SET_CLASS(var, SG_CLASS_STRING);					\
    SG_STRING_SIZE(var) = size;						\
  } while (0)
#else
#define SG_ALLOC_TEMP_STRING(var, size)		\
  do { (var) = Sg_ReserveString(size, 0); } while (0)
#endif

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeStringC(const char *value);
SG_EXTERN SgObject Sg_MakeString(const SgChar *value, SgStringType flag,
				 long length);

SG_EXTERN SgObject Sg_ReserveString(long size, SgChar fill);
/* this is for get-string-n related not for c use */
SG_EXTERN SgObject Sg_MakeEmptyString();

SG_EXTERN SgObject Sg_StringToList(SgString *s, long start, long end);
SG_EXTERN SgObject Sg_ListToString(SgObject obj, long start, long end);

/* compare */
SG_EXTERN int 	   Sg_StringEqual(SgString *s1, SgString *s2);
SG_EXTERN int 	   Sg_StringCompare(SgString *s1, SgString *s2);

/* concat */
SG_EXTERN SgObject Sg_StringAppend2(SgString *a, SgString *b);
SG_EXTERN SgObject Sg_StringAppendC(SgString *a, const SgChar *s, long size);
SG_EXTERN SgObject Sg_StringAppend(SgObject args);
SG_EXTERN SgObject Sg_CopyString(SgString *a);

/* search */
SG_EXTERN SgObject Sg_StringScan(SgString *s1, SgString *s2, int retmode);
SG_EXTERN SgObject Sg_StringScanChar(SgString *s1, SgChar ch, int retmode);
/* split */
SG_EXTERN SgObject Sg_StringSplitChar(SgString *s1, SgChar ch);

/* modify */
SG_EXTERN SgObject Sg_Substring(SgString *x, long start, long end);
SG_EXTERN void     Sg_StringFill(SgString *s, SgChar c, long start, long end);
/* for srfi-13 */
SG_EXTERN SgObject Sg_MaybeSubstring(SgString *s, long start, long end);

/* check if the string is literal (not immutable) string */
SG_EXTERN int      Sg_LiteralStringP(SgString *s);
/* converts given string to immutable string if it's not */
SG_EXTERN SgObject Sg_StringToIString(SgString *s, long start, long end);
/* mostly for cache */
SG_EXTERN SgObject Sg_StringIntern(SgString *s);

SG_CDECL_END

#endif /* STRING_SAGITTARIUS_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
