/* cache.h                                         -*- mode:c; coding:utf-8; -*-
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
#ifndef SAGITTARIUS_PRIVATE_CACHE_H_
#define SAGITTARIUS_PRIVATE_CACHE_H_

#include "sagittariusdefs.h"
#ifdef HAVE_SETJMP_H
# include <setjmp.h>
#else
# error TODO implement own set jmp
#endif

/* read cache state */
enum {
  CACHE_READ,
  RE_CACHE_NEEDED,
  INVALID_CACHE,
};

SG_CLASS_DECL(Sg_WriteCacheCtxClass);
SG_CLASS_DECL(Sg_ReadCacheCtxClass);
#define SG_CLASS_WRITE_CACHE_CTX (&Sg_WriteCacheCtxClass)
#define SG_CLASS_READ_CACHE_CTX (&Sg_ReadCacheCtxClass)

/* we need to show this for write cache */
struct cache_ctx_rec
{
  SG_HEADER;
  SgObject     name;
  SgHashTable *sharedObjects;
  int          uid;
  /* for pass1 */
  jmp_buf      escape;
  int          index;		/* code builder index */
  int          macroPhaseP;	/* avoid to emit local macros */
  SgObject     closures;	/* for calling writer external */
};
typedef struct cache_ctx_rec SgWriteCacheCtx;

#define SG_WRITE_CACHE_CTX(obj)   ((SgWriteCacheCtx *)obj)
#define SG_WRITE_CACHE_CTX_P(obj) SG_XTYPEP(obj, SG_CLASS_WRITE_CACHE_CTX)

/* for reading cache */

struct read_ctx_rec
{
  SG_HEADER;
  SgHashTable *sharedObjects;
  SgHashTable *seen;
  int isLinkNeeded:1;
  int insnP:1;			/* for temporary flag */
  int deprecatedP:1;		/* the content of cache is deprecated */
  SgString    *file;
  SgObject     links;		/* list of object will be linked */
  jmp_buf      escape;
};
typedef struct read_ctx_rec SgReadCacheCtx;

#define SG_READ_CACHE_CTX(obj)   ((SgReadCacheCtx *)obj)
#define SG_READ_CACHE_CTX_P(obj) SG_XTYPEP(obj, SG_CLASS_READ_CACHE_CTX)


SG_CDECL_BEGIN

SG_EXTERN int  Sg_WriteCache(SgObject name, SgString *id, SgObject cache);
SG_EXTERN int  Sg_ReadCache(SgString *id);
SG_EXTERN int  Sg_ReadCacheFromPort(SgPort *port);
SG_EXTERN int  Sg_ReadCacheFromImage(uint8_t *image, size_t len);
SG_EXTERN void Sg_CleanCache(SgObject target);

/* cache helper */
SG_EXTERN SgObject Sg_WriteCacheScanRec(SgObject obj, SgObject cbs,
					SgWriteCacheCtx *ctx);
SG_EXTERN void Sg_WriteObjectCache(SgObject o, SgPort *out,
				   SgWriteCacheCtx *ctx);
SG_EXTERN SgObject Sg_ReadCacheObject(SgPort *p, SgReadCacheCtx *ctx);

/* for compiler */
SG_EXTERN int  Sg_CachableP(SgObject o);

/* target file to cache path */
SG_EXTERN SgObject Sg_FileToCacheFile(SgString *o);

SG_CDECL_END

#endif /* SAGITTARIUS_CACHE_H_ */
