/* weak.h                                          -*- mode:c; coding:utf-8; -*-
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
#ifndef SAGITTARIUS_PRIVATE_WEAK_H_
#define SAGITTARIUS_PRIVATE_WEAK_H_

#include "sagittariusdefs.h"
#include "clos.h"

SG_CLASS_DECL(Sg_WeakVectorClass);
SG_CLASS_DECL(Sg_WeakHashTableClass);
#define SG_CLASS_WEAK_VECTOR    (&Sg_WeakVectorClass)
#define SG_CLASS_WEAK_HASHTABLE (&Sg_WeakHashTableClass)
typedef struct SgWeakVectorRec
{
  SG_HEADER;
  long  size;
  void *pointers;		/* opaque */
} SgWeakVector;

#define SG_WEAK_VECTOR(obj)  ((SgWeakVector*)(obj))
#define SG_WEAK_VECTORP(obj) SG_XTYPEP(obj,SG_CLASS_WEAK_VECTOR)

/* weak box for weak hashtable */
SG_CLASS_DECL(Sg_WeakBoxClass);
#define SG_CLASS_WEAK_BOX (&Sg_WeakBoxClass)
#define SG_WEAK_BOX(obj)  ((SgWeakBox*)(obj))
#define SG_WEAK_BOXP(obj) SG_XTYPEP(obj, SG_CLASS_WEAK_BOX)
struct SgWeakBoxRec
{
  SG_HEADER;
  void *ptr;
  int   registered;
};
typedef struct SgWeakBoxRec SgWeakBox;

#include "hashtable.h"

/* This should not be part of enum. remove entry when the value is GCed */
#define  SG_WEAK_REMOVE (1L<<2)
typedef enum {
  SG_WEAK_KEY   = (1L<<0),
  SG_WEAK_VALUE = (1L<<1),
  SG_WEAK_BOTH  = (SG_WEAK_KEY | SG_WEAK_VALUE),
  /* if key is removed then entry should be removed as well */
  /* SG_WEAK_REMOVE_KEY = (SG_WEAK_REMOVE | SG_WEAK_KEY), */
  SG_WEAK_REMOVE_VALUE = (SG_WEAK_REMOVE | SG_WEAK_VALUE),
  SG_WEAK_REMOVE_BOTH  = (SG_WEAK_REMOVE | SG_WEAK_KEY | SG_WEAK_VALUE),
} SgWeakness;

typedef struct SgWeakHashTableRec
{
  SgHashTable parent;		/* sub class of hashtable */
  SgWeakness  weakness;
  SgObject    defaultValue;
  SgHashProc        *hasher;
  SgHashCompareProc *compare;
  unsigned int goneEntries;
} SgWeakHashTable;

typedef SgHashIter SgWeakHashIter;

#define SG_WEAK_HASHTABLE(obj)      ((SgWeakHashTable*)obj)
#define SG_WEAK_HASHTABLE_P(obj)    SG_XTYPEP(obj, SG_CLASS_WEAK_HASHTABLE)
#define SG_WEAK_HASHTABLE_CORE(obj) (&SG_HASHTABLE(obj)->core)
#define SG_WEAK_HASHTABLE_TYPE(obj) (SG_HASHTABLE(obj)->type)
#define SG_WEAK_HASHTABLE_WEAKNESS(obj) (SG_WEAK_HASHTABLE(obj)->weakness)
#define SG_WEAK_HASHTABLE_DEFAULT_VALUE(obj) \
  (SG_WEAK_HASHTABLE(obj)->defaultValue)

SG_CDECL_BEGIN

/* weak vector */
SG_EXTERN SgObject Sg_MakeWeakVector(long size);
SG_EXTERN SgObject Sg_WeakVectorRef(SgWeakVector *v,
				    long index, SgObject fallback);
SG_EXTERN SgObject Sg_WeakVectorSet(SgWeakVector *v,
				    long index, SgObject value);

/* weak box */
SG_EXTERN SgWeakBox* Sg_MakeWeakBox(void *value);
SG_EXTERN int        Sg_WeakBoxEmptyP(SgWeakBox *wbox);
SG_EXTERN void       Sg_WeakBoxSet(SgWeakBox *wbox, void *value);
SG_EXTERN void*      Sg_WeakBoxRef(SgWeakBox *wbox);

/* weak hash */
SG_EXTERN SgObject Sg_MakeWeakHashTableSimple(SgHashType type,
					      SgWeakness weakness,
					      long initSize,
					      SgObject defaultValue);
SG_EXTERN SgObject Sg_MakeWeakHashTable(SgObject hasher,
					SgObject compare,
					SgWeakness weakness,
					long initSize,
					SgObject defaultValue);
SG_EXTERN SgObject Sg_WeakHashTableCopy(SgWeakHashTable *table);
SG_EXTERN SgObject Sg_WeakHashTableRef(SgWeakHashTable *table,
				       SgObject key, SgObject fallback);
SG_EXTERN SgObject Sg_WeakHashTableSet(SgWeakHashTable *table,
				       SgObject key, SgObject value, int flag);
SG_EXTERN SgObject Sg_WeakHashTableDelete(SgWeakHashTable *table,
					  SgObject key);
SG_EXTERN SgObject Sg_WeakHashTableKeys(SgWeakHashTable *table);
SG_EXTERN SgObject Sg_WeakHashTableValues(SgWeakHashTable *table);

SG_EXTERN void     Sg_WeakHashIterInit(SgWeakHashIter *iter,
				       SgWeakHashTable *table);
SG_EXTERN int      Sg_WeakHashIterNext(SgWeakHashIter *iter,
				       SgObject *key, SgObject *value);
SG_EXTERN int      Sg_WeakHashTableShrink(SgWeakHashTable *table);

SG_CDECL_END

#endif /* SAGITTARIUS_WEAK_H_ */
