/* hashtable.h                                     -*- mode:c; coding:utf-8; -*-
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
#ifndef SAGITTARIUS_PRIVATE_HASHTABLE_H_
#define SAGITTARIUS_PRIVATE_HASHTABLE_H_

#include "sagittariusdefs.h"
#include "collection.h"
#include "clos.h"

typedef enum {
  SG_HASH_EQ,
  SG_HASH_EQV,
  SG_HASH_EQUAL,
  SG_HASH_STRING,
  SG_HASH_GENERAL,
} SgHashType;

typedef struct SgHashCoreRec SgHashCore;
typedef struct SgHashIterRec SgHashIter;

typedef unsigned long SgHashVal;
/* hasher */
typedef SgHashVal SgHashProc(const SgHashCore *hc, intptr_t key);
/* tester */
typedef int  SgHashCompareProc(const SgHashCore *hc, intptr_t key,
			       intptr_t entryKey);

typedef SgDictEntry SgHashEntry;
/* 
   TODO use comparator.
 */
struct SgHashCoreRec
{
  void  **buckets;
  long    bucketCount;
  long    entryCount;
  long    bucketsLog2Count;
  void              *access;
  SgHashProc        *hasher;
  SgHashCompareProc *compare;
  SgObject generalHasher;	/* for make-hashtable */
  SgObject generalCompare; 	/* ditto */
  void   *data; 
  /* how to create entry of this hash */
  void (*create_entry)(SgHashCore *, SgHashEntry *);
};

struct SgHashIterRec
{
  SgHashCore *core;
  long         bucket;
  void       *next;
  SgObject    table;  /* need for weak hashtable */
  /* Iterator itself should have next operation */
  SgHashEntry *(*iter_next)(SgHashIter *, 
			    SgObject *key /* out */, 
			    SgObject *val /* out */);
};

SG_CLASS_DECL(Sg_HashTableClass);
#define SG_CLASS_HASHTABLE (&Sg_HashTableClass)

/* 
   To make hashtable and weak-hashtable have the same interface.
*/
typedef struct SgHashOpTableRec
{
  /* entry ref: table entry flags*/
  SgObject (*ref)(SgObject, SgHashEntry *, int);
  /* entry set: table entry value flags */
  SgObject (*set)(SgObject, SgHashEntry *, SgObject, int);
  /* we can't use delete since it's keyword on C++... */
  /* table key */
  SgObject (*remove)(SgObject, SgObject);
  /* table mutableP */
  SgObject (*copy)(SgObject, int);
  /* we also need this */
  void     (*init_iter)(SgObject, SgHashIter *);
} SgHashOpTable;

struct SgHashTableRec
{
  SG_HEADER;
  char       immutablep;
  SgHashType type;
  SgHashCore core;
  SgHashOpTable *opTable;
};

#define SG_HASH_NO_OVERWRITE SG_DICT_NO_OVERWRITE
#define SG_HASH_NO_CREATE    SG_DICT_NO_CREATE

#define SG_HASHTABLE_P(obj)    SG_ISA(obj, SG_CLASS_HASHTABLE)
#define SG_HASHTABLE(obj)      ((SgHashTable*)(obj))
#define SG_HASHTABLE_CORE(obj) (&SG_HASHTABLE(obj)->core)
#define SG_HASH_ENTRY_KEY      SG_DICT_ENTRY_KEY
#define SG_HASH_ENTRY_VALUE    SG_DICT_ENTRY_VALUE
#define SG_HASH_ENTRY_SET_VALUE SG_DICT_ENTRY_SET_VALUE
#define SG_HASHTABLE_OPTABLE(obj) SG_HASHTABLE(obj)->opTable
#define SG_HASHTABLE_TYPE(obj)    SG_HASHTABLE(obj)->type

#define SG_HASHTABLE_ENTRY_REF(ht, e, f)	\
  SG_HASHTABLE_OPTABLE(ht)->ref(ht, e, f)
#define SG_HASHTABLE_ENTRY_SET(ht, e, v, f)	\
  SG_HASHTABLE_OPTABLE(ht)->set(ht, e, v, f)

#define SG_IMMUTABLE_HASHTABLE_P(obj)					\
  (SG_HASHTABLE_P(obj) && SG_HASHTABLE(obj)->immutablep)

/* hash core seaerch flags */
typedef enum {
  SG_HASH_NO_ERROR = (1L<<0),	/* don't raise an error on hashtable level */
} SgHashRefFlag;


SG_CDECL_BEGIN
/* hash core */
SG_EXTERN void Sg_HashCoreInitSimple(SgHashCore *core,
				     SgHashType type,
				     long initSize,
				     void *data);
SG_EXTERN void Sg_HashCoreInitGeneral(SgHashCore *core,
				      SgHashProc *hasher,
				      SgHashCompareProc *compare,
				      long initSize,
				      void *data);
SG_EXTERN int Sg_HashCoreTypeToProcs(SgHashType type, SgHashProc **hasher,
				     SgHashCompareProc **compare);
SG_EXTERN SgHashEntry* Sg_HashCoreSearch(SgHashCore *table, intptr_t key,
					 SgDictOp op, int flags);
/* core operation to copy hashtable structure */
SG_EXTERN void Sg_HashCoreCopy(SgHashTable *dst, const SgHashTable *src);
SG_EXTERN void Sg_HashCoreClear(SgHashCore *ht, long k);

/* iterator */
SG_EXTERN void Sg_HashIterInit(SgObject table, SgHashIter *itr);
/* this return entry itself for convenience */
SG_EXTERN SgHashEntry* Sg_HashIterNext(SgHashIter *itr,
				       SgObject *key /* out */,
				       SgObject *val /* out */);

/* hasher */
SG_EXTERN SgHashVal Sg_EqHash(SgObject obj, SgHashVal bound);
SG_EXTERN SgHashVal Sg_EqvHash(SgObject obj, SgHashVal bound);
SG_EXTERN SgHashVal Sg_EqualHash(SgObject obj, SgHashVal bound);
SG_EXTERN SgHashVal Sg_StringHash(SgString *str, SgHashVal bound);

SG_EXTERN SgObject Sg_MakeHashTableSimple(SgHashType type, long initSize);
SG_EXTERN SgObject Sg_InitHashTableSimple(SgHashTable *table, 
					  SgHashType type, long initSize);

SG_EXTERN SgObject Sg_MakeHashTable(SgObject hasher, 
				    SgObject compare, long initSize);
SG_EXTERN SgObject Sg_MakeHashTableWithComparator(SgObject comparator, 
						  long initSize);

SG_EXTERN SgObject Sg_HashTableCopy(SgHashTable *table, int mutableP);

SG_EXTERN SgObject Sg_HashTableRef(SgHashTable *table, SgObject key,
				   SgObject fallback);
SG_EXTERN SgObject Sg_HashTableSet(SgHashTable *table, SgObject key,
				   SgObject value, int flags);
SG_EXTERN SgObject Sg_HashTableDelete(SgHashTable *table, SgObject key);

SG_EXTERN SgObject Sg_HashTableAddAll(SgHashTable *dst, SgHashTable *src);
SG_EXTERN SgObject Sg_HashTableKeys(SgHashTable *table);
SG_EXTERN SgObject Sg_HashTableValues(SgHashTable *table);
/* status for hash table */
SG_EXTERN SgObject Sg_HashTableStat(SgHashTable *table);

SG_EXTERN long     Sg_HashTableSize(SgHashTable *table);

SG_CDECL_END

#endif /* SAGITTARIUS_HASHTABLE_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
