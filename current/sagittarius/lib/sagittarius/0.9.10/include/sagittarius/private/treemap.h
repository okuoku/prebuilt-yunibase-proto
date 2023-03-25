/* treemap.h                                              -*- coding: utf-8 -*-
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
#ifndef SAGITTARIUS_PRIVATE_TREEMAP_H_
#define SAGITTARIUS_PRIVATE_TREEMAP_H_

#include "sagittariusdefs.h"
#include "collection.h"

/* For C use compare function */
typedef SgDictEntry SgTreeEntry;
typedef int SgTreeCompareProc(SgTreeMap *, intptr_t, intptr_t);
typedef SgTreeEntry* SgTreeRefProc(SgTreeMap *, intptr_t);
typedef SgTreeEntry* SgTreeSearchProc(SgTreeMap *, intptr_t, SgDictOp);
typedef SgObject SgTreeCopyProc(const SgTreeMap *);

/* only for c */
typedef struct SgTreeIterRec SgTreeIter;
typedef SgTreeIter* SgTreeIterInitProc(SgTreeIter *, SgTreeMap *,
				       SgTreeEntry *, int ascP);

/*
  header:
  ........ ........ .....S.. ....0111 : S: Scheme
 */
struct SgTreeMapRec
{
  SG_HEADER;
  /* These are could be Scheme object or C pointer */
  intptr_t root;
  int      entryCount;
  int      schemep;
  union {
    struct {
      SgTreeCompareProc  *cmp;
      SgTreeSearchProc   *search;
      SgTreeCopyProc     *copy;
      SgTreeIterInitProc *iter;
      /* NavigationMap (optional)*/
      SgTreeRefProc      *higher;
      SgTreeRefProc      *lower;
    } c;
    /* for now we don't support this */
#if 0
    struct {
      SgObject cmp;
      SgObject search;
      SgObject copy;
      /* NavigationMap (optional)*/
      SgObject higher;
      SgObject lower;
    } scm;
#endif
  } procs;
  void *data;
};

SG_CLASS_DECL(Sg_TreeMapClass);
#define SG_CLASS_TREE_MAP  (&Sg_TreeMapClass)

#define SG_TREEMAP_PROC(__tc, __type, __proc)	\
  (SG_TREEMAP(__tc)->procs.__type.__proc)

#define SG_TREEMAP_C_PROC(__tc, __proc)		\
  SG_TREEMAP_PROC(__tc, c, __proc)

#define SG_TREEMAP_SCM_PROC(__tc, __proc)	\
  SG_TREEMAP_PROC(__tc, scm, __proc)

/* this is not Scheme object */
struct SgTreeIterRec
{
  SgTreeMap   *t;
  SgTreeEntry *e;	       /* current entry of this iterator */
  int          end;	       /* if this iterator is at end or not */
  /* for scalabilty */
  SgTreeEntry* (*next)(SgTreeIter *);
};

enum SgTreeFlags{
  SG_TREE_NO_OVERWRITE = (1L<<0), /* do not overwrite the existing entry */
  SG_TREE_NO_CREATE    = (1L<<1)  /* do not create new one if no match */
};

#define SG_TREEMAP(obj)     ((SgTreeMap*)obj)
#define SG_TREEMAP_P(obj)   SG_XTYPEP(obj, SG_CLASS_TREE_MAP)
#define SG_SCHEME_TREEMAP_P(obj)				\
  (SG_TREEMAP_P(obj) && SG_TREEMAP(obj)->schemep)

SG_CDECL_BEGIN

/* C APIs */
SG_EXTERN SgTreeEntry* Sg_TreeMapCoreSearch(SgTreeMap *tm, intptr_t key,
					    SgDictOp op, int flags);
SG_EXTERN SgObject Sg_MakeDefaultTreeMap(SgTreeCompareProc *cmp);
SG_EXTERN SgObject Sg_TreeMapCopy(const SgTreeMap *src);
SG_EXTERN SgObject Sg_TreeMapRef(SgTreeMap *tm, SgObject key,
				 SgObject fallback);
SG_EXTERN SgObject Sg_TreeMapSet(SgTreeMap *tm, SgObject key, SgObject value,
				 int flags);
SG_EXTERN SgObject Sg_TreeMapDelete(SgTreeMap *tm, SgObject key);
SG_EXTERN void     Sg_TreeMapClear(SgTreeMap *tm);

/* generic constructors */
SG_EXTERN SgObject Sg_MakeGenericCTreeMap(SgTreeCompareProc *cmp,
					  SgTreeSearchProc *search,
					  SgTreeCopyProc *copy,
					  SgTreeIterInitProc *iter,
					  SgTreeRefProc *higher,
					  SgTreeRefProc *lower,
					  void *data);
/* for now not supported */
/*
SG_EXTERN SgObject Sg_MakeGenericSchemeTreeMap(SgObject cmp,
					       SgObject search,
					       SgObject copy);
*/

/* iterator these APIs are only for C */
SG_EXTERN void         Sg_TreeIterInit(SgTreeIter *iter,
				       SgTreeMap *tm, SgTreeEntry *start);
SG_EXTERN void         Sg_TreeReverseIterInit(SgTreeIter *iter,
					      SgTreeMap *tm,
					      SgTreeEntry *start);
SG_EXTERN SgTreeEntry* Sg_TreeIterNext(SgTreeIter *iter);
SG_EXTERN int          Sg_TreeIterHasNext(SgTreeIter *iter);
SG_EXTERN SgObject     Sg_TreeMapEntries(SgTreeMap *tm);
SG_EXTERN SgObject     Sg_TreeMapKeys(SgTreeMap *tm);
SG_EXTERN SgObject     Sg_TreeMapValues(SgTreeMap *tm);

SG_EXTERN SgTreeEntry* Sg_TreeMapHigherEntry(SgTreeMap *tm, SgObject key);
SG_EXTERN SgTreeEntry* Sg_TreeMapLowerEntry(SgTreeMap *tm, SgObject key);
SG_EXTERN SgTreeEntry* Sg_TreeMapFirstEntry(SgTreeMap *tm);
SG_EXTERN SgTreeEntry* Sg_TreeMapLastEntry(SgTreeMap *tm);

/* Supported implementation tree constructors */
SG_EXTERN SgObject Sg_MakeRBTreeMap(SgTreeCompareProc *cmp);
SG_EXTERN SgObject Sg_MakeSchemeRBTreeMap(SgObject cmp);

/* common procedures(only C) */
SG_EXTERN int      Sg_TreeMapEq(SgTreeMap *a, SgTreeMap *b);

SG_CDECL_END

#endif /* SAGITTARIUS_TREEMAP_HPP_ */
