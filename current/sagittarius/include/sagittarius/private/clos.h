/* clos.h                                                 -*- coding: utf-8; -*-
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
#ifndef SAGITTARIUS_PRIVATE_CLOS_H_
#define SAGITTARIUS_PRIVATE_CLOS_H_

#include "sagittariusdefs.h"
#include "thread.h"

struct SgInstanceRec
{
  SgByte   *tag;
  SgObject *slots;
};
#define SG_INSTANCE(obj) ((SgInstance*)(obj))

#define SG_INSTANCE_HEADER SgInstance hdr

/* accessor */
SG_CLASS_DECL(Sg_SlotAccessorClass)
#define SG_CLASS_SLOT_ACCESSOR (&Sg_SlotAccessorClass)
typedef struct SgSlotAccessorRec SgSlotAccessor;
typedef SgObject (*SgSlotGetterProc)(SgObject);
typedef void     (*SgSlotSetterProc)(SgObject, SgObject);
struct SgSlotAccessorRec
{
  SG_HEADER;
  int index;			/* index */
  const char *cname;		/* for static initilization */
  SgObject name;		/* field name */
  SgClass *klass;		/* slot class */
  SgSlotGetterProc getter;	/* C getter */
  SgSlotSetterProc setter;	/* C setter */
  /* No bound? for c proc */
  SgObject getterS;
  SgObject setterS;
  SgObject boundP;
  SgObject definition;		/* slot definition for book keeping */
};
#define SG_SLOT_ACCESSOR(obj)  ((SgSlotAccessor*)(obj))
#define SG_SLOT_ACCESSORP(obj) SG_XTYPEP(obj, SG_CLASS_SLOT_ACCESSOR)

#define SG_CLASS_SLOT_SPEC(name, index, getter, setter)		\
  { { SG_CLASS2TAG(SG_CLASS_SLOT_ACCESSOR) },			\
      (index), (name), SG_FALSE, NULL,				\
	(SgSlotGetterProc)(getter),				\
	(SgSlotSetterProc)(setter),				\
	SG_FALSE, SG_FALSE, SG_FALSE, SG_NIL }


/* based on tiny clos. most of tricks are from Gauche */
/* define cprocs */
typedef void (*SgClassPrintProc)(SgObject obj, SgPort *port,
				 SgWriteContext *mode);
typedef int  (*SgClassCompareProc)(SgObject x, SgObject y, int equalP);
/* for external representation */
typedef int  (*SgClassSerializeProc)(SgObject obj, SgPort *port,
				     SgObject context);
typedef SgObject (*SgClassAllocateProc)(SgClass *klass, SgObject initargs);
/* For future use, define read/write own object cache */
typedef SgObject (*SgReadCacheProc)(SgPort *port, void *ctx);
typedef SgObject (*SgWriteCacheScanProc)(SgObject obj, SgObject cbs, void *ctx);
typedef void     (*SgWriteCacheProc)(SgObject obj, SgPort *port, void *ctx);

struct SgClassRec
{
  union {
    SG_INSTANCE_HEADER;
    double align_dummy;
  } classHdr;

  SgClassPrintProc     printer;
  SgClassCompareProc   compare;
  SgClassSerializeProc serialize;
  SgClassAllocateProc  allocate;
  /* cache procs */
  SgReadCacheProc      cacheReader;
  SgWriteCacheScanProc cacheScanner;
  SgWriteCacheProc     cacheWriter;

  SgClass **cpa;
  int       nfields;		/* need this? */
  int       coreSize;
  int       flags;

  /* scheme info */
  SgObject name;		/* class name (scheme) */
  SgObject directSupers;	/* list of classes */
  SgObject cpl;			/* list of classes */
  SgObject directSlots;		/* alist of name and definition */
  SgObject slots;		/* alist of name and definition */
  SgObject directSubclasses;	/* list of subclasses */
  SgSlotAccessor **gettersNSetters; /* array of accessors, NULL terminated */

  /* scheme cache */
  SgObject creader;
  SgObject cscanner;
  SgObject cwriter;
  SgObject redefined;		/* for change-class */
  SgObject library;		/* defined library */
  SgObject initargs;		/* for book keeping */
  /* For R6R integration */
  SgObject rtd;
  SgObject rcd;
  /* mutex */
  SgInternalMutex mutex;
  SgInternalCond  cv;
};

#define SG_ISA(obj, clazz) (SG_XTYPEP(obj, clazz)||Sg_TypeP(SG_OBJ(obj), clazz))

#define SG_CLASS(obj)  ((SgClass*)(obj))
#define SG_CLASSP(obj) SG_ISA(obj, SG_CLASS_CLASS)

#define SG_CLASS_FLAGS(obj)    (SG_CLASS(obj)->flags)
#define SG_CLASS_CATEGORY(obj) (SG_CLASS_FLAGS(obj) & 3)

#define SG_CLASS_APPLICABLE_P(obj) (SG_CLASS_FLAGS(obj)&SG_CLASS_APPLICABLE)

#define SG_ALLOCATE(klassname, klass)		\
  ((klassname*)Sg_AllocateInstance(klass))

enum {
  SG_CLASS_BUILTIN  = 0,
  SG_CLASS_ABSTRACT = 1,
  SG_CLASS_BASE     = 2,
  SG_CLASS_SCHEME   = 3,
  /* A special flag that only be used for "native applicable" objects,
     which basically inherits SgProcedure. */
  SG_CLASS_APPLICABLE = 0x04,
};

/* built-in classes */
SG_CLASS_DECL(Sg_TopClass);
SG_CLASS_DECL(Sg_BoolClass);
SG_CLASS_DECL(Sg_CharClass);
SG_CLASS_DECL(Sg_ClassClass);
SG_CLASS_DECL(Sg_EOFObjectClass);
SG_CLASS_DECL(Sg_UndefinedClass);
SG_CLASS_DECL(Sg_UnknownClass);
SG_CLASS_DECL(Sg_ObjectClass);	/* base of Scheme-defined objects */

#define SG_CLASS_TOP   	    	  (&Sg_TopClass)
#define SG_CLASS_BOOL  	    	  (&Sg_BoolClass)
#define SG_CLASS_CHAR  	    	  (&Sg_CharClass)
#define SG_CLASS_CLASS 	    	  (&Sg_ClassClass)
#define SG_CLASS_EOF_OBJECT 	  (&Sg_EOFObjectClass)
#define SG_CLASS_UNDEFINED_OBJECT (&Sg_UndefinedClass)
#define SG_CLASS_UNKNOWN          (&Sg_UnknownClass)
#define SG_CLASS_OBJECT           (&Sg_ObjectClass)

extern SgClass *Sg_DefaultCPL[];
extern SgClass *Sg_ObjectCPL[];

#define SG_CLASS_DEFAULT_CPL   (Sg_DefaultCPL)
#define SG_CLASS_OBJECT_CPL    (Sg_ObjectCPL)

#define SG_DEFINE_CLASS_FULL(cname, coreSize, flag, reader, scanner, writer, printer, compare, serialize, allocate, cpa) \
  SgClass CLASS_KEYWORD cname = {					\
    {{ SG_CLASS_STATIC_TAG(Sg_ClassClass), NULL }},			\
    printer,								\
    compare,								\
    serialize,								\
    allocate,								\
    (SgReadCacheProc     )reader,					\
    (SgWriteCacheScanProc)scanner,					\
    (SgWriteCacheProc    )writer,					\
    cpa,								\
    0,				/* nfields */				\
    coreSize,			/* coreSize */				\
    flag,			/* flag */				\
    SG_FALSE,			/* name */				\
    SG_NIL,			/* directSupers */			\
    SG_NIL,			/* cpl */				\
    SG_NIL,			/* directSlots */			\
    SG_NIL,			/* slots */				\
    SG_NIL,			/* fieldInitializers */			\
    NULL,			/* gettersNSetters */			\
    SG_FALSE,			/* creader */				\
    SG_FALSE,			/* cscanner */				\
    SG_FALSE, 			/* cwriter */				\
    SG_FALSE, 			/* redefined */				\
    SG_FALSE, 			/* library */				\
    SG_NIL,			/* initargs */				\
  }

#define SG_DEFINE_CLASS_COMMON(cname, coreSize, flag, printer, compare, serialize, allocate, cpa) \
  SG_DEFINE_CLASS_FULL(cname, coreSize, flag, NULL, NULL, NULL,		\
		       printer, compare, serialize, allocate, cpa)

#define SG_DEFINE_BUILTIN_CLASS_WITH_CACHE(cname, reader, cacher, writer, printer, compare, serialize, allocate, cpa) \
  SG_DEFINE_CLASS_FULL(cname, 0, SG_CLASS_BUILTIN, reader, cacher, writer, \
		       printer, compare, serialize, allocate, cpa)
#define SG_DEFINE_BUILTIN_CLASS_SIMPLE_WITH_CACHE(cname, reader, cacher, writer, printer) \
  SG_DEFINE_CLASS_FULL(cname, 0, SG_CLASS_BUILTIN, reader, cacher, writer, \
		       printer, NULL, NULL, NULL, NULL)

#define SG_DEFINE_BUILTIN_CLASS(cname, printer, compare, serialize, allocate, cpa) \
  SG_DEFINE_CLASS_COMMON(cname, 0, SG_CLASS_BUILTIN,			\
			 printer, compare, serialize, allocate, cpa)
#define SG_DEFINE_BUILTIN_CLASS_SIMPLE(cname, printer)			\
  SG_DEFINE_CLASS_COMMON(cname, 0, SG_CLASS_BUILTIN,			\
			 printer, NULL, NULL, NULL, NULL)

#define SG_DEFINE_ABSTRACT_CLASS(cname, cpa)				\
  SG_DEFINE_CLASS_COMMON(cname, 0, SG_CLASS_ABSTRACT,			\
			 NULL, NULL, NULL, NULL, cpa)

#define SG_DEFINE_BASE_CLASS(cname, ctype, printer, compare, serialize, allocate, cpa) \
  SG_DEFINE_CLASS_COMMON(cname, sizeof(ctype), SG_CLASS_BASE,	\
			 printer, compare, serialize, allocate, cpa)

SG_CDECL_BEGIN

/* for Scheme world */
SG_EXTERN SgObject Sg_VMSlotRef(SgObject obj, SgObject name);
SG_EXTERN SgObject Sg_VMSlotSet(SgObject obj, SgObject name, SgObject value);
SG_EXTERN SgObject Sg_VMSlotBoundP(SgObject obj, SgObject name);

SG_EXTERN SgObject Sg_SlotRefUsingAccessor(SgObject obj, SgSlotAccessor *ac);
SG_EXTERN int      Sg_SlotBoundUsingAccessor(SgObject obj, SgSlotAccessor *ac);
SG_EXTERN void     Sg_SlotSetUsingAccessor(SgObject obj, SgSlotAccessor *ac,
					   SgObject value);
SG_EXTERN SgObject Sg_SlotRefUsingClass(SgClass *klass, SgObject obj,
					  SgObject name);
SG_EXTERN void     Sg_SlotSetUsingClass(SgClass *klass, SgObject obj,
					  SgObject name, SgObject value);
SG_EXTERN int      Sg_SlotBoundUsingClass(SgClass *klass, SgObject obj,
					  SgObject name);
/* MOP looks like APIs */
SG_EXTERN SgClass* Sg_ClassOf(SgObject obj);
SG_EXTERN SgObject Sg_VMClassOf(SgObject obj);
SG_EXTERN SgObject Sg_VMIsA(SgObject obj, SgClass *klass);
/* type check */
SG_EXTERN int      Sg_TypeP(SgObject obj, SgClass *type);
SG_EXTERN int      Sg_SubtypeP(SgClass *sub, SgClass *type);

/* To mimic with-world-lock and wrapper class in PCL */
SG_EXTERN void     Sg_StartClassRedefinition(SgClass *klass);
SG_EXTERN void     Sg_EndClassRedefinition(SgClass *klass, SgObject newklass);
SG_EXTERN void     Sg_ReplaceClassBinding(SgClass *oldklass, SgClass *newklass);

/* just access to SgClass */
#define SG_CLASS_DIRECT_SUPERS(klass)  (SG_CLASS(klass)->directSupers)
#define SG_CLASS_DIRECT_SLOTS(klass)   (SG_CLASS(klass)->directSlots)
#define SG_CLASS_CPL(klass)            (SG_CLASS(klass)->cpl)
/* for C use */
#define SG_CLASS_CPA(klass)            (SG_CLASS(klass)->cpa)
#define SG_CLASS_SLOTS(klass)          (SG_CLASS(klass)->slots)

/* intercessory protocol */
SG_EXTERN SgObject Sg_AllocateInstance(SgClass *klass);
SG_EXTERN SgObject Sg_ComputeCPL(SgClass *klass);
SG_EXTERN SgObject Sg_ComputeSlots(SgClass *klass);
SG_EXTERN SgObject Sg_ComputeGetterAndSetter(SgClass *klass, SgObject slot);
SG_EXTERN SgObject Sg_MakeSlotAccessor(SgClass *klass, SgObject slot,
				       int index, 
				       SgObject getter, SgObject setter,
				       SgObject boundP);

SG_EXTERN int      Sg_ApplicableP(SgObject spec, SgObject args);

/* builtin class <object> */
SG_EXTERN SgObject Sg_ObjectAllocate(SgClass *klass, SgObject initargs);

/* MOP util */
SG_EXTERN void     Sg_AddDirectSubclass(SgClass *super, SgClass *sub);
SG_EXTERN void     Sg_RemoveDirectSubclass(SgClass *super, SgClass *sub);

/* internal for C. */
SG_EXTERN void     Sg_InitStaticClass(SgClass *klass, const SgChar *name,
				      SgLibrary *lib, SgSlotAccessor *specs,
				      int flags);
SG_EXTERN void     Sg_InitStaticClassWithMeta(SgClass *klass,
					      const SgChar *name,
					      SgLibrary *lib, SgClass *meta,
					      SgObject supers,
					      SgSlotAccessor *specs, int flags);

SG_EXTERN SgObject Sg_VMSlotInitializeUsingAccessor(SgObject obj, SgObject acc,
						    SgObject initargs);
SG_EXTERN SgObject Sg_VMSlotRefUsingSlotDefinition(SgObject obj, 
						   SgObject slot);
SG_EXTERN SgObject Sg_VMSlotSetUsingSlotDefinition(SgObject obj, 
						   SgObject slot,
						   SgObject value);
SG_EXTERN SgObject Sg_VMSlotBoundUsingSlotDefinition(SgObject obj, 
						     SgObject slot);

SG_EXTERN SgClass* Sg_BaseClassOf(SgClass *klass);
				      
SG_EXTERN void     Sg_SwapClassAndSlots(SgObject newInstance, 
					SgObject oldInstance);

/* compare */
SG_EXTERN int      Sg_ObjectCompare(SgObject x, SgObject y);

SG_CDECL_END

#endif /* SAGITTARIUS_CLOS_H_ */
