/*=====================================================================*/
/*    .../project/bigloo/bigloo/runtime/Include/bigloo_object.h        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Mar  5 08:05:01 2016                          */
/*    Last change :  Sun Aug 21 08:32:34 2022 (serrano)                */
/*    Copyright   :  2016-22 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo OBJECTs                                                   */
/*=====================================================================*/
#ifndef BIGLOO_OBJECT_H 
#define BIGLOO_OBJECT_H

/*---------------------------------------------------------------------*/
/*    Does someone really wants C++ here?                              */
/*---------------------------------------------------------------------*/
#ifdef __cplusplus
extern "C" {
#endif
#ifdef __cplusplus_just_for_emacs_indent
}
#endif

/*---------------------------------------------------------------------*/
/*    types                                                            */
/*---------------------------------------------------------------------*/
/* old name mangling framework */
typedef struct __object_bgl {
   header_t header;
   obj_t widening;
} *object_bglt;

/* bootstrap configuration */   
typedef struct __bgl__object_00_bgl {
   header_t header;
   obj_t widening;
} *bgl__object_00_bglt;
   
/* new name mangling framework */   
typedef struct BgL__object_00_bgl {
   header_t header;
   obj_t widening;
} *BgL__object_00_bglt;
   
typedef struct BgL_objectz00_bgl {
   header_t header;
   obj_t widening;
} *BgL_objectz00_bglt;

#if (defined(BGL_TAG_CNST32)) /* BGL_TAG_CNST32 */
#  define BGL_OBJECTP(o) \
    (BGL_TAG_CNSTP(o) && (((unsigned long)o) < ((unsigned long)0xff << 24)))
#  define BOBJECT(o) ((obj_t)((unsigned long)o + TAG_CNST))
#  define COBJECT(o) ((obj_t)((unsigned long)o - TAG_CNST))
#elif (defined(TAG_OBJECT)) /* TAG_OBJECT */
#  if (TAG_OBJECT != 0)
#    define BGL_OBJECTP(o) \
      ((((long)o) & TAG_MASKOBJECT) == TAG_OBJECT)
#  else
#    define BGL_OBJECTP(o) \
      ((o) && (((long)o) & TAG_MASKOBJECT) == TAG_OBJECT)
#  endif
#  define BOBJECT(o) ((obj_t)((unsigned long)o + TAG_OBJECT))
#  define COBJECT(o) ((obj_t)((unsigned long)o - TAG_OBJECT))
#else /* !BGL_TAG_CNST32 && !TAG_OBJECT */
#  define BGL_OBJECTP(o) \
    ((POINTERP(o) && (TYPE(o) >= OBJECT_TYPE)))
#  define BOBJECT(o) BREF(o)
#  define COBJECT(o) CREF(o)
#endif

#if (defined(TAG_NANOBJECT)) /* TAG_NANOBJECT */
#  if (TAG_NANOBJECT != 0)
#    define BGL_NANOBJECTP(o) ((((long)o) & TAG_MASK) == TAG_NANOBJECT)
#  else
#    define BGL_NANOBJECTP(o) ((o) && (((long)o) & TAG_MASK) == TAG_NANOBJECT)
#  endif
#  define BNANOBJECT(o) ((obj_t)((unsigned long)o + TAG_NANOBJECT))
#  undef COBJECT
#  define COBJECT(o) ((obj_t)((unsigned long)o & ~(TAG_MASK)))
#else /* !TAG_NANOBJECT */
#  define BGL_NANOBJECTP(o) (0)
#  define BNANOBJECT(o) BOBJECT(o)
#endif

#if (defined(TAG_RESERVED))
#  define BRESERVEDOBJECT(o) BRESERVED(o)
#else
#  define BRESERVEDOBJECT(o) BOBJECT(o)
#endif

/*---------------------------------------------------------------------*/
/*    Object macros                                                    */
/*---------------------------------------------------------------------*/
#define BGL_MAX_CLASS_NUM() \
   ((1L << HEADER_TYPE_BIT_SIZE) - 1)

#define BGL_HEADER_OBJECT_TYPE_SIZE 20
#define BGL_HEADER_OBJECT_TYPE_MASK ((1 << BGL_HEADER_OBJECT_TYPE_SIZE) - 1)

#define BGL_OBJECT_CLASS_NUM(_obj) \
     (HEADER_TYPE(COBJECT(_obj)->header))

#define BGL_OBJECT_CLASS_NUM_SET(_1, _2) \
   (((obj_t)COBJECT(_1))->header = MAKE_HEADER(_2, 0), BUNSPEC)
   
#define BGL_OBJECT_WIDENING(_obj) \
   (((object_bglt)(COBJECT(_obj)))->widening)

#define BGL_OBJECT_WIDENING_SET(_obj, _wdn) \
   BASSIGN(BGL_OBJECT_WIDENING(_obj), _wdn, (obj_t)_obj)

#define BGL_OBJECT_HEADER_SIZE(_obj) \
   (HEADER_SIZE(COBJECT(_obj)->header))

#define BGL_OBJECT_HEADER_SIZE_SET(_o, _s) \
   (((obj_t)COBJECT(_o))->header = \
    MAKE_HEADER(HEADER_SANS_SIZE(COBJECT(_o)->header), _s))

#define BGL_OBJECT_HEADER_AAASIZE(_obj) \
   (HEADER_AAASIZE(COBJECT(_obj)->header))

#define BGL_OBJECT_HEADER_AAASIZE_SET(_o, _s, _a) \
   (((obj_t)COBJECT(_o))->header = \
    BGL_MAKE_HEADER_AAA(HEADER_SANS_SIZE(COBJECT(_o)->header), _s, _a))

#if (PTR_ALIGNMENT >= 3 && !BGL_NAN_TAGGING)
#  define BGL_OBJECT_INHERITANCE_NUM(_obj) \
     (HEADER_DATA(COBJECT(_obj)->header))

#  define BGL_OBJECT_INHERITANCE_NUM_SET(_o, _d) \
   (((obj_t)COBJECT(_o))->header = \
    ((((obj_t)COBJECT(_o))->header \
      & ((1 << (HEADER_SHIFT + HEADER_SIZE_BIT_SIZE + HEADER_TYPE_BIT_SIZE)) - 1)) \
     | ((_d << (TYPE_SHIFT + HEADER_TYPE_BIT_SIZE)))))
#else
#  define BGL_OBJECT_INHERITANCE_NUM(_obj) 0
#endif

/*---------------------------------------------------------------------*/
/*    BGL_ISA                                                          */
/*---------------------------------------------------------------------*/
#if (PTR_ALIGNMENT >= 3 && !BGL_NAN_TAGGING)
#  define BGL_CONDEXPAND_ISA_ARCH64() 1
#else
#  define BGL_CONDEXPAND_ISA_ARCH64() 0
#endif

/*---------------------------------------------------------------------*/
/*    Classes                                                          */
/*---------------------------------------------------------------------*/
#define BGL_CLASSP(o) (POINTERP(o) && (TYPE(o) == CLASS_TYPE))

#define BGL_CLASS_SIZE (sizeof(struct bgl_class))
#define BGL_CLASS(f) (CREF(f)->class)
   
#define BGL_CLASS_NAME(f) (BGL_CLASS(f).name)
   
#define BGL_CLASS_INDEX(f) (BGL_CLASS(f).index)
   
#define BGL_CLASS_NUM(f) ((BGL_CLASS(f).index) + BGL_CLASS(f).inheritance_index)
   
#define BGL_CLASS_INHERITANCE_INDEX(f) (BGL_CLASS(f).inheritance_index)
   
#define BGL_CLASS_DEPTH(f) (BGL_CLASS(f).depth)
   
#define BGL_CLASS_SUPER(f) (BGL_CLASS(f).super)
#define BGL_CLASS_ANCESTORS(f) (BGL_CLASS(f).ancestors)
#define BGL_CLASS_ANCESTORS_REF(f, i) (&(BGL_CLASS(f).ancestor0))[ i ]
   
#define BGL_CLASS_SUBCLASSES(f) (BGL_CLASS(f).subclasses)
#define BGL_CLASS_SUBCLASSES_SET(f, v) BASSIGN(BGL_CLASS_SUBCLASSES(f), v, f)
   
#define BGL_CLASS_DIRECT_FIELDS(f) (BGL_CLASS(f).direct_fields)
#define BGL_CLASS_DIRECT_FIELDS_SET(f, v) BASSIGN(BGL_CLASS_DIRECT_FIELDS(f), v, f)
   
#define BGL_CLASS_ALL_FIELDS(f) (BGL_CLASS(f).all_fields)
#define BGL_CLASS_ALL_FIELDS_SET(f, v) BASSIGN(BGL_CLASS_ALL_FIELDS(f), v, f)
   
#define BGL_CLASS_VIRTUAL_FIELDS(f) (BGL_CLASS(f).virtual_fields)
   
#define BGL_CLASS_MODULE(f) (BGL_CLASS(f).module)
   
#define BGL_CLASS_ALLOC_FUN(f) (BGL_CLASS(f).alloc_fun)

#define BGL_CLASS_HASH(f) (BGL_CLASS(f).hash)
   
#define BGL_CLASS_NEW_FUN(f) (BGL_CLASS(f).new_fun)
   
#define BGL_CLASS_NIL_FUN(f) (BGL_CLASS(f).nil_fun)
   
#define BGL_CLASS_NIL(f) (BGL_CLASS(f).nil)
#define BGL_CLASS_NIL_SET(f, v) BASSIGN(BGL_CLASS_NIL(f), v, f)
   
#define BGL_CLASS_CONSTRUCTOR(f) (BGL_CLASS(f).constructor)
   
#define BGL_CLASS_SHRINK(f) (BGL_CLASS(f).shrink)
   
#define BGL_CLASS_EVDATA(f) (BGL_CLASS(f).evdata)   
#define BGL_CLASS_EVDATA_SET(f, o) (BGL_CLASS_EVDATA(f) = o)
   
/*---------------------------------------------------------------------*/
/*    C++                                                              */
/*---------------------------------------------------------------------*/
#ifdef __cplusplus
}
#endif
#endif
