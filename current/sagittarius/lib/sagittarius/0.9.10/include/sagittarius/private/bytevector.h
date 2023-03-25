/* bytevector.h                                    -*- mode:c; coding:utf-8; -*-
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
#ifndef SAGITTARIUS_PRIVATE_BYTEVECTOR_H_
#define SAGITTARIUS_PRIVATE_BYTEVECTOR_H_

#include "sagittariusdefs.h"

SG_CLASS_DECL(Sg_ByteVectorClass);
#define SG_CLASS_BVECTOR (&Sg_ByteVectorClass)

struct SgByteVectorRec
{
  SG_HEADER;
  unsigned long literalp : 1;
  long size     : (SIZEOF_LONG*CHAR_BIT-1);
  uint8_t *elements;
};

#define SG_BVECTOR(obj)      ((SgByteVector*)obj)
#define SG_BVECTORP(obj)     SG_XTYPEP(obj, SG_CLASS_BVECTOR)
#define SG_BVECTOR_SIZE(obj)       (SG_BVECTOR(obj)->size)
#define SG_BVECTOR_ELEMENTS(obj)   (SG_BVECTOR(obj)->elements)
#define SG_BVECTOR_ELEMENT(obj, i) (SG_BVECTOR(obj)->elements[i])
#define SG_BVECTOR_LITERALP(obj)   (SG_BVECTOR(obj)->literalp)

#define SG_LITERAL_BVECTORP(obj)				\
  (SG_BVECTORP(obj) && SG_BVECTOR(obj)->literalp)
#define SG_BVECTOR_SET_LITERAL(obj)				\
  (SG_BVECTOR(obj)->literalp = TRUE)

/* utility macros */
#define SG_IS_BYTE(v)  (-128 <= v && v <= 127)
#define SG_IS_OCTET(v) (0 <= v && v <= 255)

#define SG_BVECTOR_IS_VALID_INDEX(bv, index)	\
  (0 <= index && index < SG_BVECTOR_SIZE(bv))

#ifdef HAVE_ALLOCA
#define SG_ALLOC_TEMP_BVECTOR(var, size)				\
  do {									\
    (var) = SG_BVECTOR(alloca(sizeof(SgByteVector)));			\
    SG_SET_CLASS(var, SG_CLASS_BVECTOR);				\
    SG_BVECTOR_SIZE(var) = size;					\
    SG_BVECTOR_LITERALP(var) = FALSE;					\
    SG_BVECTOR_ELEMENTS(var) = (uint8_t*)(alloca(size));		\
  } while (0)
#else
#define SG_ALLOC_TEMP_BVECTOR(var, size)		\
  do { (var) = Sg_MakeByteVector(size, 0); } while (0)
#endif

#define SG_STATIC_BYTEVECTOR(size_, elements_)				\
  { { SG_CLASS_STATIC_TAG(Sg_ByteVectorClass) }, TRUE, (size_), (elements_) }

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeByteVector(long size, int fill);

SG_EXTERN SgObject Sg_MakeByteVectorFromU8Array(const uint8_t *buf,
						long size);

SG_EXTERN SgObject Sg_NativeEndianness();
SG_EXTERN int      Sg_ByteVectorEqP(SgByteVector *bv1, SgByteVector *bv2);
SG_EXTERN SgObject Sg_ByteVectorCopy(SgByteVector *src, long start, long end);
SG_EXTERN void     Sg_ByteVectorCopyX(SgByteVector *src, long srcStart,
				      SgByteVector *dst, long dstStart,
				      long size);
SG_EXTERN void     Sg_ByteVectorFill(SgByteVector *bv, int value,
				     long start, long end);
SG_EXTERN SgObject Sg_ByteVectorReverseX(SgByteVector *bv,
					 long start, long end);

/* converter */
SG_EXTERN SgObject Sg_ListToByteVector(SgObject lst, int bitCount, int signP);
SG_EXTERN SgObject Sg_ByteVectorToList(SgByteVector *bv, int bitCount,
				       int signP);
SG_EXTERN SgObject Sg_ByteVectorToString(SgByteVector *bv,
					 SgTranscoder *transcoder,
					 long start, long end);
SG_EXTERN SgObject Sg_StringToByteVector(SgString *s,
					 SgTranscoder *transcoder,
					 long start, long end);
/* u/s8 accessor */
SG_EXTERN uint8_t  Sg_ByteVectorU8Ref(SgByteVector *bv, long index);
SG_EXTERN void     Sg_ByteVectorU8Set(SgByteVector *bv, long index,
				      uint8_t value);
SG_EXTERN int8_t   Sg_ByteVectorS8Ref(SgByteVector *bv, long index);
SG_EXTERN void     Sg_ByteVectorS8Set(SgByteVector *bv, long index,
				      int8_t value);
/* u/s16 accessor */
SG_EXTERN uint16_t Sg_ByteVectorU16NativeRef(SgByteVector *bv, long index);
SG_EXTERN uint16_t Sg_ByteVectorU16LittleRef(SgByteVector *bv, long index);
SG_EXTERN uint16_t Sg_ByteVectorU16BigRef(SgByteVector *bv, long index);
SG_EXTERN void     Sg_ByteVectorU16NativeSet(SgByteVector *bv, long index,
					     uint16_t value);
SG_EXTERN void     Sg_ByteVectorU16LittleSet(SgByteVector *bv, long index,
					     uint16_t value);
SG_EXTERN void     Sg_ByteVectorU16BigSet(SgByteVector *bv, long index,
					  uint16_t value);
SG_EXTERN int16_t  Sg_ByteVectorS16NativeRef(SgByteVector *bv, long index);
SG_EXTERN int16_t  Sg_ByteVectorS16LittleRef(SgByteVector *bv, long index);
SG_EXTERN int16_t  Sg_ByteVectorS16BigRef(SgByteVector *bv, long index);
SG_EXTERN void     Sg_ByteVectorS16NativeSet(SgByteVector *bv, long index,
					     int16_t value);
SG_EXTERN void     Sg_ByteVectorS16LittleSet(SgByteVector *bv, long index,
					     int16_t value);
SG_EXTERN void     Sg_ByteVectorS16BigSet(SgByteVector *bv, long index,
					  int16_t value);
/* u/s32 accessor */
SG_EXTERN uint32_t Sg_ByteVectorU32NativeRef(SgByteVector *bv, long index);
SG_EXTERN uint32_t Sg_ByteVectorU32LittleRef(SgByteVector *bv, long index);
SG_EXTERN uint32_t Sg_ByteVectorU32BigRef(SgByteVector *bv, long index);
SG_EXTERN void     Sg_ByteVectorU32NativeSet(SgByteVector *bv, long index,
					     uint32_t value);
SG_EXTERN void     Sg_ByteVectorU32LittleSet(SgByteVector *bv, long index,
					     uint32_t value);
SG_EXTERN void     Sg_ByteVectorU32BigSet(SgByteVector *bv, long index,
					  uint32_t value);
SG_EXTERN int32_t  Sg_ByteVectorS32NativeRef(SgByteVector *bv, long index);
SG_EXTERN int32_t  Sg_ByteVectorS32LittleRef(SgByteVector *bv, long index);
SG_EXTERN int32_t  Sg_ByteVectorS32BigRef(SgByteVector *bv, long index);
SG_EXTERN void     Sg_ByteVectorS32NativeSet(SgByteVector *bv, long index,
					     int32_t value);
SG_EXTERN void     Sg_ByteVectorS32LittleSet(SgByteVector *bv, long index,
					     int32_t value);
SG_EXTERN void     Sg_ByteVectorS32BigSet(SgByteVector *bv, long index,
					  int32_t value);
/* u/s64 accessor */
SG_EXTERN uint64_t Sg_ByteVectorU64NativeRef(SgByteVector *bv, long index);
SG_EXTERN uint64_t Sg_ByteVectorU64LittleRef(SgByteVector *bv, long index);
SG_EXTERN uint64_t Sg_ByteVectorU64BigRef(SgByteVector *bv, long index);
SG_EXTERN void     Sg_ByteVectorU64NativeSet(SgByteVector *bv, long index,
					     uint64_t value);
SG_EXTERN void     Sg_ByteVectorU64LittleSet(SgByteVector *bv, long index,
					     uint64_t value);
SG_EXTERN void     Sg_ByteVectorU64BigSet(SgByteVector *bv, long index,
					  uint64_t value);
SG_EXTERN int64_t  Sg_ByteVectorS64NativeRef(SgByteVector *bv, long index);
SG_EXTERN int64_t  Sg_ByteVectorS64LittleRef(SgByteVector *bv, long index);
SG_EXTERN int64_t  Sg_ByteVectorS64BigRef(SgByteVector *bv, long index);
SG_EXTERN void     Sg_ByteVectorS64NativeSet(SgByteVector *bv, long index,
					     int64_t value);
SG_EXTERN void     Sg_ByteVectorS64LittleSet(SgByteVector *bv, long index,
					     int64_t value);
SG_EXTERN void     Sg_ByteVectorS64BigSet(SgByteVector *bv, long index,
					  int64_t value);
/* float accessor */
SG_EXTERN float    Sg_ByteVectorIEEESingleNativeRef(SgByteVector *bv,
						    long index);
SG_EXTERN float    Sg_ByteVectorIEEESingleLittleRef(SgByteVector *bv,
						    long index);
SG_EXTERN float    Sg_ByteVectorIEEESingleBigRef(SgByteVector *bv,
						 long index);
SG_EXTERN void     Sg_ByteVectorIEEESingleNativeSet(SgByteVector *bv,
						    long index, float value);
SG_EXTERN void     Sg_ByteVectorIEEESingleLittleSet(SgByteVector *bv,
						    long index, float value);
SG_EXTERN void     Sg_ByteVectorIEEESingleBigSet(SgByteVector *bv,
						 long index, float value);
/* double accessor */
SG_EXTERN double    Sg_ByteVectorIEEEDoubleNativeRef(SgByteVector *bv,
						     long index);
SG_EXTERN double    Sg_ByteVectorIEEEDoubleLittleRef(SgByteVector *bv,
						     long index);
SG_EXTERN double    Sg_ByteVectorIEEEDoubleBigRef(SgByteVector *bv,
						  long index);
SG_EXTERN void      Sg_ByteVectorIEEEDoubleNativeSet(SgByteVector *bv,
						     long index,
						     double value);
SG_EXTERN void      Sg_ByteVectorIEEEDoubleLittleSet(SgByteVector *bv,
						     long index,
						     double value);
SG_EXTERN void      Sg_ByteVectorIEEEDoubleBigSet(SgByteVector *bv,
						  long index, double value);

/* utility */
SG_EXTERN SgObject Sg_ByteVectorToIntegerBig(SgByteVector *bv,
					     long start, long end);
SG_EXTERN SgObject Sg_ByteVectorToIntegerSBig(SgByteVector *bv,
					      long start, long end);
SG_EXTERN SgObject Sg_IntegerToByteVectorBig(SgObject num, long size);
SG_EXTERN SgObject Sg_SIntegerToByteVectorBig(SgObject num, long size);
SG_EXTERN SgObject Sg_ByteVectorToIntegerLittle(SgByteVector *bv,
						long start, long end);
SG_EXTERN SgObject Sg_ByteVectorToIntegerSLittle(SgByteVector *bv,
						 long start, long end);
SG_EXTERN SgObject Sg_IntegerToByteVectorLittle(SgObject num, long size);
SG_EXTERN SgObject Sg_SIntegerToByteVectorLittle(SgObject num, long size);
SG_EXTERN SgObject Sg_ByteVectorConcatenate(SgObject bvList);

#define Sg_ByteVectorToInteger Sg_ByteVectorToIntegerBig
#define Sg_IntegerToByteVector Sg_IntegerToByteVectorBig
#define Sg_ByteVectorToIntegerS Sg_ByteVectorToIntegerSBig
#define Sg_SIntegerToByteVector Sg_SIntegerToByteVectorBig

SG_EXTERN int      Sg_ByteVectorCmp(SgByteVector *x, SgByteVector *y);

SG_CDECL_END

#endif /* SAGITTARIUS_BYTEVECTOR_H_ */
