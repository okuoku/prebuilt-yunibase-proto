/* extend.h                                        -*- mode:c; coding:utf-8; -*-
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
 */
#ifndef SAGITTARIUS_EXTEND_H_
#define SAGITTARIUS_EXTEND_H_

#ifndef SAGITTARIUS_H_
#include <sagittarius.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#if defined(__CYGWIN__)
  /* why did they change this... */
# if defined(__x86_64__)
  extern int __data_start__[], __data_end__[], __bss_start__[], __bss_end__[];
# define SG_INIT_EXTENSION(name)		\
  do {						\
    Sg_RegisterDL((void*)&__data_start__,	\
		  (void*)&__data_end__,		\
		  (void*)&__bss_start__,	\
		  (void*)&__bss_end__);		\
  } while (0)
# else
  extern int _data_start__[], _data_end__[], _bss_start__[], _bss_end__[];
# define SG_INIT_EXTENSION(name)		\
  do {						\
    Sg_RegisterDL((void*)&_data_start__,	\
		  (void*)&_data_end__,		\
		  (void*)&_bss_start__,		\
		  (void*)&_bss_end__);		\
  } while (0)
# endif
#else
#define SG_INIT_EXTENSION(name)	/* nothing */
#endif

#ifdef __cplusplus
#define SG_EXTENSION_ENTRY_QUAL extern "C"
#else
#define SG_EXTENSION_ENTRY_QUAL
#endif

#ifdef __WATCOMC__
# define CDECL __cdecl
#else
# define CDECL
#endif

#if defined(__CYGWIN__) || defined(SAGITTARIUS_WINDOWS)
#define SG_EXTENSION_ENTRY SG_EXTENSION_ENTRY_QUAL __declspec(dllexport)
#else
#define SG_EXTENSION_ENTRY SG_EXTENSION_ENTRY_QUAL
#endif

#undef SG_EXTERN
#undef SG_EXPORT
#if defined(__CYGWIN__) || defined(SAGITTARIUS_WINDOWS)
# if defined(LIBSAGITTARIUS_BODY) || defined(LIBSAGITTARIUS_EXT_BODY)
#  define SG_EXPORT __declspec(dllexport)
# else
#  define SG_EXPORT __declspec(dllimport)
# endif
# define SG_EXTERN extern SG_EXPORT
#else
# define SG_EXPORT 
# define SG_EXTERN extern
#endif


#ifdef __cplusplus
}
#endif

#endif /* SAGITTARIUS_EXTEND_H_ */
