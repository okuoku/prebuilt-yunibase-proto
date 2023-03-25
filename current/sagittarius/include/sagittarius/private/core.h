/* core.h                                          -*- mode:c; coding:utf-8; -*-
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
#ifndef SAGITTARIUS_PRIVATE_CORE_H_
#define SAGITTARIUS_PRIVATE_CORE_H_

#include "sagittariusdefs.h"
#include <sagittarius/gc.h>

/* 
   Main ans child thread must have different stack size
   (At least on Cygwin it does)
   Main  1M
   Child 64KB
 */
#define SG_MAIN_THREAD_STACK_SIZE_LIMIT  0x100000
#define SG_CHILD_THREAD_STACK_SIZE_LIMIT 0x10000

SG_CDECL_BEGIN

SG_EXTERN void  Sg_Init();

SG_EXTERN SG_NO_RETURN void Sg_Exit(int code);
SG_EXTERN SG_NO_RETURN void Sg_EmergencyExit(int code);
SG_EXTERN void 	Sg_Cleanup();
SG_EXTERN void*	Sg_AddCleanupHandler(void (*proc)(void *data), void *data);
SG_EXTERN void	Sg_DeleteCleanupHandler(void *handle);
SG_EXTERN void 	Sg_Panic(const char* msg, ...);
SG_EXTERN void 	Sg_Abort(const char* msg);
/* gc wrappers */
SG_EXTERN void 	Sg_GC();
SG_EXTERN int   Sg_FinalizerRegisteredP(SgObject z);

SG_EXTERN void 	Sg_RegisterDisappearingLink(void **p, void *value);
SG_EXTERN void 	Sg_UnregisterDisappearingLink(void **p);
SG_EXTERN void* Sg_GCBase(void *value);
SG_EXTERN size_t Sg_GetHeapSize();
SG_EXTERN size_t Sg_GetTotalBytes();
SG_EXTERN uintptr_t Sg_GcCount();
SG_EXTERN void  Sg_GCSetPrintWarning(int onP);
SG_EXTERN int   Sg_GCStackBase(uintptr_t *base);
/* this is intptr_t to return -1 */
SG_EXTERN intptr_t   Sg_AvailableStackSize(uintptr_t csp);

/* experimental */

/* cond-expand */
SG_EXTERN void  Sg_AddCondFeature(const SgChar *feature);
SG_EXTERN SgObject Sg_CondFeatures();

/* convenient function to start */
SG_EXTERN void Sg_Start(SgObject in, SgObject args,
			const char *fmt, SgObject rest);


SG_CDECL_END

#endif /* SAGITTARIUS_PRIVATE_CORE_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
