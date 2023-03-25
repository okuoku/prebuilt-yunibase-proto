/* values.h                                       -*- mode:c; coding:utf-8; -*-
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
#ifndef SAGITTARIUS_PRIVATE_VALUES_H_
#define SAGITTARIUS_PRIVATE_VALUES_H_

#include "sagittariusdefs.h"

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_Values(SgObject args);
SG_EXTERN SgObject Sg_Values2(SgObject v1, SgObject v2);
SG_EXTERN SgObject Sg_Values3(SgObject v1, SgObject v2, SgObject v3);
SG_EXTERN SgObject Sg_Values4(SgObject v1, SgObject v2, SgObject v3, 
			      SgObject v4);
SG_EXTERN SgObject Sg_Values5(SgObject v1, SgObject v2, SgObject v3, 
			      SgObject v4, SgObject v5);

SG_CDECL_END

#endif /* SAGITTARIUS_VALUES_H_ */
/*
  end of file:
  Local Variables:
  coding: utf-8-unix
  End:
 */
