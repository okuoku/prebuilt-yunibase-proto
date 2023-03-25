/* error.h                                         -*- mode:c; coding:utf-8; -*-
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
#ifndef SAGITTARIUS_PRIVATE_ERROR_H_
#define SAGITTARIUS_PRIVATE_ERROR_H_

#include "sagittariusdefs.h"

typedef enum {
  SG_IO_READ_ERROR,
  SG_IO_WRITE_ERROR,
  SG_IO_FILE_NOT_EXIST_ERROR,
  SG_IO_FILE_ALREADY_EXIST_ERROR,
  SG_IO_DECODE_ERROR,
  SG_IO_ENCODE_ERROR,
  SG_IO_FILENAME_ERROR,
  SG_IO_FILE_PROTECTION_ERROR,
  SG_IO_UNKNOWN_ERROR = -1		/* for convenience */
} SgIOErrorType;

SG_CDECL_BEGIN

SG_EXTERN void Sg_Warn(const SgChar* msg, ...);
SG_EXTERN void Sg_Error(const SgChar* msg, ...);
SG_EXTERN void Sg_ReadError(const SgChar* msg, ...);
SG_EXTERN void Sg_SystemError(int errono, const SgChar* msg, ...);
SG_EXTERN void Sg_SyntaxError(SgObject form, SgObject irritants);

/* general &i/o exception*/
SG_EXTERN void Sg_IOError(SgIOErrorType type, SgObject who, SgObject msg, 
			  SgObject file, SgObject port);
SG_EXTERN void Sg_IOReadError(SgObject who, SgObject msg, SgObject port,
			      SgObject irr);
SG_EXTERN void Sg_IOWriteError(SgObject who, SgObject msg, SgObject port,
			       SgObject irr);
SG_EXTERN void Sg_IOFileDoesNotExistError(SgObject file, SgObject who,
					  SgObject msg);
SG_EXTERN void Sg_IOFileAlreadyExistsError(SgObject file, SgObject who,
				      SgObject msg);
SG_EXTERN void Sg_IODecodingError(SgObject port, SgObject who, SgObject msg);
SG_EXTERN void Sg_IOEncodingError(SgObject port, SgChar c, SgObject who,
				  SgObject msg);
SG_EXTERN void Sg_IOFilenameError(SgObject file, SgObject who, SgObject msg);
SG_EXTERN void Sg_IOFileProtectionError(SgObject file, SgObject who,
					SgObject msg);

SG_EXTERN void Sg_AssertionViolation(SgObject who, SgObject message, 
				     SgObject irritants);
SG_EXTERN void Sg_UndefinedViolation(SgObject who, SgObject message);
SG_EXTERN void Sg_ImplementationRestrictionViolation(SgObject who,
						     SgObject message,
						     SgObject irritants);

/* these are for stub files */
SG_EXTERN void Sg_WrongTypeOfArgumentViolation(SgObject who,
					       SgObject requiredType,
					       SgObject gotValue,
					       SgObject irritants);
SG_EXTERN void Sg_WrongNumberOfArgumentsViolation(SgObject who,
						  int requiredCounts,
						  int gotCounts,
						  SgObject irritants);
SG_EXTERN void Sg_WrongNumberOfArgumentsAtLeastViolation(SgObject who,
							 int requiredCounts,
							 int gotCounts,
							 SgObject irritants);
SG_EXTERN void Sg_WrongNumberOfArgumentsBetweenViolation(SgObject who,
							 int startCounts,
							 int endCounts,
							 int gotCounts,
							 SgObject irritants);

SG_EXTERN SgObject Sg_Raise(SgObject condition, int continuableP);

SG_EXTERN SgIOErrorType Sg_ErrnoToIOErrorType(int e);

SG_CDECL_END

#endif /* SAGITTARIUS_ERROR_HPP_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
