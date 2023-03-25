/*                                                              -*- C -*-
 * extra.h      -- Extra pre-processor definitions
 *
 * Copyright ? 2000-2020 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
 * USA.
 *
 *           Author: Erick Gallesio [eg@unice.fr]
 *    Creation date: 19-May-2000 18:44 (eg)
 */

#define PREFIXDIR               "/opt/yunibase/current/stklos"
#define SCMDIR                  "/opt/yunibase/current/stklos/share/stklos/1.70"
#define DEFAULT_BOOT_FILE       "/opt/yunibase/current/stklos/share/stklos/1.70/boot.img"
#define EXECDIR                 "/opt/yunibase/current/stklos/lib/stklos/1.70"
#define BUILD_OS                "Linux-5.15.0-67-generic"
#define BUILD_ARCH              "x86_64"
#define BUILD_MACHINE           "Linux-5.15.0-67-generic-x86_64"
#define LINUX               1
#define LINUX_5_15       1
#define STACK_GROWS_UP
#define PCRE_PKG_CONFIG         "present"

/* SRFI-176 stuff */
#define CONF_SUMMARY            "(:system (libffi libpcre libgmp libgc ) :compiled () :configure ( \"--prefix=/opt/yunibase/current/stklos\" \"CC=gcc\" \"CFLAGS=-g -O2\"))"
#define C_VERSION               "gcc (Ubuntu 11.2.0-19ubuntu1) 11.2.0"
#define C_COMPILE               "(\"gcc\" \"-g\" \"-O2\" \"-rdynamic\" )"
#define C_LINK                  "(\"gcc\" \"-lgmp\" \"-lpcre\" \"-lpcreposix\" \"-lffi\" \"-lgc\" \"-lpthread\" \"-ldl\" \"-lm\" )"

#define SHARED_LIB_COMPILE      "(\"gcc\" \"-fpic\" \"-nostdlib\" \"-I/opt/yunibase/current/stklos/include/stklos\" \"-I/opt/yunibase/current/stklos/include/stklos/gc\" )"
#define SHARED_LIB_LINK         "(\"ld\" \"-shared\" )"
#define SHARED_LIB_SUFFIX       "so"
#define SHARED_SUFFIX           "so"
#define SHARED_EXTENSION        ".so"


/* Various stuff */
#define unix 1
#define GNU_Linux    1           /* result of uname -o */
#define DEFAULT_CASE_SENSITIVE  1
#define CONTROL_FX_PARAMETERS   1
