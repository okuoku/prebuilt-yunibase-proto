#!/bin/sh
#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bigloo/autoconf/bglafile.sh.in       */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Tue Oct 13 14:06:43 2015                          */
#*    Last change :  Thu Apr  8 09:48:43 2021 (serrano)                */
#*    Copyright   :  2015-21 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    Afile wrapper                                                    */
#*=====================================================================*/

LD_LIBRARY_PATH=/opt/yunibase/current/bigloo/lib/bigloo/4.5b:/opt/yunibase/current/bigloo/lib/bigloo/4.5b:/opt/yunibase/current/bigloo/lib/bigloo/4.5b:/yunisrc/yunibase/impl-current/bigloo/libuv/libuv-v1.44.2/.libs:/opt/yunibase/current/bigloo/lib/bigloo/4.5b:/yunisrc/yunibase/impl-current/bigloo/libbacktrace/opt/yunibase/current/bigloo/lib/bigloo/4.5b:/opt/yunibase/current/bigloo/lib/bigloo/4.5b:$LD_LIBRARY_PATH
DYLD_LIBRARY_PATH=/opt/yunibase/current/bigloo/lib/bigloo/4.5b:/opt/yunibase/current/bigloo/lib/bigloo/4.5b:/opt/yunibase/current/bigloo/lib/bigloo/4.5b:/yunisrc/yunibase/impl-current/bigloo/libuv/libuv-v1.44.2/.libs:/opt/yunibase/current/bigloo/lib/bigloo/4.5b:/yunisrc/yunibase/impl-current/bigloo/libbacktrace/opt/yunibase/current/bigloo/lib/bigloo/4.5b:/opt/yunibase/current/bigloo/lib/bigloo/4.5b:$DYLD_LIBRARY_PATH

export LD_LIBRARY_PATH
export DYLD_LIBRARY_PATH

exec /opt/yunibase/current/bigloo/bin/bglafile "$@"
