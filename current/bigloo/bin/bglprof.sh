#!/bin/sh
#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bigloo/autoconf/bglprof.sh.in        */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Tue Oct 13 14:06:43 2015                          */
#*    Last change :  Sun Jan  7 15:37:23 2018 (serrano)                */
#*    Copyright   :  2015-18 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    Afile wrapper                                                    */
#*=====================================================================*/

LD_LIBRARY_PATH=/yunisrc/yunibase/impl-current/bigloo/libbacktrace/opt/yunibase/current/bigloo/lib/bigloo/4.5b:/opt/yunibase/current/bigloo/lib/bigloo/4.5b:$LD_LIBRARY_PATH
DYLD_LIBRARY_PATH=/yunisrc/yunibase/impl-current/bigloo/libbacktrace/opt/yunibase/current/bigloo/lib/bigloo/4.5b:/opt/yunibase/current/bigloo/lib/bigloo/4.5b:$DYLD_LIBRARY_PATH

export LD_LIBRARY_PATH
export DYLD_LIBRARY_PATH

exec /opt/yunibase/current/bigloo/bin/bglprof "$@"
