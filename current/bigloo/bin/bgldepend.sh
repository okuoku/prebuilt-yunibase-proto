#!/bin/sh
#*=====================================================================*/
#*    serrano/prgm/project/bigloo/autoconf/bgldepend.sh.in             */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Tue Oct 13 14:06:43 2015                          */
#*    Last change :  Wed Oct 14 09:07:34 2015 (serrano)                */
#*    Copyright   :  2015 Manuel Serrano                               */
#*    -------------------------------------------------------------    */
#*    Afile wrapper                                                    */
#*=====================================================================*/

LD_LIBRARY_PATH=/yunisrc/yunibase/impl-current/bigloo/libbacktrace/opt/yunibase/current/bigloo/lib/bigloo/4.5b:/opt/yunibase/current/bigloo/lib/bigloo/4.5b:$LD_LIBRARY_PATH
DYLD_LIBRARY_PATH=/yunisrc/yunibase/impl-current/bigloo/libbacktrace/opt/yunibase/current/bigloo/lib/bigloo/4.5b:/opt/yunibase/current/bigloo/lib/bigloo/4.5b:$DYLD_LIBRARY_PATH

export LD_LIBRARY_PATH
export DYLD_LIBRARY_PATH

exec /opt/yunibase/current/bigloo/bin/bgldepend "$@"
