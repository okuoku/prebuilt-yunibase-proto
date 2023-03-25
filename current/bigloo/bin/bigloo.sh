#!/bin/sh
#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bigloo/autoconf/bigloo.sh.in         */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Tue Oct 13 14:06:20 2015                          */
#*    Last change :  Fri May  1 15:35:51 2020 (serrano)                */
#*    Copyright   :  2015-20 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    Bigloo wrapper                                                   */
#*=====================================================================*/

LD_LIBRARY_PATH=/yunisrc/yunibase/impl-current/bigloo/libbacktrace/opt/yunibase/current/bigloo/lib/bigloo/4.5b:/opt/yunibase/current/bigloo/lib/bigloo/4.5b:$LD_LIBRARY_PATH
DYLD_LIBRARY_PATH=/yunisrc/yunibase/impl-current/bigloo/libbacktrace/opt/yunibase/current/bigloo/lib/bigloo/4.5b:/opt/yunibase/current/bigloo/lib/bigloo/4.5b:$DYLD_LIBRARY_PATH

export LD_LIBRARY_PATH
export DYLD_LIBRARY_PATH

exec /opt/yunibase/current/bigloo/bin/bigloo -ldopt "-L/yunisrc/yunibase/impl-current/bigloo/libbacktrace/opt/yunibase/current/bigloo/lib/bigloo/4.5b " "$@"
