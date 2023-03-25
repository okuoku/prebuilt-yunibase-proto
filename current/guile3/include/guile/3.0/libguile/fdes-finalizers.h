#ifndef SCM_FDES_FINALIZERS_H
#define SCM_FDES_FINALIZERS_H

/* Copyright 2016,2018
     Free Software Foundation, Inc.

   This file is part of Guile.

   Guile is free software: you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Guile is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
   License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with Guile.  If not, see
   <https://www.gnu.org/licenses/>.  */



#include "libguile/scm.h"



SCM_INTERNAL SCM scm_add_fdes_finalizer_x (SCM fd, SCM finalizer);
SCM_INTERNAL SCM scm_remove_fdes_finalizer_x (SCM fd, SCM finalizer);
SCM_INTERNAL void scm_run_fdes_finalizers (int fd);

SCM_INTERNAL void scm_register_fdes_finalizers (void);

#endif  /* SCM_FDES_FINALIZERS_H */
