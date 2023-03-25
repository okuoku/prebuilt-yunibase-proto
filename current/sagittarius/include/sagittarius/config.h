#ifndef SAGITTARIUS_CONFIG_H
#define SAGITTARIUS_CONFIG_H

#define SAGITTARIUS_VERSION "0.9.10"
#define SAGITTARIUS_SHARE_LIB_PATH "/opt/yunibase/current/sagittarius/share/sagittarius/0.9.10/lib"
#define SAGITTARIUS_SHARE_SITE_LIB_PATH "/opt/yunibase/current/sagittarius/share/sagittarius/0.9.10/sitelib"
#define SAGITTARIUS_SITE_LIB_PATH "/opt/yunibase/current/sagittarius/share/sagittarius/sitelib"
#define SAGITTARIUS_DYNLIB_PATH "/opt/yunibase/current/sagittarius/lib/sagittarius/0.9.10/x86_64-pc-linux"
#define SAGITTARIUS_SITE_DYNLIB_PATH "/opt/yunibase/current/sagittarius/lib/sagittarius/sitelib/x86_64-pc-linux"

#define SAGITTARIUS_TRIPLE "x86_64-pc-linux"

#define USE_BOEHM_GC
#define USE_IMMEDIATE_FLONUM
/* #undef USE_UCS4_CPP */

/* #undef HAVE_GC_H */
#define HAVE_GC_GC_H
#define HAVE_ALLOCA_H
#define HAVE_STDINT_H
#define HAVE_STDLIB_H
#define HAVE_STRING_H
#define HAVE_STDIO_H
#define HAVE_LIMITS_H
#define HAVE_STDARG_H
#define HAVE_SETJMP_H
#define HAVE_SYS_TIME_H
#define HAVE_TIME_H
#define HAVE_SIGNAL_H
#define HAVE_DLFCN_H
#define HAVE_SCHED_H
/* #undef HAVE_IO_H */
#define HAVE_SYS_MMAN_H
#define HAVE_UNISTD_H
#define HAVE_SEMAPHORE_H
#define HAVE_EXECINFO_H
#define HAVE_FEATURES_H
#define HAVE_PTHREAD_H

/* C11 features */
#define HAVE_UCHAR_H
#define HAVE_STDNORETURN_H

#define HAVE_TIMESPEC

#define HAVE_ALLOCA
#define HAVE_SCHED_YIELD
#define HAVE_NANOSLEEP
#define HAVE_SELECT
#define HAVE_CLOCK_GETTIME
#define HAVE_GETTIMEOFDAY
#define HAVE_MMAP
#define HAVE_GETRUSAGE
#define HAVE_SEM_TIMEDWAIT
#define HAVE_SEM_TRYWAIT
#define HAVE_BACKTRACE
#define HAVE_BACKTRACE_SYMBOL
#define HAVE_BACKTRACE_SYMBOL_FD
#define HAVE_UTIMENSAT
#define HAVE_UTIMES
/* C11 feature for MSVC as wchar_t is 16bits... */
#define HAVE_CHAR32_T

#define HAVE_MUTEX_RECURSIVE
#define HAVE_MUTEX_RECURSIVE_NP
#define HAVE_PTHREAD_GETCPUCLOCKID
/* for CPU count */
#define HAVE__SC_NPROCESSORS_ONLN
#define HAVE__SC_NPROCESSORS_CONF
#define HAVE_SCHED_GETAFFINITY
#define HAVE_CPU_COUNT

#define SHLIB_SO_SUFFIX ".so"

#define SIZEOF_INT 4
#define SIZEOF_SHORT 2
#define SIZEOF_LONG 8
#define SIZEOF___INT64 8
#define SIZEOF_SIZE_T 8
#define SIZEOF_VOIDP 8
#define SIZEOF_FLOAT 4
#define SIZEOF_DOUBLE 8
#define SIZEOF_WCHAR_T 4

#define SAGITTARIUS_PROFILE

#define SAGITTARIUS_PLATFORM "linux"
#define SAGITTARIUS_PROCESSOR "x86_64"

#ifdef _MSC_VER
#define PRIdPTR     "Id"
#endif

#endif
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
