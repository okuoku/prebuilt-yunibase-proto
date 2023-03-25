/* src/gauche/config.h.  Generated from config.h.in by configure.  */
/* src/gauche/config.h.in.  Generated from configure.ac by autoheader.  */

/* Define if building universal (internal helper macro) */
/* #undef AC_APPLE_UNIVERSAL_BUILD */

/* Define to one of `_getb67', `GETB67', `getb67' for Cray-2 and Cray-YMP
   systems. This function is required for `alloca.c' support on those systems.
   */
/* #undef CRAY_STACKSEG_END */

/* Define to 1 if using `alloca.c'. */
/* #undef C_ALLOCA */

/* Define to use mixed endian ARM processor */
/* #undef DOUBLE_ARMENDIAN */

/* Define to Gauche ABI version string */
#define GAUCHE_ABI_VERSION "0.98"

/* Define to override Gauche API version number */
/* #undef GAUCHE_API_VERSION */

/* CA file path */
/* #undef GAUCHE_CA_BUNDLE */

/* Define to check CA availability at runtime */
#define GAUCHE_CA_BUNDLE_CHECK 1

/* Define to use file CA */
/* #undef GAUCHE_CA_BUNDLE_FILE */

/* Define not to use CA */
/* #undef GAUCHE_CA_BUNDLE_NONE */

/* Define to use system CA */
/* #undef GAUCHE_CA_BUNDLE_SYSTEM */

/* Define if Gauche handles multi-byte character as EUC-JP */
/* #undef GAUCHE_CHAR_ENCODING_EUC_JP */

/* Define if Gauche handles multi-byte character as Shift JIS */
/* #undef GAUCHE_CHAR_ENCODING_SJIS */

/* Define if Gauche handles multi-byte character as UTF-8 */
#define GAUCHE_CHAR_ENCODING_UTF_8 /**/

/* Define 1 if building framework on MacOSX */
/* #undef GAUCHE_MACOSX_FRAMEWORK */

/* Gauche major version number */
#define GAUCHE_MAJOR_VERSION 0

/* Gauche patch level number */
#define GAUCHE_MICRO_VERSION 13

/* Gauche minor version number */
#define GAUCHE_MINOR_VERSION 9

/* Gauche signature string */
#define GAUCHE_SIGNATURE "0.98,utf8,pthreads"

/* Define if you use axTLS */
/* #undef GAUCHE_USE_AXTLS */

/* Define if you use mbed TLS */
/* #undef GAUCHE_USE_MBEDTLS */

/* Define if we use pthreads */
#define GAUCHE_USE_PTHREADS 1

/* Define if we use windows threads */
/* #undef GAUCHE_USE_WTHREADS */

/* Gauche version string */
#define GAUCHE_VERSION "0.9.13_pre2"

/* Define to use Darwin threads */
/* #undef GC_DARWIN_THREADS */

/* Define to use FreeBSD threads */
/* #undef GC_FREEBSD_THREADS */

/* Define to use GNU threads */
/* #undef GC_GNU_THREADS */

/* Define to use HP-UX threads */
/* #undef GC_HPUX_THREADS */

/* Define to use IRIX threads */
/* #undef GC_IRIX_THREADS */

/* Define to use Linux threads */
#define GC_LINUX_THREADS 1

/* Define to use NetBSD threads */
/* #undef GC_NETBSD_THREADS */

/* Define to use OpenBSD threads */
/* #undef GC_OPENBSD_THREADS */

/* Define to use pthreads */
/* #undef GC_PTHREADS */

/* Define to use Solaris pthreads */
/* #undef GC_SOLARIS_PTHREADS */

/* Define to use Solaris threads */
/* #undef GC_SOLARIS_THREADS */

/* Define to use Win32 threads */
/* #undef GC_WIN32_THREADS */

/* Define number of args gethostbyaddr_r takes */
/* #undef GETHOSTBYADDR_R_NUMARGS */

/* Define number of args gethostbyname_r takes */
/* #undef GETHOSTBYNAME_R_NUMARGS */

/* Define number of args getprotobyname_r takes */
/* #undef GETPROTOBYNAME_R_NUMARGS */

/* Define number of args getprotobynumber_r takes */
/* #undef GETPROTOBYNUMBER_R_NUMARGS */

/* Define number of args getservbyname_r takes */
/* #undef GETSERVBYNAME_R_NUMARGS */

/* Define number of args getservbyport_r takes */
/* #undef GETSERVBYPORT_R_NUMARGS */

/* Define to 1 if you have `alloca', as a function or macro. */
#define HAVE_ALLOCA 1

/* Define to 1 if you have <alloca.h> and it should be used (not on Ultrix).
   */
#define HAVE_ALLOCA_H 1

/* Define to 1 if you have the <bsd/libutil.h> header file. */
/* #undef HAVE_BSD_LIBUTIL_H */

/* Define to 1 if you have the `clearenv' function. */
#define HAVE_CLEARENV 1

/* Define to 1 if you have the `clock_getres' function. */
#define HAVE_CLOCK_GETRES 1

/* Define to 1 if you have the `clock_gettime' function. */
#define HAVE_CLOCK_GETTIME 1

/* Define to 1 if you have the <crt_externs.h> header file. */
/* #undef HAVE_CRT_EXTERNS_H */

/* Define if uses libcrypt */
#define HAVE_CRYPT 1

/* Define to 1 if you have the <crypt.h> header file. */
#define HAVE_CRYPT_H 1

/* Define 1 you have the <dbm.h> header file. */
/* #undef HAVE_DBM_H */

/* Define to 1 if you have the `dbm_open' function. */
/* #undef HAVE_DBM_OPEN */

/* Define to 1 if you have the <dlfcn.h> header file. */
#define HAVE_DLFCN_H 1

/* Define if the system has dlopen() */
#define HAVE_DLOPEN 1

/* Define if you have fcntl */
#define HAVE_FCNTL 1

/* Define if you have forkpty */
#define HAVE_FORKPTY 1

/* Define to 1 if you have the `fpsetprec' function. */
/* #undef HAVE_FPSETPREC */

/* Define to 1 if you have the <fpu_control.h> header file. */
#define HAVE_FPU_CONTROL_H 1

/* Define to 1 if you have the <gdbm.h> header file. */
/* #undef HAVE_GDBM_H */

/* Define 1 you have the <gdbm-dbm.h> header file. */
/* #undef HAVE_GDBM_MINUS_DBM_H */

/* Define 1 you have the <gdbm-ndbm.h> header file. */
/* #undef HAVE_GDBM_MINUS_NDBM_H */

/* Define 1 you have the <gdbm/dbm.h> header file. */
/* #undef HAVE_GDBM_SLASH_DBM_H */

/* Define 1 you have the <gdbm/ndbm.h> header file. */
/* #undef HAVE_GDBM_SLASH_NDBM_H */

/* Define to 1 if you have the `getdomainname' function. */
#define HAVE_GETDOMAINNAME 1

/* Define to 1 if you have the `gethostname' function. */
#define HAVE_GETHOSTNAME 1

/* Define to 1 if you have the `getloadavg' function. */
#define HAVE_GETLOADAVG 1

/* Define to 1 if you have the <getopt.h> header file. */
#define HAVE_GETOPT_H 1

/* Define to 1 if you have the `getpgid' function. */
#define HAVE_GETPGID 1

/* Define to 1 if you have the `gettimeofday' function. */
#define HAVE_GETTIMEOFDAY 1

/* Define to 1 if you have the <glob.h> header file. */
#define HAVE_GLOB_H 1

/* Define to 1 if you have the <gperftools/profiler.h> header file. */
/* #undef HAVE_GPERFTOOLS_PROFILER_H */

/* Define if you have iconv.h and want to use it */
/* #undef HAVE_ICONV_H */

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define if you have IPv6 support */
#define HAVE_IPV6 1

/* Define to 1 if you have the `isinf' function. */
#define HAVE_ISINF 1

/* Define to 1 if you have the `isnan' function. */
#define HAVE_ISNAN 1

/* Define to 1 if you have the `issetugid' function. */
/* #undef HAVE_ISSETUGID */

/* Define to 1 if you have the `lchown' function. */
#define HAVE_LCHOWN 1

/* Define to 1 if you have the `lgamma' function. */
#define HAVE_LGAMMA 1

/* Define to 1 if you have the `m' library (-lm). */
#define HAVE_LIBM 1

/* Define to 1 if you have the `libproc' library (-lproc). */
/* #undef HAVE_LIBPROC */

/* Define to 1 if you have the <libproc.h> header file. */
/* #undef HAVE_LIBPROC_H */

/* Define to 1 if you have the `rt' library (-lrt). */
#define HAVE_LIBRT 1

/* Define to 1 if you have the `sunmath' library (-lsunmath). */
/* #undef HAVE_LIBSUNMATH */

/* Define to 1 if you have the <libutil.h> header file. */
/* #undef HAVE_LIBUTIL_H */

/* Define to 1 if the system has the type `long double'. */
#define HAVE_LONG_DOUBLE 1

/* Define to 1 if the system has the type `long long'. */
#define HAVE_LONG_LONG 1

/* Define to 1 if you have the `lrand48' function. */
#define HAVE_LRAND48 1

/* Define to 1 if you have the <malloc.h> header file. */
#define HAVE_MALLOC_H 1

/* Define to 1 if you have the <memory.h> header file. */
/* #undef HAVE_MEMORY_H */

/* Define to 1 if you have the `mkdtemp' function. */
#define HAVE_MKDTEMP 1

/* Define to 1 if you have the `mkstemp' function. */
#define HAVE_MKSTEMP 1

/* Define to 1 if you have the `nanosleep' function. */
#define HAVE_NANOSLEEP 1

/* Define 1 you have the <ndbm.h> header file. */
/* #undef HAVE_NDBM_H */

/* Define to 1 if you have the <net/if.h> header file. */
#define HAVE_NET_IF_H 1

/* Define if you have openpty */
#define HAVE_OPENPTY 1

/* Define if you have pthread_cancel */
#define HAVE_PTHREAD_CANCEL 1

/* Define to 1 if the system has the type `pthread_spinlock_t'. */
#define HAVE_PTHREAD_SPINLOCK_T 1

/* Define to 1 if you have the <pty.h> header file. */
#define HAVE_PTY_H 1

/* Define to 1 if you have the `putenv' function. */
#define HAVE_PUTENV 1

/* Define to 1 if you have the `random' function. */
#define HAVE_RANDOM 1

/* Define to 1 if you have the `readlink' function. */
#define HAVE_READLINK 1

/* Define to 1 if you have the `realpath' function. */
#define HAVE_REALPATH 1

/* Define to 1 if you have the `rint' function. */
#define HAVE_RINT 1

/* Define to 1 if you have the <rpc/types.h> header file. */
/* #undef HAVE_RPC_TYPES_H */

/* Define to 1 if you have the <sched.h> header file. */
#define HAVE_SCHED_H 1

/* Define if uses librt */
#define HAVE_SCHED_YIELD 1

/* Define to 1 if you have the `select' function. */
#define HAVE_SELECT 1

/* Define to 1 if you have the `setdomainname' function. */
#define HAVE_SETDOMAINNAME 1

/* Define to 1 if you have the `setenv' function. */
#define HAVE_SETENV 1

/* Define to 1 if you have the `setgroups' function. */
#define HAVE_SETGROUPS 1

/* Define to 1 if you have the `sethostname' function. */
#define HAVE_SETHOSTNAME 1

/* Define to 1 if you have the `setlogmask' function. */
#define HAVE_SETLOGMASK 1

/* Define to 1 if you have the `sigwait' function. */
#define HAVE_SIGWAIT 1

/* Define to 1 if you have the `srand48' function. */
#define HAVE_SRAND48 1

/* Define to 1 if you have the `srandom' function. */
#define HAVE_SRANDOM 1

/* Define to 1 if you have the <stdalign.h> header file. */
#define HAVE_STDALIGN_H 1

/* Define to 1 if you have the <stdatomic.h> header file. */
#define HAVE_STDATOMIC_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have `strsignal' function. */
#define HAVE_STRSIGNAL 1

/* Define to 1 if `gr_passwd' is a member of `struct group'. */
#define HAVE_STRUCT_GROUP_GR_PASSWD 1

/* Define to 1 if the system has the type `struct ifreq'. */
#define HAVE_STRUCT_IFREQ 1

/* Define to 1 if `ifr_ifindex' is a member of `struct ifreq'. */
#define HAVE_STRUCT_IFREQ_IFR_IFINDEX 1

/* Define to 1 if `ifr_index' is a member of `struct ifreq'. */
/* #undef HAVE_STRUCT_IFREQ_IFR_INDEX */

/* Define to 1 if `ifr_netmask' is a member of `struct ifreq'. */
#define HAVE_STRUCT_IFREQ_IFR_NETMASK 1

/* Define to 1 if `pw_class' is a member of `struct passwd'. */
/* #undef HAVE_STRUCT_PASSWD_PW_CLASS */

/* Define to 1 if `pw_gecos' is a member of `struct passwd'. */
#define HAVE_STRUCT_PASSWD_PW_GECOS 1

/* Define to 1 if `pw_passwd' is a member of `struct passwd'. */
#define HAVE_STRUCT_PASSWD_PW_PASSWD 1

/* Define to 1 if `sin6_len' is a member of `struct sockaddr_in6'. */
/* #undef HAVE_STRUCT_SOCKADDR_IN6_SIN6_LEN */

/* Define to 1 if `sin_len' is a member of `struct sockaddr_in'. */
/* #undef HAVE_STRUCT_SOCKADDR_IN_SIN_LEN */

/* Define to 1 if the system has the type `struct sockaddr_storage'. */
#define HAVE_STRUCT_SOCKADDR_STORAGE 1

/* Define to 1 if `sun_len' is a member of `struct sockaddr_un'. */
/* #undef HAVE_STRUCT_SOCKADDR_UN_SUN_LEN */

/* Define to 1 if `st_atim' is a member of `struct stat'. */
#define HAVE_STRUCT_STAT_ST_ATIM 1

/* Define to 1 if `st_ctim' is a member of `struct stat'. */
#define HAVE_STRUCT_STAT_ST_CTIM 1

/* Define to 1 if `st_mtim' is a member of `struct stat'. */
#define HAVE_STRUCT_STAT_ST_MTIM 1

/* Define to 1 if the system has the type `struct timespec'. */
#define HAVE_STRUCT_TIMESPEC 1

/* Define to 1 if you have the <sunmath.h> header file. */
/* #undef HAVE_SUNMATH_H */

/* Define to 1 if you have the `symlink' function. */
#define HAVE_SYMLINK 1

/* Define to 1 if you have the `syslog' function. */
#define HAVE_SYSLOG 1

/* Define to 1 if you have the <syslog.h> header file. */
#define HAVE_SYSLOG_H 1

/* Define to 1 if you have the <sys/event.h> header file. */
/* #undef HAVE_SYS_EVENT_H */

/* Define to 1 if you have the <sys/inotify.h> header file. */
#define HAVE_SYS_INOTIFY_H 1

/* Define to 1 if you have the <sys/loadavg.h> header file. */
/* #undef HAVE_SYS_LOADAVG_H */

/* Define to 1 if you have the <sys/mman.h> header file. */
#define HAVE_SYS_MMAN_H 1

/* Define to 1 if you have the <sys/resource.h> header file. */
#define HAVE_SYS_RESOURCE_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/statvfs.h> header file. */
#define HAVE_SYS_STATVFS_H 1

/* Define to 1 if you have the <sys/time.h> header file. */
#define HAVE_SYS_TIME_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the `tgamma' function. */
#define HAVE_TGAMMA 1

/* Define to 1 if you have the <time.h> header file. */
#define HAVE_TIME_H 1

/* Define to 1 if you have the `trunc' function. */
#define HAVE_TRUNC 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to 1 if you have the `unsetenv' function. */
#define HAVE_UNSETENV 1

/* Define to 1 if you have the `usleep' function. */
#define HAVE_USLEEP 1

/* Define to 1 if you have the <util.h> header file. */
/* #undef HAVE_UTIL_H */

/* Define to 1 if you have the `utimensat' function. */
#define HAVE_UTIMENSAT 1

/* Define to 1 if you have the <wincrypt.h> header file. */
/* #undef HAVE_WINCRYPT_H */

/* Define if you have zlib.h and want to use it */
#define HAVE_ZLIB_H 1

/* Define if time_t is typedef'ed to an integral type */
#define INTEGRAL_TIME_T 1

/* Define to use i386 optimizations */
/* #undef SCM_TARGET_I386 */

/* Define to use x86_64 optimizations */
#define SCM_TARGET_X86_64 1

/* Shared library file suffix */
#define SHLIB_SO_SUFFIX "so"

/* The size of `double', as computed by sizeof. */
#define SIZEOF_DOUBLE 8

/* The size of `float', as computed by sizeof. */
#define SIZEOF_FLOAT 4

/* The size of `int', as computed by sizeof. */
#define SIZEOF_INT 4

/* The size of `long', as computed by sizeof. */
#define SIZEOF_LONG 8

/* The size of `intptr_t', as computed by sizeof. */
#define SIZEOF_INTPTR_T 8

/* The size of `off_t', as computed by sizeof. */
#define SIZEOF_OFF_T 8

/* The size of `ptrdiff_t', as computed by sizeof. */
#define SIZEOF_PTRDIFF_T 8

/* The size of `rlim_t', as computed by sizeof. */
#define SIZEOF_RLIM_T 8

/* The size of `size_t', as computed by sizeof. */
#define SIZEOF_SIZE_T 8

/* The size of `ssize_t', as computed by sizeof. */
#define SIZEOF_SSIZE_T 8

/* The size of `uintptr_t', as computed by sizeof. */
#define SIZEOF_UINTPTR_T 8

/* Slib installation path */
#define SLIB_DIR "/usr/local/slib"

/* If using the C implementation of alloca, define if you know the
   direction of stack growth for your system; otherwise it will be
   automatically deduced at runtime.
        STACK_DIRECTION > 0 => grows toward higher addresses
        STACK_DIRECTION < 0 => grows toward lower addresses
        STACK_DIRECTION = 0 => direction of growth unknown */
/* #undef STACK_DIRECTION */

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Define to 1 if your <sys/time.h> declares `struct tm'. */
/* #undef TM_IN_SYS_TIME */

/* Define if uses iconv */
/* #undef USE_ICONV */

/* Define if uses zlib */
#define USE_ZLIB /**/

/* Define WORDS_BIGENDIAN to 1 if your processor stores words with the most
   significant byte first (like Motorola and SPARC, unlike Intel). */
#if defined AC_APPLE_UNIVERSAL_BUILD
# if defined __BIG_ENDIAN__
#  define WORDS_BIGENDIAN 1
# endif
#else
# ifndef WORDS_BIGENDIAN
/* #  undef WORDS_BIGENDIAN */
# endif
#endif

/* Enable large inode numbers on Mac OS X 10.5.  */
#ifndef _DARWIN_USE_64_BIT_INODE
# define _DARWIN_USE_64_BIT_INODE 1
#endif

/* Number of bits in a file offset, on hosts where this is settable. */
/* #undef _FILE_OFFSET_BITS */

/* Define for large files, on AIX-style hosts. */
/* #undef _LARGE_FILES */

/* Define POSIX C version */
/* #undef _POSIX_C_SOURCE */

/* Define to use Solaris pthreads */
/* #undef _POSIX_PTHREAD_SEMANTICS */

/* Define if thread support requires this symbol */
/* #undef _PTHREADS */

/* Define to use reentrant libc */
#define _REENTRANT 1

/* Define to empty if `const' does not conform to ANSI C. */
/* #undef const */

/* Define to `__inline__' or `__inline' if that's what the C compiler
   calls it, or to nothing if 'inline' is not supported under any name.  */
#ifndef __cplusplus
/* #undef inline */
#endif

/* Define to `unsigned int' if <sys/types.h> does not define. */
/* #undef size_t */

/* Substitute for socklen_t */
/* #undef socklen_t */
