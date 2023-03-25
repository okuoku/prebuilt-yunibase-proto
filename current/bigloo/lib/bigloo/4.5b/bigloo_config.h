/* Automatically generated file (don't edit), Sat Mar 18 05:48:55 UTC 2023 */
/*=====================================================================*/
/*    serrano/prgm/project/bigloo/autoconf/bigloo_config.h.in          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Mar 16 18:48:21 1995                          */
/*    Last change :  Mon Sep 22 15:03:58 2014 (serrano)                */
/*    -------------------------------------------------------------    */
/*    Bigloo configuration stuff                                       */
/*=====================================================================*/
#ifndef BIGLOO_CONFIG_H
#define BIGLOO_CONFIG_H

#define BGL_4_5b

#undef BGL_RELEASE_NUMBER
#define BGL_RELEASE_NUMBER "4.5b"

#undef BGL_RELEASE_BRANCH
#define BGL_RELEASE_BRANCH 450

#undef BGL_HOMEURL
#define BGL_HOMEURL "http://www-sop.inria.fr/indes/fp/Bigloo"

#undef BGL_SPECIFIC_VERSION
#define BGL_SPECIFIC_VERSION ""

#undef BGL_DEFAULT_BACK_END
#define BGL_DEFAULT_BACK_END "native"

#undef BGL_GC_VERSION
#define BGL_GC_VERSION 822

#undef BGL_GC_ISCUSTOM
#define BGL_GC_ISCUSTOM 1

#undef BGL_DEFAULT_A_OUT
#define BGL_DEFAULT_A_OUT "a.out"

#undef BGL_DEFAULT_A_BAT
#define BGL_DEFAULT_A_BAT "a.out"

#undef BGL_ISOC99
#define BGL_ISOC99 0

#undef BGL_HAS_ERRNOH
#define BGL_HAS_ERRNOH 1

#undef BGL_CCDIV0
#define BGL_CCDIV0 1

#undef BGL_HAVE_STDINT
#define BGL_HAVE_STDINT 1

#undef BGL_HAVE_STDLIBINT
#define BGL_HAVE_STDLIBINT 0

#undef BGL_HAVE_UNISTDINT
#define BGL_HAVE_UNISTDINT 0

#undef BGL_HAVE_GETUID
#define BGL_HAVE_GETUID 1

#undef BGL_HAVE_GETGID
#define BGL_HAVE_GETGID 1

#undef BGL_HAVE_GECOS
#define BGL_HAVE_GECOS 1

#undef BGL_HAVE_GETPROTOENT
#define BGL_HAVE_GETPROTOENT 1

#undef STACK_GROWS_DOWN
#define STACK_GROWS_DOWN 1

#undef PTR_ALIGNMENT
#define PTR_ALIGNMENT 3

#undef BGL_INT_BIT_SIZE
#define BGL_INT_BIT_SIZE 61

#undef BGL_ELONG_BIT_SIZE
#define BGL_ELONG_BIT_SIZE 64

#undef BGL_NAN_TAGGING
#define BGL_NAN_TAGGING 0

#undef BGL_RESERVED_TAGGING
#define BGL_RESERVED_TAGGING 0

#undef BGL_SMI
#define BGL_SMI 0

#undef HAVE_BCOPY
#define HAVE_BCOPY 1

#undef NB_WINDOW_REGISTER
#define NB_WINDOW_REGISTER 0

#undef HAVE_MUSTTAIL
#define HAVE_MUSTTAIL 0

#undef HAVE_SIGPIPE
#define HAVE_SIGPIPE 1

#undef BGL_HAVE_GETGROUPS
#define BGL_HAVE_GETGROUPS 1

#undef BGL_HAVE_IOCTL
#define BGL_HAVE_IOCTL 1

#undef BGL_HAVE_SEMAPHORE
#define BGL_HAVE_SEMAPHORE 1

#undef BGL_HAVE_SYSLOG
#define BGL_HAVE_SYSLOG 1

#undef BGL_HAVE_OVERFLOW
#define BGL_HAVE_OVERFLOW 1

#undef HAVE_SIGSETMASK
#define HAVE_SIGSETMASK 1

#undef HAVE_SIGPROCMASK
#define HAVE_SIGPROCMASK 1

#undef BGL_HAVE_ALLOCA
#define BGL_HAVE_ALLOCA 1

#undef BGL_HAVE_ALLOCAH
#define BGL_HAVE_ALLOCAH 1

#undef BGL_HAVE_C99STACKALLOC
#define BGL_HAVE_C99STACKALLOC 1

#undef HAVE_MMAP
#define HAVE_MMAP 1

#undef HAVE_GETCWD
#define HAVE_GETCWD 1

#undef HAVE_GETWD
#define HAVE_GETWD 1

#undef BGL_HAVE_LIKELY
#define BGL_HAVE_LIKELY 1

#undef BGL_HAVE_GCCATTRS
#define BGL_HAVE_GCCATTRS 1

#undef CONSTANT_ALIGNED
#define CONSTANT_ALIGNED 1

#undef BGL_BIG_ENDIAN
#define BGL_BIG_ENDIAN 0

#undef HAVE_PIPE
#define HAVE_PIPE 1

#undef BGL_HAVE_SYMLINK
#define BGL_HAVE_SYMLINK 1

#define BGL_HAVE_LABS 1
#define BGL_LABS labs

#undef BGL_LONGLONG_T
#define BGL_LONGLONG_T long long
#define BGL_LONG_LIMBS 1
#define BGL_LONGLONG_LIMBS 1
#define C_LONG_SIGN_BIT 0x1000000000000000L
#define C_ELONG_SIGN_BIT 0x8000000000000000L
#define C_LLONG_SIGN_BIT 0x8000000000000000LL
#define BGL_HAVE_LLABS 1
#define BGL_HAVE_LONGLONG 1
#define BGL_LLABS llabs
#define BGL_STRTOL(_1,_2,_3) (strtol(_1+_2,0,_3))
#define BGL_STRTOUL(_1,_2,_3) (strtoul(_1,(char **)_2,_3))
#define BGL_HAVE_STRTOLL 1
#define BGL_STRTOLL(_1,_2,_3) (strtoll(_1,(char **)_2,_3))
#define BGL_HAVE_STRTOULL 1
#define BGL_STRTOULL(_1,_2,_3) (strtoull(_1,(char **)_2,_3))

#undef BGL_HAVE_GMP
#define BGL_HAVE_GMP 1

#undef BGL_HAVE_RESOLV
#define BGL_HAVE_RESOLV 1

#undef BGL_HAVE_RESOLV_ZXFR
#define BGL_HAVE_RESOLV_ZXFR 0

#undef BGL_HAVE_RESOLV_APL
#define BGL_HAVE_RESOLV_APL 1

#undef BGL_HAVE_UNISTRING
#define BGL_HAVE_UNISTRING 1

#undef BGL_HAVE_BACKTRACE
#define BGL_HAVE_BACKTRACE 1

#undef HAVE_SIGCHLD
#define HAVE_SIGCHLD 1

#undef BGL_HAVE_SIGACTION
#define BGL_HAVE_SIGACTION 1

#undef BGL_HAVE_SIGINFO
#define BGL_HAVE_SIGINFO 1

#undef BGL_HAVE_GETRLIMIT
#define BGL_HAVE_GETRLIMIT 1

#undef HAVE_SETENV
#define HAVE_SETENV 1

#undef BGL_HAVE_SELECT
#define BGL_HAVE_SELECT 1

#undef BGL_HAVE_FCNTL
#define BGL_HAVE_FCNTL 1

#undef BGL_HAVE_LOCKF
#define BGL_HAVE_LOCKF 1

#undef HAVE_TERMIO
#define HAVE_TERMIO 1

#undef HAVE_TERMIOS
#define HAVE_TERMIOS 1

#undef POSIX_FILE_OPS
#define POSIX_FILE_OPS 1

#define BGL_SENDFILE_NO 1
#define BGL_SENDFILE_LINUX 2
#define BGL_SENDFILE_BSD 3

#undef BGL_SENDFILE_BRAND
#define BGL_SENDFILE_BRAND BGL_SENDFILE_LINUX

#undef BGL_HAVE_SENDFILE
#define BGL_HAVE_SENDFILE (BGL_SENDFILE_BRAND != BGL_SENDFILE_NO)

#undef BGL_SENDFILE_REQUIRE_INPUT_FILE
#define BGL_SENDFILE_REQUIRE_INPUT_FILE 1

#undef BGL_SENDFILE_REQUIRE_OUTPUT_SOCKET
#define BGL_SENDFILE_REQUIRE_OUTPUT_SOCKET 1

#undef BGL_NANOSLEEP
#define BGL_NANOSLEEP 1

#undef BGL_SLEEP
#define BGL_SLEEP 1

#undef BGL_TIMEZONE
#define BGL_TIMEZONE timezone

#undef BGL_HAVE_TIMEVAL
#define BGL_HAVE_TIMEVAL 1

#undef BGL_HAVE_GMTOFF
#define BGL_HAVE_GMTOFF 1

#undef BGL_HAVE_LOCALTIME_R
#define BGL_HAVE_LOCALTIME_R 1

#undef BGL_HAVE_GMTIME_R
#define BGL_HAVE_GMTIME_R 1

#undef BGL_HAVE_TIMEGM
#define BGL_HAVE_TIMEGM 1

#undef HAVE_SHARED_LIBRARY
#define HAVE_SHARED_LIBRARY 1

#undef HAVE_DLOPEN
#define HAVE_DLOPEN 1

#define DLOPEN_LD_OPT "-ldl"

#undef BGL_HAVE_SOCKLEN
#define BGL_HAVE_SOCKLEN 1

#undef BGL_HAVE_SOCKET_TCP_NODELAY
#define BGL_HAVE_SOCKET_TCP_NODELAY 1

#undef BGL_HAVE_SOCKET_TCP_CORK
#define BGL_HAVE_SOCKET_TCP_CORK 1

#undef BGL_HAVE_SOCKET_TCP_QUICKACK
#define BGL_HAVE_SOCKET_TCP_QUICKACK 1

#undef BGL_HAVE_INET_ATON
#define BGL_HAVE_INET_ATON 1

#undef BGL_HAVE_INET_PTON
#define BGL_HAVE_INET_PTON 1

#undef BGL_HAVE_UNIX_SOCKET
#define BGL_HAVE_UNIX_SOCKET 1

#undef BGL_HAVE_GETADDRINFO
#define BGL_HAVE_GETADDRINFO 1

#undef BGL_HAVE_GETIFADDRS
#define BGL_HAVE_GETIFADDRS 1

#undef BGL_HAVE_GETHWADDRS
#define BGL_HAVE_GETHWADDRS 1

#undef BGL_AUTO_FINALIZER
#define BGL_AUTO_FINALIZER 0

#undef SHELL
#define SHELL "/bin/sh"

#undef BGL_SHELL_MV
#define BGL_SHELL_MV "mv"

#undef BGL_SHELL_RM
#define BGL_SHELL_RM "rm"

#undef C_COMPILER_STYLE
#define C_COMPILER_STYLE "gcc"

#undef C_COMPILER
#define C_COMPILER "gcc"

#undef C_LD
#define C_LD "gcc"

#undef C_FLAGS
#define C_FLAGS "  -Wno-unused-value -Wno-parentheses-equality -Wno-parentheses -Wno-invalid-source-encoding -Wno-return-type -Wno-trigraphs"

#undef C_PICFLAGS
#define C_PICFLAGS "-fPIC"

#undef C_NANFLAGS
#define C_NANFLAGS "-DBGL_NAN_TAGGING=0"

#undef C_COMPILER_O_OPTION
#define C_COMPILER_O_OPTION "-o "

#undef C_COMPILER_FP_OPTION
#define C_COMPILER_FP_OPTION "-fno-omit-frame-pointer"

#undef C_COMPILER_DEBUG_OPTION
#define C_COMPILER_DEBUG_OPTION "-g"

#undef C_OBJECT_FILE_EXTENSION
#define C_OBJECT_FILE_EXTENSION "o"

#undef C_COMPILER_OPTIM_FLAGS
#define C_COMPILER_OPTIM_FLAGS "-O3"

#undef C_COMPILER_RPATH
#define C_COMPILER_RPATH "-Wl,--enable-new-dtags,-rpath=~a"

#undef C_STRIP_FLAGS
#define C_STRIP_FLAGS "-s"

#undef C_LINKER_STYLE
#define C_LINKER_STYLE "gcc"

#undef C_LINKER_O_OPTION
#define C_LINKER_O_OPTION "-o "

#undef C_LINKER_FLAGS
#define C_LINKER_FLAGS " "

#undef C_LINKER_DEBUG_OPTION
#define C_LINKER_DEBUG_OPTION "-g "

#undef C_LINKER_OPTIM_FLAGS
#define C_LINKER_OPTIM_FLAGS ""

#undef C_LINKER_SONAME_OPTION
#define C_LINKER_SONAME_OPTION "-Wl,-soname=~a"

#undef C_LINKER_SHARED_OPTION
#define C_LINKER_SHARED_OPTION "-shared"

#undef C_PROFILE_FLAGS
#define C_PROFILE_FLAGS "-pg -fno-inline   -Wno-unused-value -Wno-parentheses-equality -Wno-parentheses -Wno-invalid-source-encoding -Wno-return-type -Wno-trigraphs"

#undef C_STRING_SPLIT
#define C_STRING_SPLIT 0

#undef BGL_LD_LIBRARY_DIR
#define BGL_LD_LIBRARY_DIR "/opt/yunibase/current/bigloo/lib"

#undef LIBRARY_DIRECTORY
#define LIBRARY_DIRECTORY "/opt/yunibase/current/bigloo/lib/bigloo/4.5b"

#undef BGL_NON_CUSTOM_GC_DIR
#define BGL_NON_CUSTOM_GC_DIR ""

#undef BGL_GC_LIBRARY
#define BGL_GC_LIBRARY "bigloogc"

#undef BGL_GC_CUSTOM
#define BGL_GC_CUSTOM 1

#undef ZIP_DIRECTORY
#define ZIP_DIRECTORY "/opt/yunibase/current/bigloo/lib/bigloo/4.5b"

#undef DLL_DIRECTORY
#define DLL_DIRECTORY "/opt/yunibase/current/bigloo/lib/bigloo/4.5b"

#undef USER_LIBRARIES
#define USER_LIBRARIES "-ldl -lresolv -lunistring -lpcre2-8 -lgmp -lm -lc"

#undef C_BEAUTIFIER
#define C_BEAUTIFIER ""

#undef DIRNAME_CMD
#define DIRNAME_CMD "dirname"

#undef LIBRARY_BASE_NAME
#define LIBRARY_BASE_NAME "bigloo"

#undef DOUBLE_PRECISION
#define DOUBLE_PRECISION 14

#undef ADDITIONAL_STATIC_LINK_OPTION
#define ADDITIONAL_STATIC_LINK_OPTION ""

#undef ADDITIONAL_SHARED_LINK_OPTION
#define ADDITIONAL_SHARED_LINK_OPTION ""

#undef BGL_HAVE_BIGLOO_ABORT
#define BGL_HAVE_BIGLOO_ABORT 0

#undef BGL_HEAP_DEBUG_COPT
#define BGL_HEAP_DEBUG_COPT ""

#undef BGL_HAVE_BDB
#define BGL_HAVE_BDB 0

#undef BGL_JAVA
#define BGL_JAVA "java"

#undef BGL_JAVA_OPT
#define BGL_JAVA_OPT ""

#undef BGL_JAVA_VOPT
#define BGL_JAVA_VOPT ""

#undef BGL_JAVA_SHELL
#define BGL_JAVA_SHELL "sh"

#undef BGL_JAR
#define BGL_JAR ""

#undef BGL_IMPORT
#define BGL_IMPORT extern

#undef BGL_EXPORTED_DECL
#define BGL_EXPORTED_DECL extern

#undef BGL_EXPORTED_DEF
#define BGL_EXPORTED_DEF 

#undef BGL_RUNTIME_DECL
#define BGL_RUNTIME_DECL extern

#undef BGL_RUNTIME_DEF
#define BGL_RUNTIME_DEF 

#define OS_CLASS "unix"
#define OS_NAME "linux"
#define OS_ARCH "x86_64"
#define OS_VERSION "5.15.0-67-generic"
#define OS_TMP "/tmp"
#define OS_CHARSET "C"
#define FILE_SEPARATOR '/'
#define PATH_SEPARATOR ':'
#define STATIC_LIB_SUFFIX "a"
#define SHARED_LIB_SUFFIX "so"
#define UCS2_DISPLAYABLE 0

#define BGL_INLINE_MUTEX 1

#define BGL_HAVE_MUTEX_TIMEDLOCK 1
#define BGL_HAVE_PTHREAD_CANCEL 1
#define BGL_HAVE_SPINLOCK 1
#define BGL_HAVE_MUTEX_RECURSIVE 1
#define BGL_POSIX_CONDV_TIMEDWAIT 1

#define BGL_HAVE_PTHREAD_TIMEDJOIN 1

#define BGL_PTHREAD_TERM_SIG 15

#define BGL_HAS_THREAD_LOCALSTORAGE 1

#define BGL_THREAD_DECL __thread

#define BGL_HAVE_MIXER 1

#define BGL_REGEXP_regex 1
#define BGL_REGEXP_pregexp 2
#define BGL_REGEXP_pcre 3

#define BGL_REGEXP_TYPE BGL_REGEXP_pcre2
#define BGL_REGEXP_FAMILY "pcre2"
#define BGL_REGEXP_HAS_FREE_STUDY 0

#define BGL_CLASS_DISPLAY_MIN_SIZE 6

#define BGL_NO_GC 1
#define BGL_BOEHM_GC 2
#define BGL_SAW_GC 3

#ifndef BGL_GC
#  define BGL_GC BGL_BOEHM_GC
#endif

#ifndef BGL_GC_BUMP_ALLOC
#  define BGL_GC_BUMP_ALLOC 0
#endif

#define BGL_GC_HAVE_BLOCKING 0
#define BGL_GC_HAVE_DO_BLOCKING 1

#if( BGL_GC_BUMP_ALLOC && defined( BGL_GC_THREADS ) && !BGL_HAS_THREAD_LOCALSTORAGE )
#  undef BGL_GC_BUMP_ALLOC
#  define BGL_GC_BUMP_ALLOC 0
#endif

/*---------------------------------------------------------------------*/
/*    DNS caching                                                      */
/*---------------------------------------------------------------------*/
#define BGL_DNS_CACHE 1

/*---------------------------------------------------------------------*/
/*    Machine dependent declarations                                   */
/*---------------------------------------------------------------------*/
#if( !defined( SIGBUS ) && defined( SIGSEGV ) )
#  define SIGBUS SIGSEGV
#endif

#if defined( sony_news )
#  include <news/machparam.h>
#endif

#if defined( _MSC_VER ) || defined( _MINGW_VER )
#  include <direct.h>
#  include <io.h>
#  include <string.h>
#  define chdir _chdir
/* !!!!! PROBABLY NOT PORTABLE !!!!! */
#  define chmod _chmod                            
#  define execl _execl
#  define getcwd _getcwd
/* SECOND ARG = UNIX MODE to make directory 0700 --> OWNER RWX GROUP --- OTHERS --- */
#  define mkdir( a, b ) _mkdir( (a) )
#  define rmdir _rmdir
#endif

#if defined( _MSC_VER )
#  define strtoll strtol
#  define strtoull strtoul
#  define alloca _alloca
#endif

#if defined( _MINGW_VER )
#  include <malloc.h>
#  include <process.h>
#endif

/*---------------------------------------------------------------------*/
/*    bcopy                                                            */
/*---------------------------------------------------------------------*/
#if( !HAVE_BCOPY )
#  define bcopy( _src_, _dst_, _len_ ) memcpy( _dst_, _src_, _len_ )
#  define bzero( _dest_, _len_ ) memset( _dest_, 0, _len_ )
#endif

/*---------------------------------------------------------------------*/
/*    SIGPIPE                                                          */
/*---------------------------------------------------------------------*/
#if( !HAVE_SIGPIPE )
#  define SIGPIPE -1
#endif
   
/*---------------------------------------------------------------------*/
/*    sigsetmask                                                       */
/*---------------------------------------------------------------------*/
#if( HAVE_SIGPROCMASK )
BGL_RUNTIME_DECL int bgl_sigsetmask( int );
#  define BGL_SIGSETMASK( _int_ ) bgl_sigsetmask( _int_ )
#else
#  if( HAVE_SIGSETMASK )
#    define BGL_SIGSETMASK( _int_ ) sigsetmask( _int_ )
#  else
#    define BGL_SIGSETMASK( _int_ )
#  endif
#endif

/*---------------------------------------------------------------------*/
/*    getcwd                                                           */
/*---------------------------------------------------------------------*/
#if( !HAVE_GETCWD )
#  if( HAVE_GETWD )
#    define getcwd( _path_, _int_ ) getwd( _path_ )
#  else
#    define getcwd( _path_, _int_ ) ((char *)getenv( "PWD" ))
#  endif
#endif

/*---------------------------------------------------------------------*/
/*    likely                                                           */
/*---------------------------------------------------------------------*/
#if( !BGL_HAVE_LIKELY )
#  define BGL_LIKELY(x) (x)
#  define BGL_UNLIKELY(x) (x)
#else
#  define BGL_LIKELY(x) __builtin_expect( x, 1 )
#  define BGL_UNLIKELY(x) __builtin_expect( x, 0 )
#endif

/*---------------------------------------------------------------------*/
/*    gccattrs                                                         */
/*---------------------------------------------------------------------*/
#if ( !BGL_HAVE_GCCATTRS )
#  define BGL_NOINLINE
#else
#  define BGL_NOINLINE __attribute__((noinline))
#endif

/*---------------------------------------------------------------------*/
/*    The allocations.                                                 */
/*---------------------------------------------------------------------*/
#if( !BGL_HAVE_ALLOCA )
#  define alloca( sz ) GC_MALLOC( sz )
#else
#  if BGL_HAVE_ALLOCAH
#    include <alloca.h>
#  endif
#endif

#if( !defined( __GNUC__ ) || (BGL_GC != BGL_BOEHM_GC && BGL_GC != BGL_SAW_GC) )
extern union scmobj * an_object;
#  define AN_OBJECT union scmobj *an_object;
#else
#  define AN_OBJECT 
#endif

#if( !defined( BGL_FUNCTION_BEGIN) )
#  define BGL_FUNCTION_BEGIN AN_OBJECT
#endif
#if( !defined( BGL_FUNCTION_END ) )
#  define BGL_FUNCTION_END
#endif

/*---------------------------------------------------------------------*/
/*    SETJMP                                                           */
/*---------------------------------------------------------------------*/
#define SIGSETJMP_SAVESIGS 0

#define jmp_buf_t sigjmp_buf

#undef SETJMP
#define SETJMP(__jb) sigsetjmp(__jb,SIGSETJMP_SAVESIGS)
#undef LONGJMP
#define LONGJMP siglongjmp

/*---------------------------------------------------------------------*/
/*    BGL_ECONNEREST                                                   */
/*---------------------------------------------------------------------*/
#if( defined( WSAECONNRESET ) )
#  define BGL_ECONNRESET WSAECONNRESET
#else
#  define BGL_ECONNRESET ECONNRESET
#endif

/*---------------------------------------------------------------------*/
/*    BGL_UTF8_LOCALE_COMPARE3 ...                                     */
/*---------------------------------------------------------------------*/
#if( BGL_HAVE_UNISTRING )
#  define BGL_UTF8_STRING_LOCALE_COMPARE3( s1, s2 ) \
   bgl_strcoll( s1, s2 )
#else
#  define BGL_UTF8_STRING_LOCALE_COMPARE3( s1, s2 ) \
   strcoll( BSTRING_TO_STRING( s1 ), BSTRING_TO_STRING( s2 ) )
#endif

/*---------------------------------------------------------------------*/
/*    regex include                                                    */
/*---------------------------------------------------------------------*/
#if( BGL_HAVE_REGEX )
#  include <regex.h>
#endif   
   
/*---------------------------------------------------------------------*/
/*    GMP include                                                      */
/*---------------------------------------------------------------------*/
#if( BGL_HAVE_GMP )
#  include <gmp.h>
#endif   

/*---------------------------------------------------------------------*/
/*    stdint                                                           */
/*---------------------------------------------------------------------*/
#if( BGL_HAVE_STDINT )
#  include <stdint.h>
#else
#  if( BGL_HAVE_UNISTDINT )
#    include <unistdint.h>
#  else
#    if( !BGL_HAVE_STDLIBINT && !BGL_HAVE_UNISTDINT )
/* int8 */   
typedef signed char int8_t;
typedef unsigned char uint8_t;
   
/* int16 */   
#      if( sizeof( short ) >= 2 )
typedef short int16_t;
typedef unsigned short uint16_t;
#      else   
typedef int int16_t;
typedef unsigned int uint16_t;
#      endif
   
/* int32 */   
#      if( sizeof( int ) == 4 )
typedef int int32_t;
typedef unsigned int uint32_t;
#      else
typedef long int32_t;
typedef unsigned long uint32_t;
#      endif
   
/* int64 */   
#      if( sizeof( long ) == 8 )
typedef long int64_t;
typedef unsigned long uint64_t;
#      else   
#        if( BGL_HAVE_LONGLONG )    
typedef long long int64_t;
typedef unsigned long long uint64_t;
#        else
/* MS: don't know what to use on hosts that lack 64bit int */   
typedef long int64_t;
typedef unsigned long uint64_t;
#        endif
#      endif   
#    endif
#  endif   
#endif

/*---------------------------------------------------------------------*/
/*    LONG LONG constants                                              */
/*---------------------------------------------------------------------*/
#if BGL_HAVE_LONGLONG
#  if defined( LLONG_MAX )
#    define BGL_LONGLONG_MAX LLONG_MAX
#  else
#    define BGL_LONGLONG_MAX ((BGL_LONGLONG_T) (~0ULL >> 1))
#  endif
#  if defined( LLONG_MIN )
#    define BGL_LONGLONG_MIN LLONG_MIN
#  else
#    define BGL_LONGLONG_MIN ((BGL_LONGLONG_T) ((~0ULL >> 1) + 1ULL))
#  endif
#else
#  define BGL_LONGLONG_MAX LONG_MAX
#  define BGL_LONGLONG_MIN LONG_MIN
#endif

/*---------------------------------------------------------------------*/
/*    LOCK                                                             */
/*---------------------------------------------------------------------*/
#if !BGL_HAVE_LOCKF
#  define F_LOCK 0
#  define F_TLOCK 0
#  define F_ULOCK 0
#  define F_TEST 0
#endif

/*---------------------------------------------------------------------*/
/*    syslog                                                           */
/*---------------------------------------------------------------------*/
#if( BGL_HAVE_SYSLOG )
#  include <syslog.h>
#endif

#endif
