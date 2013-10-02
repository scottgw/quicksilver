AC_INIT([libqs], [1.0], [scott.west@inf.ethz.ch])

AM_INIT_AUTOMAKE([foreign silent-rules -Wall])
dnl AM_SILENT_RULES([yes])
AM_PROG_AR

AC_CONFIG_MACRO_DIR([m4])
LT_INIT
AC_PROG_CC
AC_PROG_CXX

## BEGIN systemtap (almost) verbatim copy
AC_MSG_CHECKING([whether to include SystemTap tracing support])
AC_ARG_ENABLE([systemtap],
              [AS_HELP_STRING([--enable-systemtap],
                              [Enable SystemTap support])],
              [ENABLE_SYSTEMTAP="${enableval}"],
              [ENABLE_SYSTEMTAP='no'])
AM_CONDITIONAL([ENABLE_SYSTEMTAP], [test x$ENABLE_SYSTEMTAP = xyes])
AC_MSG_RESULT(${ENABLE_SYSTEMTAP})

if test "x${ENABLE_SYSTEMTAP}" = xyes; then
   AC_DEFINE([HAVE_SYSTEMTAP], [1], [Define as 1 if using probes.])
fi

# check for the dtrace program
AC_CHECK_PROGS(DTRACE, dtrace)
if test -z "$DTRACE"; then
  AC_MSG_ERROR([dtrace not found])
fi


# check for the sbt SystemTap headers
AC_CHECK_HEADER([sys/sdt.h],
                [SDT_H_FOUND='yes'],
                [SDT_H_FOUND='no';
                 AC_MSG_ERROR([systemtap support needs sys/sdt.h header])])

AC_ARG_WITH([tapset-install-dir],
              [AS_HELP_STRING([--with-tapset-install-dir],
                              [The absolute path where the tapset dir will be installed])],
              [if test "x${withval}" = x; then
                 ABS_TAPSET_DIR="\$(datadir)/systemtap/tapset"
               else
                 ABS_TAPSET_DIR="${withval}"
               fi], [ABS_TAPSET_DIR="\$(datadir)/systemtap/tapset"])
AC_SUBST(ABS_TAPSET_DIR)
## END systemtap (almost) verbatim copy

## Queue of queues enable/disable
AC_ARG_ENABLE([qoq],
              [AS_HELP_STRING([--disable-qoq],
                              [Disable QoQ queues (default=enabled)])],
              [ENABLE_QOQ='no'],
              [ENABLE_QOQ="${enableval}"])
                

AC_CONFIG_HEADERS([src/config.h])
AC_CONFIG_FILES([Makefile src/Makefile tests/Makefile tapset/Makefile])
AC_CONFIG_FILES([libqs.pc])

PKG_CHECK_MODULES([DEPS], [glib-2.0 >= 2.26, ck, tbb])

AC_OUTPUT
