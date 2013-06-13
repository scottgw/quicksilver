AC_INIT([libqs], [1.0], [scott.west@inf.ethz.ch])

AM_INIT_AUTOMAKE([foreign silent-rules -Wall])
AM_SILENT_RULES([yes])

AC_CONFIG_MACRO_DIR([m4])
LT_INIT
AC_PROG_CC
AC_PROG_CXX

AC_CONFIG_FILES([Makefile src/Makefile])
AC_CONFIG_FILES([libqs.pc])

PKG_CHECK_MODULES([DEPS], [glib-2.0 >= 2.26, ck])

AC_OUTPUT
