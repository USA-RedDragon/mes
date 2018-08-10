#! /bin/sh

# GNU Mes --- Maxwell Equations of Software
# Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
#
# This file is part of GNU Mes.
#
# GNU Mes is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# GNU Mes is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Mes.  If not, see <http://www.gnu.org/licenses/>.

set -e

. ${srcdest}build-aux/config.sh
. ${srcdest}build-aux/trace.sh

arch=x86_64-mes-gcc
if [ "$CC64" = "$TCC" ]; then
    arch=x86_64-mes-tcc
    LIBC=c+tcc # tcc bug with undefined symbols
fi

if [ -n "$LIBC" ]; then
    CC64LIBS="lib/$arch/lib$LIBC.o"
fi

c=$1

if [ -z "$ARCHDIR" ]; then
    o="$c"
    d=${c%%/*}
    p="$arch-"
else
    b=${c##*/}
    d=${c%%/*}/$arch
    o="$d/$b"
fi
mkdir -p $d

trace "CC64 $c.c" $CC64\
    -c\
    $CC64_CPPFLAGS\
    $CC64_CFLAGS\
    -o "$o".${p}o\
    "${srcdest}$c".c

if [ -z "$NOLINK" ]; then
    trace "CCLD64 $c.c" $CC64\
        $CC64_CPPFLAGS\
        $CC64_CFLAGS\
        -o "$o".${p}out\
        lib/$arch/crt1.o\
        "$o".${p}o\
        $CC64LIBS
fi
