#! /bin/sh

# Mes --- Maxwell Equations of Software
# Copyright © 2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
#
# This file is part of Mes.
#
# Mes is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# Mes is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Mes.  If not, see <http://www.gnu.org/licenses/>.

set -ex

export HEX2=${HEX2-hex2}
export M1=${M1-M1}
export BLOOD_ELF=${BLOOD_ELF-blood-elf}
export MES_SEED=${MES_SEED-../mes-seed}
export MESCC=${MESCC-$(type -p mescc)}
[ -z "$MESCC" ] && MESCC=scripts/mescc
export MES=${MES-$(type -p mes)}
[ -z "$MES" ] && MES=src/mes

if [ -d "$MES_SEED" ]; then
    $M1 --LittleEndian --Architecture=1\
        -f stage0/x86.M1\
        -f $MES_SEED/crt1.M1\
        -o lib/crt1.hex2
    $M1 --LittleEndian --Architecture=1\
        -f stage0/x86.M1\
        -f $MES_SEED/libc-mes.M1\
        -o lib/libc-mes.hex2
    $M1 --LittleEndian --Architecture=1\
        -f stage0/x86.M1\
        -f $MES_SEED/mes.M1\
        -o src/mes.hex2
    $BLOOD_ELF\
        -f stage0/x86.M1\
        -f $MES_SEED/mes.M1\
        -f $MES_SEED/libc-mes.M1\
        -o src/mes.blood-elf.M1
    $M1 --LittleEndian --Architecture=1\
        -f src/mes.blood-elf.M1\
        -o src/mes.blood-elf.hex2
    $HEX2 --LittleEndian --Architecture=1 --BaseAddress=0x1000000\
          -f stage0/elf32-header.hex2\
          -f lib/crt1.hex2\
          -f lib/libc-mes.hex2\
          -f src/mes.hex2\
          -f src/mes.blood-elf.hex2\
          --exec_enable\
          -o src/mes.seed-out
    cp src/mes.seed-out src/mes

    $M1 --LittleEndian --Architecture=1 -f\
        stage0/x86.M1\
        -f $MES_SEED/libc+tcc-mes.M1\
        -o src/libc+tcc-mes.hex2
fi

[ -n "$SEED" ] && exit 0

export GUILE=src/mes
export MES_ARENA=${MES_ARENA-30000000}
sh build-aux/mes-snarf.scm --mes src/gc.c
sh build-aux/mes-snarf.scm --mes src/lib.c
sh build-aux/mes-snarf.scm --mes src/math.c
sh build-aux/mes-snarf.scm --mes src/mes.c
sh build-aux/mes-snarf.scm --mes src/posix.c
sh build-aux/mes-snarf.scm --mes src/reader.c
sh build-aux/mes-snarf.scm --mes src/vector.c

export PREPROCESS=1
NOLINK=1 sh build-aux/cc-mes.sh lib/crt1
NOLINK=1 sh build-aux/cc-mes.sh lib/mini-libc-mes
NOLINK=1 sh build-aux/cc-mes.sh lib/libc-mes
NOLINK=1 sh build-aux/cc-mes.sh lib/libc+tcc-mes

# sh build-aux/cc-mes.sh scaffold/main
# sh build-aux/cc-mes.sh scaffold/hello
# sh build-aux/cc-mes.sh scaffold/argv
# sh build-aux/cc-mes.sh scaffold/malloc
##sh build-aux/cc-mes.sh scaffold/micro-mes
##sh build-aux/cc-mes.sh scaffold/tiny-mes
# sh build-aux/cc-mes.sh scaffold/mini-mes

sh build-aux/cc-mes.sh src/mes
# FIXME: broken
# cp src/mes.mes-out src/mes
