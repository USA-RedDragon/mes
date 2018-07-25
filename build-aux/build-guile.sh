#! /bin/sh

# GNU Mes --- Maxwell Equations of Software
# Copyright © 2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

. ${srcdest}build-aux/trace.sh

export GUILE
export GUILE_AUTO_COMPILE
GUILE=${GUILE-$(command -v guile)}
GUILE_TOOLS=${GUILE_TOOLS-$(command -v guile-tools)}
GUILE_AUTO_COMPILE=0

set -e

SCM_FILES="
${srcdest}module/mes/getopt-long.scm
${srcdest}module/mes/guile.scm
${srcdest}module/mes/misc.scm
${srcdest}module/mes/test.scm
${srcdest}module/mescc/M1.scm
${srcdest}module/mescc/as.scm
${srcdest}module/mescc/bytevectors.scm
${srcdest}module/mescc/compile.scm
${srcdest}module/mescc/i386/as.scm
${srcdest}module/mescc/info.scm
${srcdest}module/mescc/mescc.scm
${srcdest}module/mescc/preprocess.scm
"

SCRIPTS="
${srcdest}build-aux/mes-snarf.scm
${srcdest}scripts/mescc
"

export host=$($GUILE -c "(display %host-type)")

abs=$srcdest
if [ "$GUILE_EFFECTIVE_VERSION" = "2.0" ]; then
    srcdest=$abs_top_srcdir/
fi

GUILE_AUTO_COMPILE=0

for i in $SCM_FILES $SCRIPTS; do
    b=$(basename $i)
    go=${i%%.scm}.go
    if [ $i -nt $go ]; then
        trace "GUILEC $b" $GUILE_TOOLS compile -L ${srcdest}module -L ${srcdest}build-aux -L ${srcdest}scripts -o $go $i
    fi
done
