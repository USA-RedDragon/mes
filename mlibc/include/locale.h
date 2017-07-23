/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2017 Jan Nieuwenhuizen <janneke@gnu.org>
 *
 * This file is part of Mes.
 *
 * Mes is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 *
 * Mes is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Mes.  If not, see <http://www.gnu.org/licenses/>.
 */
#ifndef __MES_LOCALE_H
#define __MES_LOCALE_H 1

#if __GNUC__ && POSIX
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#undef __MES_LOCALE_H
#include_next <locale.h>

#else // ! (__GNUC__ && POSIX)
char* dirname (char*);
#endif // ! (__GNUC__ && POSIX)

#endif // __MES_LOCALE_H
