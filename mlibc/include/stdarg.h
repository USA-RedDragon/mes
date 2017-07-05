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
#ifndef __MES_STDARG_H
#define __MES_STDARG_H 1

#if __GNUC__ && POSIX
#include_next <stdarg.h>
#else // ! (__GNUC__ && POSIX)
typedef int va_list;
#define va_start(ap, last) (void)((ap) = (char*)(&(last) + 4))
#define va_arg(ap, type) (((type*)((ap) = ((ap) + sizeof(type))))[-1])
#define va_end(ap) (void)((ap) = 0)
#define va_copy(dest, src) dest = src

int vprintf (char const* format, va_list ap);

#endif // ! (__GNUC__ && POSIX)

#endif // __MES_STDARG_H
