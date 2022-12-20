/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 *
 * This file is part of GNU Mes.
 *
 * GNU Mes is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 *
 * GNU Mes is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GNU Mes.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <errno.h>
#include <linux/aarch64/syscall.h>

int
__sys_call (int sys_call)
{
  asm ("mov____x8,x0");
  asm ("svc____#0");
}

int
__sys_call1 (int sys_call, int one)
{
  asm ("mov____x8,x0");
  asm ("mov____x0,x1");
  asm ("svc____#0");
}

int
__sys_call2 (int sys_call, int one, int two)
{
  asm ("mov____x8,x0");
  asm ("mov____x0,x1");
  asm ("mov____x1,x2");
  asm ("svc____#0");
}

int
__sys_call3 (int sys_call, int one, int two, int three)
{
  asm ("mov____x8,x0");
  asm ("mov____x0,x1");
  asm ("mov____x1,x2");
  asm ("mov____x2,x3");
  asm ("svc____#0");
}

int
__sys_call4 (int sys_call, int one, int two, int three, int four)
{
  asm ("mov____x8,x0");
  asm ("mov____x0,x1");
  asm ("mov____x1,x2");
  asm ("mov____x2,x3");
  asm ("mov____x3,x4");
  asm ("svc____#0");
}

int
__sys_call6 (int sys_call, int one, int two, int three, int four, int five, int six)
{
  asm ("mov____x8,x0");
  asm ("mov____x0,x1");
  asm ("mov____x1,x2");
  asm ("mov____x2,x3");
  asm ("mov____x3,x4");
  asm ("mov____x4,x5");
  asm ("mov____x5,x6");
  asm ("svc____#0");
}

int
_sys_call (int sys_call)
{
  int r = __sys_call (sys_call);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

int
_sys_call1 (int sys_call, int one)
{
  int r = __sys_call1 (sys_call, one);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

int
_sys_call2 (int sys_call, int one, int two)
{
  int r = __sys_call2 (sys_call, one, two);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

int
_sys_call3 (int sys_call, int one, int two, int three)
{
  int r = __sys_call3 (sys_call, one, two, three);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

int
_sys_call4 (int sys_call, int one, int two, int three, int four)
{
  int r = __sys_call4 (sys_call, one, two, three, four);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

int
_sys_call6 (int sys_call, int one, int two, int three, int four, int five, int six)
{
  int r = __sys_call6 (sys_call, one, two, three, four, five, six);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}
