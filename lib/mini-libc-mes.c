/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017 Jan Nieuwenhuizen <janneke@gnu.org>
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

void
exit ()
{
  asm ("mov____0x8(%ebp),%ebx !8");

  asm ("mov____$i32,%eax SYS_exit");
  asm ("int____$0x80");
}

void
write ()
{
  asm ("mov____0x8(%ebp),%ebx !8");
  asm ("mov____0x8(%ebp),%ecx !12");
  asm ("mov____0x8(%ebp),%edx !16");

  asm ("mov____$i32,%eax SYS_write");
  asm ("int____$0x80");
}

int
strlen (char const* s)
{
  int i = 0;
  while (s[i]) i++;
  return i;
}

int
eputs (char const* s)
{
  int i = strlen (s);
  write (2, s, i);
  return 0;
}

int
puts (char const* s)
{
  int i = strlen (s);
  write (1, s, i);
  return 0;
}