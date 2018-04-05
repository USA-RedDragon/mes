/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018 Jan Nieuwenhuizen <janneke@gnu.org>
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

#include <fcntl.h>
#include <unistd.h>

int
ungetchar (int c)
{
  return ungetc (c, g_stdin);
}

int
peekchar ()
{
  int c = getchar ();
  ungetchar (c);
  return c;
}

SCM
peek_byte ()
{
  return MAKE_NUMBER (peekchar ());
}

SCM
read_byte ()
{
  return MAKE_NUMBER (getchar ());
}

SCM
unread_byte (SCM i)
{
  ungetchar (VALUE (i));
  return i;
}

SCM
peek_char ()
{
  return MAKE_CHAR (peekchar ());
}

SCM
read_char ()
{
  return MAKE_CHAR (getchar ());
}

SCM
unread_char (SCM i)
{
  ungetchar (VALUE (i));
  return i;
}

SCM
write_char (SCM i) ///((arity . n))
{
  write_byte (i);
  return i;
}

SCM
read_string ()
{
  SCM lst = cell_nil;
  SCM c = read_char ();
  while (VALUE (c) != -1)
    {
      lst = append2 (lst, cons (c, cell_nil));
      c = read_char ();
    }
  return MAKE_STRING (lst);
}

SCM
write_byte (SCM x) ///((arity . n))
{
  SCM c = car (x);
  SCM p = cdr (x);
  int fd = g_stdout;
  if (TYPE (p) == TPAIR && TYPE (car (p)) == TNUMBER && VALUE (CAR (p)) != 1)
    fd = VALUE (CAR (p));
  char cc = VALUE (c);
  write (fd, (char*)&cc, 1);
#if !__MESC__
  assert (TYPE (c) == TNUMBER || TYPE (c) == TCHAR);
#endif
  return c;
}

char string_to_cstring_buf[1024];
char const*
string_to_cstring (SCM s)
{
  //static char buf[1024];
  //char *p = buf;
  char *p = string_to_cstring_buf;
  s = STRING(s);
  while (s != cell_nil)
    {
      *p++ = VALUE (car (s));
      s = cdr (s);
    }
  *p = 0;
  //return buf;
  return string_to_cstring_buf;
}

SCM
getenv_ (SCM s) ///((name . "getenv"))
{
  char *p;
  p = getenv (string_to_cstring (s));
  return p ? MAKE_STRING (cstring_to_list (p)) : cell_f;
}

SCM
access_p (SCM file_name, SCM mode)
{
  return access (string_to_cstring (file_name), VALUE (mode)) == 0 ? cell_t : cell_f;
}

SCM
current_input_port ()
{
  return MAKE_NUMBER (g_stdin);
}

SCM
open_input_file (SCM file_name)
{
  return MAKE_NUMBER (open (string_to_cstring (file_name), O_RDONLY));
}

SCM
set_current_input_port (SCM port)
{
  int prev = g_stdin;
  g_stdin = VALUE (port) ? VALUE (port) : STDIN;
  return MAKE_NUMBER (prev);
}

SCM
current_output_port ()
{
  return MAKE_NUMBER (g_stdout);
}

SCM
open_output_file (SCM x) ///((arity . n))
{
  SCM file_name = car (x);
  x = cdr (x);
  int mode = S_IRUSR|S_IWUSR;
  if (TYPE (x) == TPAIR && TYPE (car (x)) == TNUMBER)
    mode = VALUE (car (x));
  return MAKE_NUMBER (open (string_to_cstring (file_name), O_WRONLY|O_CREAT|O_TRUNC,mode));
}

SCM
set_current_output_port (SCM port)
{
  g_stdout = VALUE (port) ? VALUE (port) : STDOUT;
  return current_output_port ();
}

SCM
force_output (SCM p) ///((arity . n))
{
  return cell_unspecified;
}
