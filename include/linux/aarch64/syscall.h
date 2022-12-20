/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2018,2022 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2020 Danny Milosavljevic <dannym@scratchpost.org>
 * Copyright © 2022 Jacob McSwain <jacob@mcswain.dev>
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
#ifndef __MES_LINUX_AARCH64_SYSCALL_H
#define __MES_LINUX_AARCH64_SYSCALL_H 1

/* libc-mini */
#ifndef SYS_exit
// CONSTANT SYS_exit 0x5d
#define SYS_exit    0x5d
#endif
#ifndef SYS_write
// CONSTANT SYS_write 0x40
#define SYS_write   0x40
#endif

/* libc */
// CONSTANT SYS_fork 0x02
#define SYS_fork    0x02
// CONSTANT SYS_read 0x03
#define SYS_read    0x03
// CONSTANT SYS_open 0x05
#define SYS_open    0x05
// #define SYS_waitpid
// CONSTANT SYS_wait4 0x104
#define SYS_wait4   0x104
// CONSTANT SYS_execve 0xdd
#define SYS_execve  0xdd
// #define SYS_chmod
// CONSTANT SYS_fchmodat 0x35
#define SYS_fchmodat  0x35
// #define SYS_access
// CONSTANT SYS_faccessat 0x30
#define SYS_faccessat  0x30
// CONSTANT SYS_brk 0xd6
#define SYS_brk     0xd6
// CONSTANT SYS_ioctl 0x1d
#define SYS_ioctl   0x1d
// CONSTANT SYS_fsync 0x52
#define SYS_fsync   0x52
// CONSTANT SYS_getcwd 0x11
#define SYS_getcwd  0x11
// CONSTANT SYS_dup 0x17
#define SYS_dup     0x17
// #define SYS_dup2
// CONSTANT SYS_dup3 0x18
#define SYS_dup3     0x18
// #define SYS_unlink
// CONSTANT SYS_unlinkat 0x23
#define SYS_unlinkat  0x23
// CONSTANT SYS_gettimeofday 0xa9
#define SYS_gettimeofday 0xa9
// CONSTANT SYS_clock_gettime 0x71
#define SYS_clock_gettime 0x71

/* libc+tcc */
#define SYS_close  0x39
#define SYS_lseek  0x3e
// #define SYS_rmdir
// #define SYS_stat

/* libc+gnu */
#define SYS_chdir     0x31  
// #define SYS_link
#define SYS_linkat    0x25
#define SYS_getpid    0xac
#define SYS_getuid    0xae
#define SYS_kill      0x51
// #define SYS_rename
#define SYS_renameat  0x26
// #define SYS_mkdir
#define SYS_mkdirat   0x22
// #define SYS_pipe
#define SYS_pipe2     0x3b
#define SYS_getgid    0xb0
#define SYS_rt_sigaction 0x86
#define SYS_rt_sigreturn 0x8b
#define SYS_fcntl     0x19
#define SYS_getrusage 0xa5
// #define SYS_lstat
#define SYS_setitimer 0x67
#define SYS_fstat 0x50
#define SYS_nanosleep 0x65
// #define SYS_getdents
#define SYS_getdents64 0x3d

/* bash */
#define SYS_setuid   0x92
#define SYS_setgid   0x90
#define SYS_geteuid  0xaf
#define SYS_getegid  0xb1
#define SYS_getppid  0xad

/* make+WITH_GLIBC */
#define SYS_rt_sigprocmask 0x87

/* tar */
// #define SYS_symlink
#define SYS_symlinkat  0x24
// #define SYS_readlink
#define SYS_readlinkat 0x4e
// #define SYS_mknod
#define SYS_mknodat    0x21

#endif /* __MES_LINUX_AARCH64_SYSCALL_H */
