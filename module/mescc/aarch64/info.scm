;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright © 2018,2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2019 Danny Milosavljevic <dannym@scratchpost.org>
;;;
;;; This file is part of GNU Mes.
;;;
;;; GNU Mes is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Mes is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Mes.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Initialize MesCC as arm compiler

;;; Code:

(define-module (mescc aarch64 info)
  #:use-module (mescc info)
  #:use-module (mescc aarch64 as)
  #:export (aarch64-info))

(define (aarch64-info)
  (make <info> #:types aarch64:type-alist #:registers aarch64:registers #:instructions aarch64:instructions))

(define aarch64:registers '("x0" "x1" "x2" "x3" "x4" "x5"))
(define aarch64:type-alist
  `(("char" . ,(make-type 'unsigned 1 #f))
    ("short" . ,(make-type 'signed 2 #f))
    ("int" . ,(make-type 'signed 4 #f))
    ("long" . ,(make-type 'signed 8 #f))
    ("default" . ,(make-type 'signed 4 #f))
    ("*" . ,(make-type 'unsigned 8 #f))
    ("long long" . ,(make-type 'signed 8 #f))
    ("long long int" . ,(make-type 'signed 8 #f))

    ("void" . ,(make-type 'void 1 #f))
    ("signed char" . ,(make-type 'signed 1 #f))
    ("unsigned char" . ,(make-type 'unsigned 1 #f))
    ("unsigned short" . ,(make-type 'unsigned 2 #f))
    ("unsigned" . ,(make-type 'unsigned 4 #f))
    ("unsigned int" . ,(make-type 'unsigned 4 #f))
    ("unsigned long" . ,(make-type 'unsigned 8 #f))
    ("unsigned long long" . ,(make-type 'unsigned 8 #f))
    ("unsigned long long int" . ,(make-type 'unsigned 8 #f))

    ("float" . ,(make-type 'float 4 #f))
    ("double" . ,(make-type 'float 8 #f))
    ("long double" . ,(make-type 'float 8 #f))

    ("short int" . ,(make-type 'signed 2 #f))
    ("unsigned short int" . ,(make-type 'unsigned 2 #f))
    ("long int" . ,(make-type 'signed 4 #f))
    ("unsigned long int" . ,(make-type 'unsigned 4 #f))))
