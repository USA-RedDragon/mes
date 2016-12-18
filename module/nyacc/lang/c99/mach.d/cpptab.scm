;; ./mach.d/cpptab.scm

;; Copyright (C) 2015,2016 Matthew R. Wette
;; 
;; This software is covered by the GNU GENERAL PUBLIC LICENCE, Version 3,
;; or any later version published by the Free Software Foundation.  See the
;; file COPYING included with the this distribution.

(define len-v
  #(1 1 5 1 3 1 3 1 3 1 3 1 3 1 3 3 1 3 3 3 3 1 3 3 1 3 3 1 3 3 3 1 2 2 2 2 
    2 2 1 2 2 1 1 4 3 1 3))

(define pat-v
  #(((4 . 1) (5 . 2) (6 . 3) (7 . 4) (37 . 5) (8 . 6) (9 . 7) (10 . 8) (11 
    . 9) (16 . 10) (15 . 11) (38 . 12) (39 . 13) (40 . 14) (41 . 15) (42 . 16)
    (43 . 17) (44 . 18) (45 . 19) (46 . 20) (47 . 21) (48 . 22) (49 . 23) (50
    . 24)) ((4 . 1) (5 . 2) (6 . 3) (7 . 4) (37 . 5) (8 . 6) (9 . 7) (10 . 8)
    (11 . 9) (16 . 10) (15 . 11) (38 . 12) (39 . 13) (40 . 14) (41 . 15) (42 
    . 16) (43 . 17) (44 . 18) (45 . 19) (46 . 20) (47 . 21) (48 . 22) (49 . 23
    ) (50 . 53) (36 . 54)) ((4 . 52)) ((-1 . -42)) ((-1 . -41)) ((-1 . -38)) (
    (4 . 1) (5 . 2) (6 . 3) (7 . 4) (37 . 5) (8 . 6) (9 . 7) (10 . 8) (11 . 9)
    (16 . 10) (15 . 11) (38 . 12) (39 . 51)) ((4 . 1) (5 . 2) (6 . 3) (7 . 4)
    (37 . 5) (8 . 6) (9 . 7) (10 . 8) (11 . 9) (16 . 10) (15 . 11) (38 . 12) 
    (39 . 50)) ((4 . 1) (5 . 2) (6 . 3) (7 . 4) (37 . 5) (8 . 6) (9 . 7) (10 
    . 8) (11 . 9) (16 . 10) (15 . 11) (38 . 12) (39 . 49)) ((4 . 1) (5 . 2) (6
    . 3) (7 . 4) (37 . 5) (8 . 6) (9 . 7) (10 . 8) (11 . 9) (16 . 10) (15 . 
    11) (38 . 12) (39 . 48)) ((4 . 1) (5 . 2) (6 . 3) (7 . 4) (37 . 5) (8 . 6)
    (9 . 7) (10 . 8) (11 . 9) (16 . 10) (15 . 11) (38 . 12) (39 . 47)) ((4 . 
    1) (5 . 2) (6 . 3) (7 . 4) (37 . 5) (8 . 6) (9 . 7) (10 . 8) (11 . 9) (16 
    . 10) (15 . 11) (38 . 12) (39 . 46)) ((9 . 44) (8 . 45) (-1 . -31)) ((-1 
    . -27)) ((14 . 41) (13 . 42) (12 . 43) (-1 . -24)) ((16 . 39) (15 . 40) (
    -1 . -21)) ((18 . 37) (17 . 38) (-1 . -16)) ((22 . 33) (21 . 34) (20 . 35)
    (19 . 36) (-1 . -13)) ((24 . 31) (23 . 32) (-1 . -11)) ((25 . 30) (-1 . 
    -9)) ((26 . 29) (-1 . -7)) ((27 . 28) (-1 . -5)) ((28 . 27) (-1 . -3)) ((
    31 . 25) (29 . 26) (2 . -1) (1 . -1) (35 . -1)) ((35 . 0)) ((4 . 1) (5 . 2
    ) (6 . 3) (7 . 4) (37 . 5) (8 . 6) (9 . 7) (10 . 8) (11 . 9) (16 . 10) (15
    . 11) (38 . 12) (39 . 13) (40 . 14) (41 . 15) (42 . 16) (43 . 17) (44 . 
    18) (45 . 19) (46 . 20) (47 . 21) (48 . 22) (49 . 76)) ((4 . 1) (5 . 2) (6
    . 3) (7 . 4) (37 . 5) (8 . 6) (9 . 7) (10 . 8) (11 . 9) (16 . 10) (15 . 
    11) (38 . 12) (39 . 13) (40 . 14) (41 . 15) (42 . 16) (43 . 17) (44 . 18) 
    (45 . 19) (46 . 20) (47 . 21) (48 . 75)) ((4 . 1) (5 . 2) (6 . 3) (7 . 4) 
    (37 . 5) (8 . 6) (9 . 7) (10 . 8) (11 . 9) (16 . 10) (15 . 11) (38 . 12) (
    39 . 13) (40 . 14) (41 . 15) (42 . 16) (43 . 17) (44 . 18) (45 . 19) (46 
    . 20) (47 . 74)) ((4 . 1) (5 . 2) (6 . 3) (7 . 4) (37 . 5) (8 . 6) (9 . 7)
    (10 . 8) (11 . 9) (16 . 10) (15 . 11) (38 . 12) (39 . 13) (40 . 14) (41 
    . 15) (42 . 16) (43 . 17) (44 . 18) (45 . 19) (46 . 73)) ((4 . 1) (5 . 2) 
    (6 . 3) (7 . 4) (37 . 5) (8 . 6) (9 . 7) (10 . 8) (11 . 9) (16 . 10) (15 
    . 11) (38 . 12) (39 . 13) (40 . 14) (41 . 15) (42 . 16) (43 . 17) (44 . 18
    ) (45 . 72)) ((4 . 1) (5 . 2) (6 . 3) (7 . 4) (37 . 5) (8 . 6) (9 . 7) (10
    . 8) (11 . 9) (16 . 10) (15 . 11) (38 . 12) (39 . 13) (40 . 14) (41 . 15)
    (42 . 16) (43 . 17) (44 . 71)) ((4 . 1) (5 . 2) (6 . 3) (7 . 4) (37 . 5) 
    (8 . 6) (9 . 7) (10 . 8) (11 . 9) (16 . 10) (15 . 11) (38 . 12) (39 . 13) 
    (40 . 14) (41 . 15) (42 . 16) (43 . 70)) ((4 . 1) (5 . 2) (6 . 3) (7 . 4) 
    (37 . 5) (8 . 6) (9 . 7) (10 . 8) (11 . 9) (16 . 10) (15 . 11) (38 . 12) (
    39 . 13) (40 . 14) (41 . 15) (42 . 16) (43 . 69)) ((4 . 1) (5 . 2) (6 . 3)
    (7 . 4) (37 . 5) (8 . 6) (9 . 7) (10 . 8) (11 . 9) (16 . 10) (15 . 11) (
    38 . 12) (39 . 13) (40 . 14) (41 . 15) (42 . 68)) ((4 . 1) (5 . 2) (6 . 3)
    (7 . 4) (37 . 5) (8 . 6) (9 . 7) (10 . 8) (11 . 9) (16 . 10) (15 . 11) (
    38 . 12) (39 . 13) (40 . 14) (41 . 15) (42 . 67)) ((4 . 1) (5 . 2) (6 . 3)
    (7 . 4) (37 . 5) (8 . 6) (9 . 7) (10 . 8) (11 . 9) (16 . 10) (15 . 11) (
    38 . 12) (39 . 13) (40 . 14) (41 . 15) (42 . 66)) ((4 . 1) (5 . 2) (6 . 3)
    (7 . 4) (37 . 5) (8 . 6) (9 . 7) (10 . 8) (11 . 9) (16 . 10) (15 . 11) (
    38 . 12) (39 . 13) (40 . 14) (41 . 15) (42 . 65)) ((4 . 1) (5 . 2) (6 . 3)
    (7 . 4) (37 . 5) (8 . 6) (9 . 7) (10 . 8) (11 . 9) (16 . 10) (15 . 11) (
    38 . 12) (39 . 13) (40 . 14) (41 . 64)) ((4 . 1) (5 . 2) (6 . 3) (7 . 4) (
    37 . 5) (8 . 6) (9 . 7) (10 . 8) (11 . 9) (16 . 10) (15 . 11) (38 . 12) (
    39 . 13) (40 . 14) (41 . 63)) ((4 . 1) (5 . 2) (6 . 3) (7 . 4) (37 . 5) (8
    . 6) (9 . 7) (10 . 8) (11 . 9) (16 . 10) (15 . 11) (38 . 12) (39 . 13) (
    40 . 62)) ((4 . 1) (5 . 2) (6 . 3) (7 . 4) (37 . 5) (8 . 6) (9 . 7) (10 . 
    8) (11 . 9) (16 . 10) (15 . 11) (38 . 12) (39 . 13) (40 . 61)) ((4 . 1) (5
    . 2) (6 . 3) (7 . 4) (37 . 5) (8 . 6) (9 . 7) (10 . 8) (11 . 9) (16 . 10)
    (15 . 11) (38 . 12) (39 . 60)) ((4 . 1) (5 . 2) (6 . 3) (7 . 4) (37 . 5) 
    (8 . 6) (9 . 7) (10 . 8) (11 . 9) (16 . 10) (15 . 11) (38 . 12) (39 . 59))
    ((4 . 1) (5 . 2) (6 . 3) (7 . 4) (37 . 5) (8 . 6) (9 . 7) (10 . 8) (11 . 
    9) (16 . 10) (15 . 11) (38 . 12) (39 . 58)) ((-1 . -39)) ((-1 . -40)) ((-1
    . -32)) ((-1 . -33)) ((-1 . -34)) ((-1 . -35)) ((-1 . -36)) ((-1 . -37)) 
    ((3 . 57)) ((2 . -45) (1 . -45)) ((2 . 55) (1 . 56)) ((-1 . -44)) ((4 . 1)
    (5 . 2) (6 . 3) (7 . 4) (37 . 5) (8 . 6) (9 . 7) (10 . 8) (11 . 9) (16 . 
    10) (15 . 11) (38 . 12) (39 . 13) (40 . 14) (41 . 15) (42 . 16) (43 . 17) 
    (44 . 18) (45 . 19) (46 . 20) (47 . 21) (48 . 22) (49 . 23) (50 . 79)) ((2
    . 78)) ((-1 . -30)) ((-1 . -29)) ((-1 . -28)) ((14 . 41) (13 . 42) (12 . 
    43) (-1 . -26)) ((14 . 41) (13 . 42) (12 . 43) (-1 . -25)) ((16 . 39) (15 
    . 40) (-1 . -23)) ((16 . 39) (15 . 40) (-1 . -22)) ((18 . 37) (17 . 38) (
    -1 . -20)) ((18 . 37) (17 . 38) (-1 . -19)) ((18 . 37) (17 . 38) (-1 . -18
    )) ((18 . 37) (17 . 38) (-1 . -17)) ((22 . 33) (21 . 34) (20 . 35) (19 . 
    36) (-1 . -15)) ((22 . 33) (21 . 34) (20 . 35) (19 . 36) (-1 . -14)) ((24 
    . 31) (23 . 32) (-1 . -12)) ((25 . 30) (-1 . -10)) ((26 . 29) (-1 . -8)) (
    (27 . 28) (-1 . -6)) ((28 . 27) (-1 . -4)) ((30 . 77) (29 . 26)) ((4 . 1) 
    (5 . 2) (6 . 3) (7 . 4) (37 . 5) (8 . 6) (9 . 7) (10 . 8) (11 . 9) (16 . 
    10) (15 . 11) (38 . 12) (39 . 13) (40 . 14) (41 . 15) (42 . 16) (43 . 17) 
    (44 . 18) (45 . 19) (46 . 20) (47 . 21) (48 . 22) (49 . 23) (50 . 80)) ((
    -1 . -43)) ((2 . -46) (1 . -46)) ((2 . -2) (1 . -2) (35 . -2))))

(define rto-v
  #(#f 50 50 49 49 48 48 47 47 46 46 45 45 44 44 44 43 43 43 43 43 42 42 42 
    41 41 41 40 40 40 40 39 39 39 39 39 39 39 38 38 38 37 37 37 37 36 36))

(define mtab
  '(("," . 1) (")" . 2) ($ident . 3) ("(" . 4) ("defined" . 5) ($chlit . 6) 
    ($fixed . 7) ("--" . 8) ("++" . 9) ("~" . 10) ("!" . 11) ("%" . 12) ("/" 
    . 13) ("*" . 14) ("-" . 15) ("+" . 16) (">>" . 17) ("<<" . 18) (">=" . 19)
    (">" . 20) ("<=" . 21) ("<" . 22) ("!=" . 23) ("==" . 24) ("&" . 25) ("^"
    . 26) ("|" . 27) ("&&" . 28) ("||" . 29) (":" . 30) ("?" . 31) (
    $code-comm . 32) ($lone-comm . 33) ($error . 34) ($end . 35)))

;;; end tables