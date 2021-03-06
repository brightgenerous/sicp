(print "----------------")

;; -----

(print "--")
(print "問題3.10")

;
; 大域環境
; +-------------------------------------------------------------------------+
; | make-withdraw: ...                                                      |
; | W2: -------------------------------------+                              |
; | W1: +                                    |                              |
; +-----+------------------------------------+------------------------------+
;       |                  ^                 |                  ^
;       |        E1        |                 |        E2        |
;       |        +---------------------+     |        +---------------------+
;       |        |                     |     |        |                     |
;       |        | initial-amount: 100 |     |        | initial-amount: 100 |
;       |        |                     |     |        |                     |
;       |        +---------------------+     |        +---------------------+
;       |                  ^                 |                  ^
;       v           E1-1   |                 v           E2-1   |
;   +---+---+-+     +--------------+     +---+---+-+     +--------------+
;   |   |     |     |              |     |   |     |     |              |
;   | ○ | ○ + + --> | balance: 100 |     | ○ | ○ + + --> | balance: 100 |
;   | | |     |     |              |     | | |     |     |              |
;   +-+-------+     +--------------+     +-+-------+     +--------------+
;     |                                      |
;     |   +----------------------------------+
;     |   |
;     v   v
;   パラメタ: amount
;


(print "--")
(print "問題3.11")

;
; 大域環境
; +----------------------------------------------------------------------------------------------------------+
; | make-account: ...                                                                                        |
; | acc2:                                                                                                    |
; | acc: +--------+                                                                                          |
; +------+--------+------------------------------------------------------------------------------------------+
;        |        |                                              ^
;        |        |           E1                                 |                                      E2...
;        |        |           +---------------------------------------------------------------------+   +---
;        |        |           | balance: 50                                                         |   |
;        |        |           | withdraw:                                                           |   |
;        |        |           | deposit:                                                            |   |
;        |        |           | dispatch:                                                           |   |
;        |        |           +---------------------------------------------------------------------+   +---
;        |        |                  ^                 ^                  ^                  ^
;        |        |           E1-1   |          E1-2   |           E1-3   |           E1-4   |
;        |        |           +-------------+   +--------------+   +--------------+   +-------------+
;        |        |           |             |   |              |   |              |   |             |
;        |        |           | m: 'deposit |   | amount: 40   |   | m: 'withdraw |   | amount: 60  |
;        |        |           |             |   |              |   |              |   |             |
;        |        |           +-------------+   +--------------+   +--------------+   +-------------+
;        |        |                  ^                 ^                  ^                  ^
;        |        v                  |                 |                  |                  |
;        |    +---+---+-+            |          deposit                   |           withdraw
;        |    |   |     |            |            amount: 40              |             amount: 60
;        |    | ○ | ○ + + -----------+                                    |
;        |    | | |     |                                                 |
;        |    +-+-------+                                                 |
;        |      |                                                         |
;        |      v                                                         |
;        |    パラメタ: 'deposit                                          |
;        v                                                                |
;    +---+---+-+                                                          |
;    |   |     |                                                          |
;    | ○ | ○ + + ---------------------------------------------------------+
;    | | |     |
;    +-+-------+
;      |
;      v
;    パラメタ: 'withdraw
;
