; 9

; 如果要表達任意數量的資料, data definition 就要借助 self-referential 表達
; 有效的 self-referential 必須包含至少兩個 clauses 和至少有一個 clause 不會引用自己

; proofs by induction

; List-of-strings -> Number
; determines how many strings are on alos
(define (how-many alos)
  (cond
    [(empty? alos) 0]
    [else
      (+ (how-many (rest alos)) 1)]))