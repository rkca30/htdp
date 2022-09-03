; 8
; self-referential definitions

; 8.1

; empty list
'()

; 新增元素到 list
(cons "Mercury", '())

; cons 有兩個 feilds: first 和 rest

; first: "Mercury", rest: '()
(cons "Mercury" '())

; first: "Venus", rest: (cons "Mercury" '())
(cons "Venus"
  (cons "Mercury"
    '()))

; first: "Earth", rest: (cons "Venus" (cons Mercury '()))
(cons "Earth"
  (cons "Venus"
    (cons "Mercury"
      '())))

; Ex129

; 1
(cons "Neptune"
  (cons "Uranus"
    (cons "Saturn"
      (cons "Jupiter"
        (cons "Mars"
          (cons "Earth"
            (cons "Venus"
              (cons "Mercury"
                '()))))))))
; 2
(cons "steak"
  (cons "french fries"
    (cons "beans"
      (cons "bread"
        (cons "water"
          (cons "Brie cheese"
            (cons "ice cream"
              '())))))))

; 3
(cons "red"
  (cons "yellow"
    (cons "blue"
      '())))

; Sketch some box representations of these lists, similar to those in figures 44 and 45. Which of the sketches do you like better?

; A List-of-names is one of:
; - '()
; - (cons String List-of-names)
; interpretation a list of invitees, by last name

; Ex130

(cons "a"
  (cons "b"
    (cons "c"
      (cons "d"
        (cons "e"
          '())))))

; Sketch a box representation of the list similar to those found in figure 44.

; 滿足第二行 data definition
(cons "1" (cons "2" '()))

; 2 是數字，不是 String
(cons 2 '())

; Ex131

; A List-of-booleans is one of:
; - '()
; - (cons Boolean List-of-booleans)
; interpretation a list of booleans

; 8.2

; 當 empty 使用在 '() 才會是 true
(empty? '())

; cons 是構成兩個 field 的 constructor, first 可以是任何類型的值, rest 需要是 list

(define-struct pair [left right])
; A ConsPair is a structure
; (make-pair Any Any)

; Any Any -> ConsPair
(define (our-cons a-value a-list)
  (make-pair a-value a-list))

; A ConsOrEmpty is one of:
; - '()
; - (make-pair Any ConsOrEmpty)
; interpretation ConsOrEmpty is the class of all lists

; Any Any -> ConsOrEmpty
(define (our-cons a-value a-list)
  (cond
    [(empty? a-list) (make-pair a-value a-list)]
    [(pair? a-list) (make-pair a-value a-list)]
    [else (error "cons: second argument ...")]))

; ConsOrEmpty -> Any
; extracts the left part of the given pair
(define (our-first a-list)
  (if (empty? a-list)
      (error 'our-first "...")
      (pair-left a-list)))

; ConsOrEmpty -> Any
; extracts the rest part of the given pair
(define (our-rest a-list)
  (if (empty? a-list)
      (error 'our-first "...")
      (pair-right a-list)))

; cons 使用 Local Definitions 防止刪改資料

; List primitives

'()
empty?
cons
first
rest
cons?

; 8.3

; List-of-names -> Boolean
; determines whether "Flatt" is on a-list-of-names
(define (contains-flatt? a-list-of-names)
  #false)

(check-expect (contains-flatt? '()) #false)
(check-expect (contains-flatt? (cons "Find" '()))
              #false)
(check-expect (contains-flatt? (cons "Flatt" '()))
              #true)
(check-expect (contains-flatt?
                (cons "A" (cons "Flatt" (cons "C" '()))))
              #true)
(check-expect (contains-flatt?
                (cons "A" (cons "B" (cons "C" '()))))
              #false)

; function cond 的 clause 數量與 data definition 的 clause 數量相同

(define (contains-flatt? alon)
  (cond
    [(empty? alon) ...]
    [(cons? alon) ...]))

(define (contains-flatt? alon)
  (cond
    [(empty? alon) ...]
    [else ...]))
  
; 如果資料有 compounds 的話，我們可以把 selector 加到 template 中

(define (contains-flatt? alon)
  (cond
    [(empty? alon) ...]
    [(cons? alon) 
     (... (first alon) ... (rest alon) ...)]))

(define (contains-flatt? alon)
  (cond
    [(empty? alon) ...]
    [(cons? alon) 
     (... (string=? (first alon) "Flatt")
      ... (contains-flatt? (rest alon)) ...)]))

; List-of-names -> Boolean
; determines whether "Flatt" occurs on alon
(check-expect
  (contains-flatt? (cons "X" (cons "Y" (cons "Z" '()))))
  #false)
(check-expect
  (contains-flatt? (cons "A" (cons "Flatt" (cons "C" '()))))
  #true)
(define (contains-flatt? alon)
  (cond
  [(empty? alon) #false]
  [(cons? alon)
    (or (string=? (first alon) "Flatt")
        (contains-flatt? (rest alon)))]))

; Ex132
(contains-flatt?
 (cons "Fagan"
  (cons "Findler"
    (cons "Fisler"
      (cons "Flanagan"
        (cons "Flatt"
          (cons "Felleisen"
            (cons "Friedman" '()))))))))

; Ex133
(define (contains-flatt? alon)
  (cond
  [(empty? alon) #false]
  [(cons? alon)
    (cond
      [(string=? (first alon) "Flatt") #true]
      [else (contains-flatt? (rest alon))])]))

; 前者較簡潔，後者較清楚

; Ex134

; List-of-strings, String -> Boolean
; determines whether given string occurs on alos
(check-expect
 (contains? (cons "X" (cons "Y" (cons "Z" '()))) "X")
  #true)

(check-expect
 (contains? (cons "X" (cons "Y" (cons "Z" '()))) "A")
  #false)

(define (contains? alos str)
  (cond
    [(empty? alos) #false]
    [(cons? alos)
      (cond
        [(string=? (first alos) str) #true]
        [else (contains? (rest alos) str)])]))

; member?
(member? "Flatt" (cons "b" (cons "Flatt" '())))

; 8.4

; Ex135

(define (contains-flatt? alon)
  (cond
  [(empty? alon) #false]
  [(cons? alon)
    (cond
      [(string=? (first alon) "Flatt") #true]
      [else (contains-flatt? (rest alon))])]))

(contains-flatt? (cons "Flatt" (cons "C" '())))

(define (contains-flatt? alon)
  (cond
  [(empty? alon) #false]
  [(cons? alon)
    (cond
      [(string=? (first alon) "Flatt") #true]
      [else (contains-flatt? (rest alon))])]))

(contains-flatt?
  (cons "A" (cons "Flatt" (cons "C" '()))))

(define (contains-flatt? alon)
  (cond
  [(empty? alon) #false]
  [(cons? alon)
    (cond
      [(string=? (first alon) "Flatt") #true]
      [else (contains-flatt? (rest alon))])]))

(contains-flatt?
  (cons "A" (cons "B" (cons "C" '()))))

; Ex 136

(define-struct pair [left right])
; A ConsPair is a structure
; (make-pair Any Any)

; A ConsOrEmpty is one of:
; - '()
; - (make-pair Any ConsOrEmpty)
; interpretation ConsOrEmpty is the class of all lists

; Any Any -> ConsOrEmpty
(define (our-cons a-value a-list)
  (cond
    [(empty? a-list) (make-pair a-value a-list)]
    [(pair? a-list) (make-pair a-value a-list)]
    [else (error "cons: second argument ...")]))

; ConsOrEmpty -> Any
; extracts the left part of the given pair
(define (our-first a-list)
  (if (empty? a-list)
      (error 'our-first "...")
      (pair-left a-list)))

; ConsOrEmpty -> Any
; extracts the rest part of the given pair
(define (our-rest a-list)
  (if (empty? a-list)
      (error 'our-first "...")
      (pair-right a-list)))

(our-first (our-cons "a" '()))
(our-rest (our-cons "a" '()))