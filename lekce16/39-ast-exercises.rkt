;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 39-ast-exercises) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Polynomy a symbolické derivování

(require 2htdp/abstraction)

;; Polynomy a operace na nich můžeme interpretovat podobně jako
;; jsme interpretovali booleovské výrazy.

;; V případě polynomů rozlišujeme základní operace
;; 1) sčítání
;; 2) násobení

;; Data jsou pak
;; 1) Proměnná polynomu ("x", "y", ...)
;; 2) Konstanta (číslo)

(define-struct variable [name])
; Variable is a struct:
#; (make-variable String)


(define-struct add [left right])
; OpAdd is a struct:
#; (make-add Polynomial Polynomial)


(define-struct mult [left right])
; OpMult is a struct:
#; (make-mult Polynomial Polynomial)


; Polynomial is one of
; - Number
; - Variable
; - OpAdd
; - OpMult


(define-struct binding [variable value])
; Binding is a struct:
#; (make-binding Variable Number)

; Environment is one of:
; - '()
; - [List-of Binding]


; Variable Variable -> Boolean
(check-expect (same-variable? (make-variable "x")
                              (make-variable "x"))
              #t)
(check-expect (same-variable? (make-variable "x")
                              (make-variable "y"))
              #f)
(define (same-variable? v1 v2)
  (match (list v1 v2)
    [(list (variable name1) (variable name2))
     (string=? name1 name2)]))


;; Abychom si s AST polynomů mohli hrát ještě před derivací,
;; budeme mít k dispozici prostředí a funkci eval.


; Variable Environment -> Number
(check-expect (bind (make-variable "x")
                    (list (make-binding (make-variable "x") 5)))
              5)
(check-error (bind (make-variable "z")
                   (list (make-binding (make-variable "x") 5))))
(define (bind searched-variable environment)
  (match environment
    ['() (error "unbound variable")]
    [(cons (binding variable value) rest)
     (if (same-variable? searched-variable variable)
         value
         (bind searched-variable rest))]))


; Polynomial Environment -> Number
(check-expect (eval 3 '()) 3)
(check-expect (eval (make-variable "x")
                    (list (make-binding (make-variable "x") 7)))
              7)
(check-expect (eval (make-add (make-variable "x") 3)
                    (list (make-binding (make-variable "x") 7)))
              10)
(check-expect (eval (make-mult (make-add (make-variable "x") 3)
                               (make-variable "y"))
                    (list (make-binding (make-variable "x") 7)
                          (make-binding (make-variable "y") 2)))
              20)
#;(check-expect (eval (make-power (make-add (make-variable "x") 1) 2)
                    (list (make-binding (make-variable "x") 2)))
              9)
(define (eval polynomial environment)
  (match polynomial
    [(? number?) polynomial]
    [(variable name) (bind (make-variable name) environment)]
    [(add left right) (+ (eval left environment)
                         (eval right environment))]
    [(mult left right) (* (eval left environment)
                          (eval right environment))]
    #;[(power base exponent) (expt (eval base environment)
                                 exponent)]))

;; Pro derivaci polynomů platí

;; Derivace konstanty/proměnné c podle x
;; -> dc/dx = 0 ;; pro c konstantu nebo proměnnou jinou než x
;; Například:
;;  - derivace 1 podle x je 0.
;;  - derivace k podle y je 0.

;; Derivace proměnné podle sama sebe
;;  -> dx/dx = 1

;; Derivace součtu polynomů
;; -> d(u + v)/dx = du/dx + dv/dx
;; Například:
;;  - derivace ((5 * x) + (7 * x)) je
;;     derivace (5 * x) + derivace (7 * x)

;; Derivace násobení polynomů
;; -> d(u * v)/dx = u * dv/dx + v du/dx
;; Například:
;;  - derivace ((5 + x) * (7 + x)) je
;;     (5 + x) * derivace (7 + x)
;;      + (7 + x) * derivace (5 + x)



;; Cvičení - pomocí pravidel pro derivaci polynomů napište funkci
;; differentiate, která zderivuje polynom podle pravidel
;; výše. Napište si vhodné pomocné funkce.

;; 1) Identifikujte které struktury představují data a které
;;    představují operace (defunkcionalizace)




;; 2) Identifikujte listy (leafs) ve stromové struktuře polynomů




;; 3) Napište dvě ukázky polynomu který bude obsahovat více proměnných




;; 4) Přečtěte si hotovou pomocnou funkci same-variable?.
;;    Kde v ní probíhá pattern matching?
;;    Proč stačí porovnat jen names uvnitř struktury variable?




;; 5) Napište funkci differentiate, která provede symbolickou derivaci podle
;;    proměnné.

;;    Převeďte pravidla pro derivace do funkcí v lokálním prostředí funkce
;;    differentiate.
;;    Tyto funkce budou očekávat příslušnou variantu
;;    z Polynomial typu a hodnotu
#;    (make-variable String)
;;    podle které bude derivováno.

; Polynomial Variable -> Polynomial
#;(check-expect (differentiate 5 (make-variable "x")) 0)
#;(check-expect (differentiate (make-variable "x")
                               (make-variable "x"))
                1)
#;(check-expect (differentiate (make-variable "y")
                               (make-variable "x"))
                0)
(define (differentiate polynomial variable)
  ...)




;; 6) Výsledek není dobře čitelný. Vytvořme funkci ast->string,
;;    která převede naši reprezentaci polynomu do čitelného stringu

; Polynomial -> String
#;(check-expect (ast->string (make-add 2 (make-variable "x")))
                "(2 + x)")
#;(check-expect (ast->string
                 (make-mult (make-add 2 (make-variable "x"))
                            (make-variable "y")))
                "((2 + x) * y)")
(define (ast->string polynomial)
  ...)




;; 7) Vidíme, že ve výsledku se vyskytují členy typu 0 * ..., které lze "vynechat".
;;    Napište funkci optimize, která odstraní členy které vždy výjdou 0.
;;    (Tree-shaking je název procedury která odstraňuje přebytečné větve ve stromové
;;     reprezentaci kódu).

; Polynomial -> Polynomial
#;(check-expect (optimize (make-mult 0 (make-variable "x"))) 0)
#;(check-expect (optimize (make-add 0 (make-variable "x")))
                (make-variable "x"))
(define (optimize polynomial)
  ...)




;; 8) Přidejte variantu pro mocninu (expression^číslo)
;;    Pro derivaci mocniny proměnné platí následující pravidlo
;;    d(x^n)/dx = n * x^(n-1)
;;    Pro kompozici funkcí platí pravidlo
;;    ( f(g(x)) )' = f'(g(x)) * g'(x)
;;    Spojením těchto dvou pravidel dostáváme pravidlo které budeme
;;    implementovat:
;;    d( f(x)^n )/dx = n * f(x)^(n-1) * d( f(x) )/dx

;;    Použijte následující strukturu:
#;    (define-struct power [base exponent])
;;    Pořadí operandů je:
;;    - base : Polynomial
;;    - exponent : Number
;;
;;    Upravte funkci differentiate tak, aby splňovala toto pravidlo.
;;    Upravte funkci ast->string tak, aby zvládla zápis mocnin.
;;    Upravte funkci optimize tak, aby výraz (expr)^0 převedla na 1,
;;    výraz (expr)^1 převedla na (expr), výraz 0^n na 0. Pro výraz
;;    0^0 zvolte převod na 1.




;; 9) Ukázka: parser z textu zpět do AST
;;
;; Když máme AST reprezentaci, pretty-printer (ast->string)
;; a interpreter (eval), můžeme doplnit ještě parser.
;; Tím dostaneme základ malého programovacího jazyka:
;;
;;   text -> parser -> AST -> eval
;;                  -> AST -> differentiate
;;                  -> AST -> ast->string
;;
;; Níže je ukázka jednoduchého recursive descent parseru pro
;; přesně ten formát, který produkuje ast->string.

(define-struct parse-result [polynomial rest])
; ParseResult is a struct:
#; (make-parse-result Polynomial [List-of String])


; String -> Polynomial
(check-expect (parse "5") 5)
(check-expect (parse "x") (make-variable "x"))
(check-expect (parse "(2 + x)")
              (make-add 2 (make-variable "x")))
(check-expect (parse "((2 + x) * y)")
              (make-mult (make-add 2 (make-variable "x"))
                         (make-variable "y")))
#;(check-expect (parse "(x ^ 3)")
              (make-power (make-variable "x") 3))
#;(check-expect
   (parse (ast->string (make-mult (make-add 2 (make-variable "x"))
                                  (make-variable "y"))))
   (make-mult (make-add 2 (make-variable "x"))
              (make-variable "y")))
(define (parse source)
  (local [(define parsed (parse-polynomial (tokenize source)))]
    (if (empty? (parse-result-rest parsed))
        (parse-result-polynomial parsed)
        (error "unexpected tokens at end"))))


; String -> [List-of String]
(check-expect (tokenize "((2 + x) * y)")
              (list "(" "(" "2" "+" "x" ")" "*" "y" ")"))
(define (tokenize source)
  (tokenize/chars (explode source) ""))


; [List-of 1String] String -> [List-of String]
(define (tokenize/chars chars current-token)
  (match chars
    ['() (finish-token current-token '())]
    [(cons char rest)
     (cond [(whitespace? char)
            (finish-token current-token
                          (tokenize/chars rest ""))]
           [(special-token? char)
            (finish-token current-token
                          (cons char
                                (tokenize/chars rest "")))]
           [else
            (tokenize/chars rest
                            (string-append current-token char))])]))


; String [List-of String] -> [List-of String]
(define (finish-token current-token tokens)
  (if (string=? current-token "")
      tokens
      (cons current-token tokens)))


; 1String -> Boolean
(define (whitespace? char)
  (or (string=? char " ")
      (string=? char "\t")
      (string=? char "\n")))


; 1String -> Boolean
(define (special-token? char)
  (or (string=? char "(")
      (string=? char ")")
      (string=? char "+")
      (string=? char "*")
      (string=? char "^")))


; [List-of String] -> ParseResult
(define (parse-polynomial tokens)
  (match tokens
    ['() (error "unexpected end of input")]
    [(cons "(" rest) (parse-parenthesized rest)]
    [_ (parse-atom tokens)]))


; [List-of String] -> ParseResult
(define (parse-parenthesized tokens)
  (local [(define left-result (parse-polynomial tokens))
          (define operator-tokens
            (expect-operator (parse-result-rest left-result)))
          (define operator (first operator-tokens))
          (define right-result (parse-polynomial (rest operator-tokens)))
          (define rest-after-right (parse-result-rest right-result))]
    (match (list operator rest-after-right)
      [(list "+" (cons ")" rest))
       (make-parse-result (make-add (parse-result-polynomial left-result)
                                    (parse-result-polynomial right-result))
                          rest)]
      [(list "*" (cons ")" rest))
       (make-parse-result (make-mult (parse-result-polynomial left-result)
                                     (parse-result-polynomial right-result))
                          rest)]
      #;[(list "^" (cons ")" rest))
       (local [(define exponent (parse-result-polynomial right-result))]
         (if (number? exponent)
             (make-parse-result
              (make-power (parse-result-polynomial left-result)
                          exponent)
              rest)
             (error "power expects numeric exponent")))]
      [_ (error "expected binary operator expression")])))


; [List-of String] -> [List-of String]
(define (expect-operator tokens)
  (match tokens
    [(cons "+" rest) tokens]
    [(cons "*" rest) tokens]
    [(cons "^" rest) tokens]
    [_ (error "expected operator")]))


; [List-of String] -> ParseResult
(define (parse-atom tokens)
  (match tokens
    [(cons token rest)
     (local [(define maybe-number (string->number token))]
       (if (number? maybe-number)
           (make-parse-result maybe-number rest)
           (make-parse-result (make-variable token) rest)))]
    ['() (error "unexpected end of input")]))
