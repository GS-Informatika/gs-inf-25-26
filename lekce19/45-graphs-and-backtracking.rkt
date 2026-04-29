;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 44-graphs-and-backtracking) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Procházení grafů

;; Se speciálním typem grafu jsme se setkali v rámci procvičovacích úloh,
;; binární strom je speciální případ acyklického orientovaného grafu.
;; Nyní se budeme zabývat obecnými grafy.

;; Graf je kolekce vrcholů (nodes) a hran (edges), které vrcholy spojují.
;; V orientovaném grafu pak mají hrany směr (vedou od jednoho vrcholu k druhému).

;; Sample problem:
;; Nadesignujme algoritmus, který najde způsob jak předat informaci
;; od jednoho člověka A k člověku B, když dostaneme orientovaný graf známostí.
;; Funkce by měla vrátit sekvenci emailových adres, mezi kterými
;; má informace projít.
;; Tomuto se říká "cesta" na grafu.

;;  Pro vyřešení tohoto problému budeme ale muset zavést backtracking!
;; Bude třeba pamatovat si všechny možnosti, vybrat jednu a v případě neúspěchu se vrátit
;; a zkusit jinou.

;; Reprezentace grafu - bod a kam se z něj lze dostat


(define-struct node [name neighbours])
; Node is a struct:
#; (make-node String [List-of String])


; Graph je [List-of Node]

(define graph
  (list (make-node "A" (list "B" "E"))
        (make-node "B" (list "E" "F"))
        (make-node "C" (list "D"))
        (make-node "D" '())
        (make-node "E" (list "C" "F"))
        (make-node "F" (list "D" "G"))
        (make-node "G" '())))






; String Graph -> [List-of String]
; Vrátí jména sousedů vrcholu se jménem name.
(define (neighbours name g)
  (cond
    [(empty? g) (error name " not in n")]
    [(string=? (node-name (first g)) name)
     (node-neighbours (first g))]
    [else (neighbours name (rest g))]))


; Definujme nyní hlavičku funkce find-path

; String String Graph -> [List-of String]
; Nalezne cestu z origin do dest v grafu g
#;(define (find-path origin dest g)
  '())

;; Co vše může funkce vrátit!?
;; Hlavička ani purpose statement nám neříká co přesně bude výsledek
;; obsahovat!
#; (find-path "C" "D" graph) ; Musí vrátit cestu
#; (find-path "E" "D" graph) ; Může vybrat a vrátit jednu z více cest
#; (find-path "C" "G" graph) ; Musí označit, že taková cesta neexistuje

;; Máme možnosti:
;;  - Výsledek bude list všech vrcholů včetně origin a dest. Pak může '() označit,
;;    že taková cesta neexistuje

;;  - Výsledek bude jen list vrcholů mezi origin a dest které musíme navštívit, pak
;;    musíme špatný výsledek označit jinak! Zde je vhodné např. #false

;; Signalizaci pomocí false ale můžeme použít i v prvním případě. Přepišme tedy hlavičku!


; Path is [List-of String]
; Sequence of names of neighbouring
; nodes that make up a path.


; String String Graph -> [Maybe Path]
; Nalezne cestu z origin do dest v grafu g
; Pokud taková cesta neexistuje, vrátí #false
#;(define (find-path origin dest g)
  #false)

;; Nyní můžeme designovat funkci - provedeme "analýzu" triviality

;; Pokud jsou vrcholy přímo spojeny, cesta mezi nimi se skládá pouze z
;; nich. Nicméně ještě triviálnější je (find-path o o g) - hledání
;; cesty z bodu A do bodu A. Výsledkem je pak
#; (list dest)

;;  Pokud se argumenty liší, musíme prozkoumat všechny sousedící vrcholy origin
;; a rozhodnout, jestli z nějákého z nich vede cesta do dest.

;; Jakmile máme cestu z origin do dest, přidáme jí k cestě z předchozího "kroku"
;; (přidáme origin node do listu)

;; Druhá část (prozkoumávání všech sousedů) je kritický a netriviální krok
;; Budeme postupovat podobně jako když jsme
;; dělali vnořená data (strom listů) - zavedeme
;; pomocnou funkci, která "vyzkouší všechny možnosti"

; [List-of String] String Graph -> [Maybe Path]
; Nalezne cestu z nějákého vrcholu z origins do dest v grafu g
; Vrátí false pokud žádnou cestu nenalezne.
#;(define (find-path/list origins dest g)
  #false)

#; (define (find-path origin dest g)
     (cond
       [(string=? origin dest)
        (list dest)]
       [else
        (... origin ...
         ... (find-path/list (neighbours origin g)
                            dest g) ...)]))

;; Sice ještě nemáme implementovanou funkci find-path/list, díky headeru ji ale
;; můžeme rovnou "napojit"! Musíme brát v potaz, že vrací [Maybe Path]

#;(define (find-path origin dest g)
    (cond
      [(string=? origin dest)
       (list dest)]
      [else
       (local ((define next (neighbours origin g))
               (define candidate
                 (find-path/list next dest g)))
         (cond
           [(boolean? candidate) ...]
           [(cons? candidate) ...]))]))

;; Nyní už můžeme funkce poměrně jednoduše doimplementovat

; String String Graph -> [Maybe Path]
; Nalezne cestu z origin do dest v grafu g
; Pokud taková cesta neexistuje, vrátí #false
(check-expect (find-path "A" "C" graph)
              (list "A" "B" "E" "C"))
(check-expect (find-path "C" "G" graph)
              #false)
(define (find-path origin dest g)
  (local (; String -> [Maybe Path]
          (define (fp o)
            (cond
              [(string=? o dest) (list dest)]
              [else (local ((define candidate (fp/list (neighbours o g))))
                      (cond
                        [(boolean? candidate) #false]
                        [else (cons o candidate)]))]))
          ; [List-of String] -> [Maybe Path]
          (define (fp/list origins)
            (cond
              [(empty? origins) #false]
              [else (local ((define candidate (fp (first origins))))
                      (cond
                        [(boolean? candidate) (fp/list (rest origins))]
                        [else candidate]))])))
    (fp origin)))

;; Funkce find-path/list zajišťuje backtracking - postupně zkoušíme
;; jednotlivé možnosti, dokud nenarazíme na správnou.

;; Co kdybychom přidali hranu od C do B. Pak už náš graf nebude acyklický.
;; Najde náš algoritmus cestu pro každý vstup?

(define cyclic-graph
  (list (make-node "A" (list "B" "E"))
        (make-node "B" (list "E" "F"))
        (make-node "C" (list "D" "B"))   ; C → B vytváří cyklus
        (make-node "D" '())
        (make-node "E" (list "C" "F"))
        (make-node "F" (list "D" "G"))
        (make-node "G" '())))

;; (find-path "A" "G" cyclic-graph) ; Zkuste - algoritmus se zacyklí!

;; Problém: find-path může navštívit stejný vrchol vícekrát
;; a nikdy neskončit (cyklus C → B → E → C → ...).

;; Řešení: budeme si pamatovat, které vrcholy jsme již navštívili.
;; Tato "paměť" je přesně to, co nazýváme akumulátorem!

; String String Graph [List-of String] -> [Maybe Path]
; Jako find-path, ale visited obsahuje již navštívené vrcholy.
; Akumulátor invariant: visited = seznam vrcholů, které jsme
; navštívili na cestě k origin (nebo dříve prozkoumali).
#;(define (find-path/visited origin dest g visited) #false)

;; Triviální případ zůstává stejný.
;; V rekurzivním případě:
;;  1) Přeskočíme sousedy, které jsme již navštívili.
;;  2) Přidáme origin do visited, než prozkoumáme jeho sousedy.

(check-expect (find-path/visited "A" "C" cyclic-graph '())
              (list "A" "B" "E" "C"))
(check-expect (find-path/visited "C" "G" cyclic-graph '())
              (list "C" "B" "E" "F" "G"))
(check-expect (find-path/visited "A" "G" cyclic-graph '())
              (list "A" "B" "E" "F" "G"))
(define (find-path/visited origin dest g visited)
  (local (; String [List-of String] -> [Maybe Path]
          (define (fpv o vis)
            (cond
              [(string=? o dest) (list dest)]
              [(member o vis) #false]
              [else (local ((define candidate
                              (fpv/list (neighbours o g) (cons o vis))))
                      (cond
                        [(boolean? candidate) #false]
                        [else (cons o candidate)]))]))
          ; [List-of String] [List-of String] -> [Maybe Path]
          (define (fpv/list origins vis)
            (cond
              [(empty? origins) #false]
              [else (local ((define candidate (fpv (first origins) vis)))
                      (cond
                        [(boolean? candidate) (fpv/list (rest origins) vis)]
                        [else candidate]))])))
    (fpv origin visited)))
