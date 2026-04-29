;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 46-n-queens) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; N-Queens problém

;; Máme šachovnici velikosti N×N. Chceme na ni umístit N dam tak,
;; aby se žádné dvě dámy neohrožovaly.
;; Dvě dámy se ohrožují, pokud leží na stejném řádku, sloupci nebo diagonále.

;; Strategie:
;;   1. Vygenerujeme seznam všech pozic šachovnice (řádek po řádku).
;;   2. Procházíme tento seznam a postupně se pokoušíme na každou pozici
;;      umístit dámu – pokud je to bezpečné.
;;   3. Pokud se dostaneme do slepé uličky, backtrackujeme.

;; --- Reprezentace dat ---

;; Pozice na šachovnici je (make-posn row col), kde row a col jsou 0 až N-1.
;; Board je [List-of Posn] — seznam umístěných dam.

;; ============================================================
;; Krok 1 — Generování pozic
;; ============================================================

;; Nejprve potřebujeme seznam všech políček šachovnice N×N,
;; seřazený řádek po řádku, zleva doprava.

; Natural -> [List-of Posn]
; Vrátí seznam všech N*N pozic na šachovnici, řádek po řádku.
(check-expect (board-positions 1) (list (make-posn 0 0)))
(check-expect (board-positions 2) (list (make-posn 0 0) (make-posn 0 1)
                                        (make-posn 1 0) (make-posn 1 1)))
; Nápověda: použijte build-list.
(define (board-positions n)
  ...)

;; ============================================================
;; Krok 2 — Bezpečnost umístění
;; ============================================================

;; Před umístěním dámy musíme ověřit, že neohrožuje žádnou z již umístěných.
;; Dvě dámy se ohrožují, pokud sdílejí řádek, sloupec, nebo diagonálu.
;; (Na diagonále jsou právě tehdy, když |Δrow| = |Δcol|.)

; Posn [List-of Posn] -> Boolean
; Je bezpečné umístit dámu na pos, pokud placed jsou již umístěné dámy?
(check-expect (safe? (make-posn 0 0) '()) #true)
(check-expect (safe? (make-posn 1 0) (list (make-posn 0 0))) #false) ; stejný sloupec
(check-expect (safe? (make-posn 1 0) (list (make-posn 0 1))) #false) ; diagonála
(check-expect (safe? (make-posn 1 2) (list (make-posn 0 0))) #true)
(check-expect (safe? (make-posn 2 0) (list (make-posn 1 2) (make-posn 0 1))) #false)
; Nápověda: andmap přes placed, pro každou dámu ověřte tři podmínky.
(define (safe? pos placed)
  ...)

;; ============================================================
;; Krok 3 — Backtracking přes seznam pozic
;; ============================================================

;; Máme seznam kandidátních pozic (zbývajících k vyzkoušení) a seznam
;; již umístěných dam. Procházíme kandidáty jeden po druhém:
;;   - Pokud jsou umístěny všechny N dámy → hotovo, vrátíme placed.
;;   - Pokud jsme vyčerpali kandidáty → slepá ulička, vrátíme #false.
;;   - Jinak vyzkoušíme první kandidáta:
;;       a) Pokud je umístění bezpečné, rekurzivně pokračujeme S umístěnou dámou.
;;       b) Pokud to vede do slepé uličky (nebo umístění není bezpečné),
;;          rekurzivně pokračujeme BEZ umístění dámy na toto políčko.

; [List-of Posn] [List-of Posn] Natural -> [Maybe Board]
; Zkusí umístit n dam procházením candidates.
; placed jsou dosud umístěné dámy.
; Vrátí Board nebo #false.
(define (queens candidates placed n)
  ...)

;; ============================================================
;; Krok 4 — Řešení
;; ============================================================

; Natural -> [Maybe Board]
; Najde jedno řešení N-queens pro šachovnici N×N, nebo #false.
(check-expect (solve-queens 1) (list (make-posn 0 0)))
(check-expect (solve-queens 2) #false)
(check-expect (solve-queens 3) #false)
(check-expect (length (solve-queens 4)) 4)
(define (solve-queens n)
  (queens (board-positions n) '() n))

;; ============================================================
;; Bonusové úlohy
;; ============================================================

;; 1) queens vrátí pouze jedno řešení.
;;    Navrhněte all-solutions : Natural -> [List-of Board],
;;    která vrátí všechna řešení N-queens problému.

;; 2) Kolik řešení existuje pro N=6? N=8?
;;    Napište count-solutions : Natural -> Natural.
;;    (https://oeis.org/A000170)

;; 3) Vizualizace pomocí 2htdp/image.
;;    (require 2htdp/image)
;;
;;    Konstanty:
;;    (define CELL 60)
;;    (define LIGHT (square CELL 'solid "wheat"))
;;    (define DARK  (square CELL 'solid "saddlebrown"))
;;    (define QUEEN (overlay (text "♛" 40 "black") (square CELL 'solid "transparent")))
;;
;;    Navrhněte:
;;
;;    a) cell->image : Posn Board -> Image
;;       Vrátí obrázek jednoho políčka — světlé nebo tmavé podle (+ row col),
;;       s dámou pokud je pos v board.
;;
;;    b) row->image : Natural Board Natural -> Image
;;       Vrátí obrázek jednoho řádku jako beside všech buněk.
;;       Nápověda: build-list + map + foldr.
;;
;;    c) board->image : Board Natural -> Image
;;       Složí řádky pomocí above.
;;       (board->image (solve-queens 4) 4) zobrazí řešení přímo v DrRacketu!
