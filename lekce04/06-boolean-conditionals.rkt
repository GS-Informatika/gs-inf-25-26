;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 06-boolean-conditionals) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Booleany využíváme při rozhodování - větvení kódu (branching)
;; Výraz if - podmíněný výraz

(define SUNNY #true)
(define ACCESSORY
  (if SUNNY
      "sunglasses" ; Pokud je sunny #true ->
      ;; výraz (if ...) se vyhodnotí na "sunglasses"
      "umbrella")) ; Pokud je sunny #false ->
      ;; výraz (if ...) se vyhodnotí na "umbrella"

;; Výraz if má obecně zápis
#;(if VýrazBool VýrazTrue VýrazFalse)

(define warm-threshold-celsius 39)
(define (water-status temperature-celsius)
  (if (>= temperature-celsius warm-threshold-celsius)
      "warm"
      "not warm"))


;; --- CVIČENÍ ---
; Pracujeme s obrázky - potřebujeme modul
(require 2htdp/image)

;  Definujte funkci
#; image-classify
;; která zkonzumuje obrázek a vytvoří
;;  - string "tall" pokud je výška obrázku větší
;;    než šířka obrázku
;;  - string "wide" pokud je šířka obrázku větší
;;    než výška obrázku
;;  - string "square" pokud je šířka obrázku
;;    stejná jako výška obrázku.


;; Výraz if vyhodnotí pouze výraz v příslušné větvi,
;; druhý výraz se nevyhodnotí! Tato vlastnost bude později
;; důležitá!

(define (only-one-branch x)
  (if (> x 0)
      (error "True větev")
      (error "False větev")))

;; Tuto vlastnost popisujeme jako "tail pozice".
;; Oba výrazy (error ...) jsou v tail pozici vůči
;; výrazu (if ...), protože při postupné redukci
;; vždy dostaneme výraz příslušné větve - ne
;; výsledek výrazu!


;; Porovnejme pomocí stepperu:
(define (function-reduction x y)
  (not (and (> x 0)
       (> y 0))))

(define (tail-reduction x y)
  (not (if (> x 0) 
       (> y 0) 
       #f)))

#;(function-reduction 5 6)
#;(tail-reduction 5 6)

;; Redex (reducible expression) výrazu
#; (not (and (> 5 0)
       (> 6 0)))
;; je 
#; (> 5 0)

;; Po vyhodnocení tohoto výrazu zůstane
#; (not (and #true
       (> 6 0)))
;; a redexem je
#; (> 6 0)


;; Redex výrazu 
#; (not (if (> 5 0) 
       (> 6 0) 
       #f))
;; je také
#; (> 5 0)

;; Po vyhodnocení tohoto výrazu zůstane
#; (not (if #true 
       (> 6 0) 
       #f))
;; a redexem je
#; (if #true 
       (> 6 0) 
       #f)
;; které se vyhodnotí na výraz
#; (> 6 0)


;; Pokud potřebujeme rozhodnout mezi více možnostmi
;; tak můžeme využít vnoření výrazu if
(define (signum-bad x)
  (if (> x 0)
      1
      (if (= x 0)
          0
          -1)))

;; To ale není příliš přehledné, zejména pokud
;; je hodně možností.
;; Lepší je využít "cond" klauzuli - kondicionál
(define (signum x)
  (cond [(> x 0) 1]
        [(= x 0) 0]
        [(< x 0) -1]))

;; Kondicionál má tvar
#;(cond
    [podmínka-1 výsledek-1]
    [podmínka-2 výsledek-2]
    ...
    [podmínka-n výsledek-n]
    [else výsledek-else])

#; výraz-podmínky-x ; je výraz, co se vždy 
;;                    redukuje na boolean!

;; --- CVIČENÍ ---

;  Definujte funkci
#; image-classify.v2
;; (z předchozího cvičení) pomocí klauzule cond.





; Pracujeme s animací - potřebujeme modul
(require 2htdp/universe)
;; V minulých lekcích jsme si ukázali funkci
#;(animate ...)

;; Animovali jsme volný pád tečky - funkce generující:
#;(define (picture-of-dot time)
  (place-image (circle 5 "solid" "red")
               50 (sqr time)
               (empty-scene 100 300)))

;; Vaším úkolem bude vytvořit stejnou animaci, odehrávající se na plátně o šířce WIDTH
;; a výšce HEIGHT s kroužkem o poloměru CIRCLE-RADIUS.
;; Jakmile se kroužek dotkne spodního okraje scény zastaví se - nebude se dále pohybovat.
(define WIDTH 100)
(define HEIGHT 300)
(define CIRCLE-RADIUS 3)
(define dot (circle CIRCLE-RADIUS "solid" "red"))

(define (circle-fall t)
  (place-image
   dot
   ...
   (empty-scene ...)))



#;(animate circle-fall)

;; Nápověda: Rozdělte problém na více funkcí - definujte si funkci
#;(circle-height t)
;; která vrátí výšku ve které se má tečka nacházet v čase t.
;; Uvnitř této funkce použíjte výraz if nebo kondicionál cond.
