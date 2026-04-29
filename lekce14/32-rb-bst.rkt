;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 32-rb-bst) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ------ Red-Black BST ------

;; Minule jsme si ukázali obyčejné BST.
;; Nevýhoda takového stromu je, že se při
;; nevhodném pořadí vkládání může "zvrhnout"
;; na skoro lineární strukturu.

;; Red-black tree je BST, které si navíc
;; udržuje informaci o barvě vrcholu a
;; při vkládání provádí lokální úpravy
;; tak, aby strom zůstal přibližně vyvážený.
;;
;; Budeme používat jednoduchou variantu,
;; kde "červené hrany" držíme vlevo.


; [RBNode T] is a struct:
#; (make-rb-node String T [RBBST T] [RBBST T])
; Node of red-black binary search tree
(define-struct rb-node [color value left right])

; [RBBST T] is one of
; - [RBNode T]
; - "empty"
; Red-black binary search tree


(define RED "red")
(define BLACK "black")
(define EMPTY "empty")


; Any -> Boolean
(define (rb-empty? t)
  (and (string? t)
       (string=? EMPTY t)))


; [RBBST T] -> Boolean
(define (red-node? tree)
  (and (rb-node? tree)
       (string=? RED (rb-node-color tree))))


;; RB tree splňuje stejné uspořádání jako BST:
;; vlevo jsou menší prvky, vpravo větší.
;;
;; Navíc hlídáme:
;; 1. kořen je černý
;; 2. červený vrchol nemá červeného potomka
;; 3. na každé cestě do listu je stejný počet
;;    černých vrcholů
;; 4. červená hrana "směřuje doleva"


(define RB-BST-1
  (make-rb-node BLACK 10
                (make-rb-node BLACK 7
                              (make-rb-node RED 5 EMPTY EMPTY)
                              EMPTY)
                (make-rb-node BLACK 20
                              EMPTY
                              EMPTY)))


;; Vyhledávání funguje stejně jako u BST.

; [RBBST T] [T T -> Boolean] [T T -> Boolean] T -> Boolean
(define (rb-contains? tree eq? larger? value)
  (cond [(rb-empty? tree) #f]
        [(rb-node? tree)
         (cond [(eq? value (rb-node-value tree)) #t]
               [(larger? value (rb-node-value tree))
                (rb-contains? (rb-node-right tree) eq? larger? value)]
               [else
                (rb-contains? (rb-node-left tree) eq? larger? value)])]))


(check-expect (rb-contains? RB-BST-1 = > 7) #t)
(check-expect (rb-contains? RB-BST-1 = > 8) #f)


;; Vkládání je složitější než u obyčejného BST.
;; Nejdříve vložíme nový prvek jako červený vrchol
;; a pak cestou zpět opravujeme zakázané konfigurace.
;;
;; Proč červený? Červený vrchol nezvyšuje černou
;; výšku, takže neporuší invariant 3 (stejný počet
;; černých na každé cestě). Může ale porušit
;; invariant 2 nebo 4 — to opravuje balance.
;;
;; Pro jednoduchost pouze vložení bez mazání.

; [RBBST T] [T T -> Boolean] [T T -> Boolean] T -> [RBBST T]
(define (rb-insert tree eq? larger? value)
  (local (
          ; String -> String
          (define (flip-color color)
            (if (string=? color RED)
                BLACK
                RED))

          ; [RBBST T] String -> [RBBST T]
          (define (paint tree color)
            (cond [(rb-empty? tree) EMPTY]
                  [else
                   (make-rb-node color
                                 (rb-node-value tree)
                                 (rb-node-left tree)
                                 (rb-node-right tree))]))

          ; [RBNode T] -> [RBNode T]
          ; Rotates subtree to the left around its root
          ;;
          ;;    X              Y
          ;;   / \r    →     r/ \
          ;;  a   Y          X   c
          ;;     / \        / \
          ;;    b   c      a   b
          ;;
          (define (rotate-left node)
            (local ((define pivot (rb-node-right node)))
              (make-rb-node (rb-node-color node)
                            (rb-node-value pivot)
                            (make-rb-node RED
                                          (rb-node-value node)
                                          (rb-node-left node)
                                          (rb-node-left pivot))
                            (rb-node-right pivot))))

          ; [RBNode T] -> [RBNode T]
          ; Rotates subtree to the right around its root
          ;;
          ;;      Y          X
          ;;    r/ \    →   / \r
          ;;    X   c      a   Y
          ;;   / \            / \
          ;;  a   b          b   c
          ;;
          (define (rotate-right node)
            (local ((define pivot (rb-node-left node)))
              (make-rb-node (rb-node-color node)
                            (rb-node-value pivot)
                            (rb-node-left pivot)
                            (make-rb-node RED
                                          (rb-node-value node)
                                          (rb-node-right pivot)
                                          (rb-node-right node)))))

          ; [RBNode T] -> [RBNode T]
          ; Swaps colors of a node and its two children
          (define (flip-colors node)
            (make-rb-node (flip-color (rb-node-color node))
                          (rb-node-value node)
                          (paint (rb-node-left node)
                                 (flip-color (rb-node-color (rb-node-left node))))
                          (paint (rb-node-right node)
                                 (flip-color (rb-node-color (rb-node-right node))))))

          ; [RBBST T] -> [RBBST T]
          ; Repairs forbidden configurations created by insertion
          (define (balance tree)
            (cond [(rb-empty? tree) EMPTY]
                  [else
                   (local (
                           ;; Červená hrana směřuje doprava → otočíme doleva
                           ;; (oprava invariantu 4)
                           (define step1
                             (if (and (red-node? (rb-node-right tree))
                                      (not (red-node? (rb-node-left tree))))
                                 (rotate-left tree)
                                 tree))
                           ;; Dvě červené hrany za sebou doleva → otočíme doprava
                           ;; (oprava invariantu 2)
                           (define step2
                             (if (and (red-node? (rb-node-left step1))
                                      (red-node? (rb-node-left (rb-node-left step1))))
                                 (rotate-right step1)
                                 step1))
                           ;; Obě hrany červené → přebarvíme
                           ;; (rozštěpení "4-uzlu", posun červené nahoru)
                           (define step3
                             (if (and (red-node? (rb-node-left step2))
                                      (red-node? (rb-node-right step2)))
                                 (flip-colors step2)
                                 step2)))
                     step3)]))

          ; [RBBST T] -> [RBBST T]
          (define (insert/inner tree)
            (cond [(rb-empty? tree)
                   (make-rb-node RED value EMPTY EMPTY)]
                  [(eq? value (rb-node-value tree)) tree]
                  [(larger? value (rb-node-value tree))
                   (balance
                    (make-rb-node (rb-node-color tree)
                                  (rb-node-value tree)
                                  (rb-node-left tree)
                                  (insert/inner (rb-node-right tree))))]
                  [else
                   (balance
                    (make-rb-node (rb-node-color tree)
                                  (rb-node-value tree)
                                  (insert/inner (rb-node-left tree))
                                  (rb-node-right tree)))]))

          ; [RBBST T]
          (define inserted (insert/inner tree)))
    (paint inserted BLACK)))


;; Tři vzestupně vložené prvky by v obyčejném BST
;; vytvořily dlouhou větev. Tady se provede rotace.

(define RB-BST-2
  (rb-insert
   (rb-insert
    (rb-insert EMPTY = > 1)
    = >
    2)
   = >
   3))

(check-expect RB-BST-2
              (make-rb-node BLACK 2
                            (make-rb-node BLACK 1 EMPTY EMPTY)
                            (make-rb-node BLACK 3 EMPTY EMPTY)))


;; Další ukázka vkládání — krokový průchod:
;;
;; 1) insert 10:  10(B)
;;
;; 2) insert 5:   10(B)
;;               /
;;             5(R)
;;
;; 3) insert 1:
;;    insert/inner vytvoří 1(R) vlevo od 5(R)
;;    → balance na úrovni 10: left=5(R), left.left=1(R)
;;      → step2: rotate-right
;;
;;        5(B)            5(R)
;;       / \     flip    / \
;;     1(R) 10(R) →    1(B) 10(B)
;;
;;      → step3: flip-colors (oba potomci červení)
;;      → paint root BLACK:
;;
;;        5(B)
;;       / \
;;     1(B) 10(B)
;;
;; 4) insert 7:
;;    7 > 5, jde doprava; 7 < 10, jde doleva
;;    → vznikne 7(R) jako levý potomek 10(B)
;;    → žádná oprava není potřeba (jedna červená doleva)
;;
;;        5(B)
;;       / \
;;     1(B) 10(B)
;;          /
;;        7(R)

(define RB-BST-3
  (rb-insert
   (rb-insert
    (rb-insert
     (rb-insert EMPTY = > 10)
     = >
     5)
    = >
    1)
   = >
   7))

(define RB-BST-4
  (foldl (lambda (num acc) (rb-insert acc = > num))
         EMPTY
         (list 8 2 1 3 4 5 6 20 19 18 17 16 10 11 12)))

;; Cvičení
;; 1. Napište funkci tree-height pro [RBBST T]
;; 2. Napište funkci is-rb-bst? která ověří BST invariant
;;    i základní red-black pravidla
;; 3. Zkuste nakreslit strom po vložení prvků
;;    1 2 3 4 5 6 7
