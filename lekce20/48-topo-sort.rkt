;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 48-topo-sort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Topologické třídění

;; Chceme seřadit vrcholy grafu tak, aby každý vrchol
;; přišel PO všech vrcholech, na kterých závisí.
;;
;; Příklady:
;;   - Prerekvizity předmětů: Matematika 2 musí přijít po Matematice 1
;;   - Build systém:  objekt .o musí být zkompilován před linkováním
;;   - Správce balíčků: závislosti musí být nainstalovány před balíčkem
;;
;; Topologické třídění je definováno pouze pro DAG
;; (directed acyclic graph — orientovaný acyklický graf).

;; ============================================================
;; Část 1 — Datová definice
;; ============================================================

;; Stejná reprezentace jako v lekci 19.
;; Hrana A → B znamená "A musí přijít PŘED B".

(define-struct node [name neighbours])
; Node je (make-node String [List-of String])

; Graph je [List-of Node]

(define prereq-graph
  (list (make-node "zaklady" (list "linalg" "prog1"))
        (make-node "linalg"  (list "algo"))
        (make-node "prog1"   (list "prog2"))
        (make-node "prog2"   (list "algo"))
        (make-node "algo"    '())))

;; zaklady → linalg → algo
;;        ↘           ↗
;;          prog1 → prog2
;;
;; Validní topologická pořadí:
;;   zaklady prog1 prog2 linalg algo
;;   zaklady linalg prog1 prog2 algo   ... a další

(define cyclic-prereq
  (list (make-node "zaklady" (list "linalg" "prog1"))
        (make-node "linalg"  (list "algo"))
        (make-node "prog1"   (list "prog2"))
        (make-node "prog2"   (list "algo" "zaklady"))  ; prog2→zaklady tvoří cyklus
        (make-node "algo"    '())))

; String Graph -> [List-of String]
; Vrátí seznam sousedů vrcholu name. (stejná funkce jako v lekci 19)
(define (neighbours name g)
  (cond [(empty? g) (error name " not in graph")]
        [(string=? (node-name (first g)) name) (node-neighbours (first g))]
        [else (neighbours name (rest g))]))

;; ============================================================
;; Část 2 — DFS post-order topologický sort
;; ============================================================

;; Myšlenka: při DFS přidám vrchol do výsledku TEPRVE POTÉ,
;; co zpracuji všechny jeho sousedy rekurzivně.
;;
;; To zaručuje, že všichni "nástupci" přijdou v acc dříve než já.
;; Po (cons name acc*) je tedy name na správném místě.
;;
;; Klíčová otázka: proč (cons name acc*) a ne (append acc* (list name))?
;; Zkuste si projít trace pro "zaklady" a uvidíte rozdíl!

; String [List-of String] -> Natural
; Vrátí index první occurrence name v xs.
(define (index-of name xs)
  (local ((define (go ys i)
            (cond
              [(empty? ys) (error name " not found")]
              [(string=? name (first ys)) i]
              [else (go (rest ys) (+ i 1))])))
    (go xs 0)))

; [List-of String] Graph -> Boolean
; Je ordering validní topologické pořadí grafu g?
(define (valid-topo-order? ordering g)
  (and (= (length ordering) (length g))
       (andmap (lambda (n) (member (node-name n) ordering)) g)
       (andmap (lambda (n)
                 (andmap (lambda (nbr)
                           (< (index-of (node-name n) ordering)
                              (index-of nbr ordering)))
                         (node-neighbours n)))
               g)))

; Graph -> [List-of String]
; Topologicky seřadí vrcholy g pomocí DFS post-order.
; Předpokládá, že g je DAG (acyklický).
(check-expect (valid-topo-order? (topo-sort/dfs prereq-graph) prereq-graph)
              #true)
(define (topo-sort/dfs g)
  (local (; String [List-of String] -> [List-of String]
          ; Post-order DFS z name; acc = vrcholy seřazené ZA name.
          ; Invariant: name ∈ acc ↔ name a celý jeho podgraf jsou zpracovány.
          (define (topo-from name acc)
            (cond
              [(member name acc) acc]
              [else
               (local ((define acc*
                         (foldl (lambda (n a) (topo-from n a))
                                acc
                                (neighbours name g))))
                 (cons name acc*))])))
    (foldl (lambda (n acc) (topo-from (node-name n) acc))
           '()
           g)))

;; Co se stane na cyklickém grafu?
;;
;; Při průchodu prereq-graph s cyklem (prog2 → zaklady):
;;   topo-from "zaklady" navštíví prog2, prog2 navštíví zaklady znovu.
;;   Jenže "zaklady" ještě NENÍ v acc — je "rozpracovaný"!
;;   → nekonečná rekurze.
;;
;; Problém: acc sleduje jen "hotové" vrcholy.
;;          Nedokáže rozlišit "ještě nezačato" od "právě zpracovávám".
;;          Proto DFS topo sort bez dalšího vylepšení na cyklech selže.
;;
;; Řešení → Kahnův algoritmus níže.

;; ============================================================
;; Část 3 — Kahnův algoritmus (worklist + in-degrees)
;; ============================================================

;; In-degree vrcholu = počet hran vedoucích DO něj
;;   = počet "závislostí", které musí přijít dřív.
;;
;; Klíčové pozorování:
;;   Vrcholy s in-degree 0 nemají žádné nesplněné závislosti
;;   → mohou přijít jako první v topologickém pořadí.
;;
;; Algoritmus:
;;   1. Spočítej in-degrees všech vrcholů.
;;   2. Dej do fronty (worklist) všechny vrcholy s in-degree 0.
;;   3. Opakuj:
;;        a) Vezmi vrchol z fronty, přidej ho do výsledku.
;;        b) Pro každého jeho souseda dekrementuj in-degree.
;;        c) Pokud sousedovi klesne in-degree na 0, přidej ho do fronty.
;;   4. Pokud |výsledek| < |vrcholy|: zbývající vrcholy tvoří cyklus → #false.

;; Degrees je [List-of (list String Number)]
;; Asociační seznam: vrchol → aktuální in-degree

; Graph -> Degrees
; Spočítá in-degree každého vrcholu grafu.
(check-expect (initial-degrees prereq-graph)
              (list (list "zaklady" 0) (list "linalg" 1) (list "prog1" 1)
                    (list "prog2" 1)   (list "algo" 2)))
(define (initial-degrees g)
  (foldl (lambda (n degs)
           (foldl (lambda (nbr d)
                    (map (lambda (entry)
                           (if (string=? (first entry) nbr)
                               (list (first entry) (+ 1 (second entry)))
                               entry))
                         d))
                  degs
                  (node-neighbours n)))
         (map (lambda (n) (list (node-name n) 0)) g)
         g))

; Graph -> [Maybe [List-of String]]
; Topologicky seřadí vrcholy g pomocí Kahnova algoritmu.
; Vrátí #false pokud g obsahuje cyklus.
(check-expect (valid-topo-order? (topo-kahn prereq-graph) prereq-graph)
              #true)
(check-expect (topo-kahn cyclic-prereq) #false)
(define (topo-kahn g)
  (local (; String Degrees -> Number
          (define (get-degree name degs)
            (cond [(empty? degs) 0]
                  [(string=? (first (first degs)) name) (second (first degs))]
                  [else (get-degree name (rest degs))]))

          ; [List-of String] Degrees -> Degrees
          ; Dekrementuje in-degree pro každý vrchol v names.
          (define (decrement-all names degs)
            (map (lambda (entry)
                   (if (member (first entry) names)
                       (list (first entry) (- (second entry) 1))
                       entry))
                 degs))

          ; [List-of String] [List-of String] Degrees -> [Maybe [List-of String]]
          ; Akumulátor result: vrcholy v topologickém pořadí dosud zpracované.
          ; Akumulátor degs: aktuální in-degrees zbývajících vrcholů.
          ; Invariant: všechny vrcholy ve frontě mají in-degree 0.
          (define (kahn queue result degs)
            (cond
              [(empty? queue)
               ;; Pokud jsme zpracovali všechny vrcholy — hotovo.
               ;; Jinak v grafu zbyla komponenta s cyklem.
               (if (= (length result) (length g)) result #false)]
              [else
               (local ((define current (first queue))
                       (define nbrs    (neighbours current g))
                       (define new-degs (decrement-all nbrs degs))
                       (define newly-ready
                         (filter (lambda (n) (= 0 (get-degree n new-degs)))
                                 nbrs)))
                 (kahn (append (rest queue) newly-ready)
                       (append result (list current))
                       new-degs))]))

          (define degs0   (initial-degrees g))
          (define queue0  (filter (lambda (n) (= 0 (get-degree n degs0)))
                                  (map node-name g))))
    (kahn queue0 '() degs0)))


;; ============================================================
;; Shrnutí: srovnání obou přístupů
;; ============================================================

;;  DFS post-order           Kahnův algoritmus
;;  ──────────────────────   ──────────────────────────────
;;  akumulátor: hotové       akumulátory: výsledek + in-degrees
;;  nevytvoří výsledek       přirozeně detekuje cykly
;;  na cyklickém grafu       (|výsledek| < |vrcholy|)
;;  rozšíření find-path      rozšíření BFS/worklist z lekce 20
;;
;;  Oba algoritmy jsou generativní rekurze:
;;    generate: sousedé vrcholu
;;    trivial:  vrchol je hotový / fronta je prázdná


;; ============================================================
;; Procvičovací úlohy
;; ============================================================

;; 1) DFS topo sort má problém s cykly. Jak ho opravit?
;;    Nápověda: přidejte třetí stav vrcholu — "právě zpracovávám"
;;    (šedé vrcholy z klasického DFS). Pokud narazíme na šedý vrchol,
;;    detekovali jsme cyklus.

;; 2) Napište all-topo-sorts : Graph -> [List-of [List-of String]]
;;    která vrátí VŠECHNA validní topologická pořadí.
;;    Nápověda: Kahnův algoritmus — místo jednoho vrcholu z fronty
;;    vyzkoušejte každý.

;; 3) Napište is-valid-topo? : [List-of String] Graph -> Boolean
;;    která ověří, zda je daná permutace vrcholů validním
;;    topologickým pořadím pro graph g.

;; 4) Rozšiřte prereq-graph o reálné předměty vašeho gymnázia
;;    (nebo oblíbeného studia) a spusťte topo-kahn.
;;    Jak vypadá výsledné pořadí?
