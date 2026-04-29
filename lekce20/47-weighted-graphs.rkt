;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 47-weighted-graphs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Ohodnocené grafy a nejlevnější cesta

;; V lekci 19 jsme se naučili najít NĚJAKOU cestu v grafu pomocí backtrackingu.
;; Reálné problémy ale mají cenu:
;;   - GPS navigace:         minimalizuj vzdálenost nebo čas jízdy
;;   - Počítačové sítě:      minimalizuj zpoždění paketů (network routing)
;;   - Letecká doprava:      minimalizuj cenu letenky
;;   - Zásobovací řetězce:   minimalizuj náklady na přepravu zboží

;; Zavedeme OHODNOCENÉ grafy — každá hrana nese nezápornou cenu (weight).

;; ============================================================
;; Část 1 — Datová definice
;; ============================================================

(define-struct edge [to cost])
; Edge je (make-edge String Number)
; Reprezentuje orientovanou hranu vedoucí do vrcholu 'to' s cenou 'cost'.
; Omezení: cost >= 0

; WNode je (make-wnode String [List-of Edge])
(define-struct wnode [name edges])

; WGraph je [List-of WNode]

;; ============================================================
;; Část 2 — Ukázkový graf a klíčová otázka
;; ============================================================

;; Graf silnic. Čísla = vzdálenosti v km.
;;
;;        (10)
;;   A ---------> B
;;   |            |
;;  (1)          (1)
;;   |            |
;;   v            v
;;   C ---(1)---> B  (hrana C→B)
;;                |
;;               (1)
;;                v
;;                D
;;
;; Zjednodušeně:
;;
;;   A ---(10)---> B ---(1)---> D
;;   |             ^
;;   +---(1)--> C -+(1)
;;
;; Otázka: jaká je nejkratší cesta z A do D?
;;
;;   Podle počtu hran:  A → B → D       (2 hrany,  cena 11)
;;   Podle ceny:        A → C → B → D   (3 hrany,  cena  3)
;;
;; BFS a DFS ignorují váhy hran a mohou vrátit "špatnou" cestu!

(define wgraph
  (list (make-wnode "A" (list (make-edge "B" 10) (make-edge "C" 1)))
        (make-wnode "B" (list (make-edge "D" 1)))
        (make-wnode "C" (list (make-edge "B" 1)))
        (make-wnode "D" '())))

; String WGraph -> [List-of Edge]
; Vrátí seznam hran vedoucích z vrcholu name.
(check-expect (wedge-neighbours "A" wgraph)
              (list (make-edge "B" 10) (make-edge "C" 1)))
(check-expect (wedge-neighbours "D" wgraph) '())
(define (wedge-neighbours name g)
  (cond
    [(empty? g) (error name " not in graph")]
    [(string=? (wnode-name (first g)) name) (wnode-edges (first g))]
    [else (wedge-neighbours name (rest g))]))


;; ============================================================
;; Část 3 — Worklist (fronta) jako akumulátor: BFS
;; ============================================================

;; Připomeňme DFS z lekce 19: prozkoumáváme "do hloubky" pomocí call stacku.
;; BFS prozkoumává "do šířky" pomocí fronty (queue):
;;   - DFS: sousedy přidám DOPŘEDU fronty  → zásobník (LIFO) → hloubka
;;   - BFS: sousedy přidám DOZADU fronty   → fronta   (FIFO) → šířka

;; BFS zaručuje nejkratší cestu měřenou POČTEM HRAN — ignoruje ceny.
;; V této lekci nás zajímá hlavně myšlenka worklistu.
;; Naše reprezentace fronty je obyčejný seznam, takže implementace není
;; asymptoticky optimální, ale je dobře čitelná.

; Path je [List-of String]

; BFSItem je (list String Path)
; jméno vrcholu + cesta z origin do tohoto vrcholu

; String String WGraph -> [Maybe Path]
(check-expect (find-path/bfs "A" "D" wgraph)
              (list "A" "B" "D"))   ; 2 hrany — ale cena 11, ne nejlevnější!
(check-expect (find-path/bfs "A" "A" wgraph)
              (list "A"))
(check-expect (find-path/bfs "B" "A" wgraph) #false)
(define (find-path/bfs origin dest g)
  ; [List-of BFSItem] [List-of String] -> [Maybe Path]
  ; Akumulátor queue: vrcholy čekající v pořadí FIFO.
  ; Akumulátor visited: vrcholy, jejichž sousedy jsme již přidali.
  (local ((define (bfs queue visited)
            (cond
              [(empty? queue) #false]
              [else
               (local ((define name (first (first queue)))
                       (define path (second (first queue))))
                 (cond
                   [(string=? name dest) path]
                   [(member name visited)
                    (bfs (rest queue) visited)]
                   [else
                    (local ((define new-items
                              (map (lambda (e)
                                     (list (edge-to e)
                                           (append path (list (edge-to e)))))
                                   (wedge-neighbours name g))))
                      ;; Nové vrcholy jdou DOZADU fronty → BFS
                      (bfs (append (rest queue) new-items)
                           (cons name visited)))]))])))
    (bfs (list (list origin (list origin))) '())))


;; ============================================================
;; Část 4 — Dijkstrův algoritmus
;; ============================================================

;; Dijkstra = BFS, ale worklist je seřazen podle CENY dosud nalezené cesty.
;; Tím zaručíme, že vždy zpracujeme nejlevnější dosud čekající variantu jako první.
;;
;; Generativní struktura zůstává stejná — mění se jen způsob řazení fronty.
;; Opět používáme obyčejný seznam a `sort`; v produkční implementaci by se
;; použila prioritní fronta.

; DijkItem je (make-dijk-item String Path Number)
; jméno vrcholu + cesta z origin + celková cena cesty
(define-struct dijk-item [name path cost])

; String String WGraph -> [Maybe (list Path Number)]
; Najde nejlevnější cestu z origin do dest; vrátí (list cesta cena) nebo #false.
(check-expect (find-cheapest "A" "D" wgraph)
              (list (list "A" "C" "B" "D") 3))  ; 3 hrany, cena 3
(check-expect (find-cheapest "B" "A" wgraph) #false)
(check-expect (find-cheapest "A" "A" wgraph)
              (list (list "A") 0))
(define (find-cheapest origin dest g)
  ; [List-of DijkItem] [List-of String] -> [Maybe (list Path Number)]
  ; Akumulátor visited: vrcholy "uzavřené" — jejich optimální cena je finální.
  ; Invariant: první prvek queue má vždy nejnižší cost ze všech čekajících.
  (local ((define (dijkstra queue visited)
            (cond
              [(empty? queue) #false]
              [else
               (local ((define item (first queue))
                       (define name (dijk-item-name item))
                       (define path (dijk-item-path item))
                       (define cost (dijk-item-cost item)))
                 (cond
                   [(string=? name dest) (list path cost)]
                   [(member name visited)
                    (dijkstra (rest queue) visited)]
                   [else
                    (local ((define new-items
                              (map (lambda (e)
                                     (make-dijk-item
                                      (edge-to e)
                                      (append path (list (edge-to e)))
                                      (+ cost (edge-cost e))))
                                   (wedge-neighbours name g)))
                             ;; Klíčový rozdíl oproti BFS: seřadíme frontu podle ceny!
                             (define new-queue
                               (sort (append (rest queue) new-items)
                                     (lambda (a b)
                                       (< (dijk-item-cost a) (dijk-item-cost b))))))
                      (dijkstra new-queue (cons name visited)))]))])))
    (dijkstra (list (make-dijk-item origin (list origin) 0)) '())))


;; ============================================================
;; Shrnutí: srovnání algoritmů
;; ============================================================

;; Všechny tři algoritmy sdílejí stejný generativní skelet:
;;   trivial:   cíl dosažen → vrátit výsledek
;;   generate:  vzít první z fronty, vygenerovat sousedy jako nové položky
;;   combine:   přidat nové položky do fronty
;;
;; Liší se hlavně způsobem organizace worklistu:
;;
;;   DFS       zásobník (cons)          → nějaká cesta
;;   BFS       fronta (append)          → nejkratší podle hran
;;   Dijkstra  řazení podle g(v)        → nejlevnější podle ceny
;;
;; Teoreticky při vhodných datových strukturách:
;;   DFS/BFS   O(V + E)
;;   Dijkstra  O(E log V)
;;
;; Pozor: tento konkrétní výukový kód používá seznamy, `append` a `sort`,
;; takže praktická složitost je horší. To je v pořádku — cílem je zde
;; pochopit princip algoritmu.


;; ============================================================
;; Část 5 — Kam dál: A*
;; ============================================================

;; Dijkstra prozkoumá vrcholy "na slepo" — nevyužívá žádnou znalost
;; o tom, kde se cíl nachází.
;;
;; Představte si navigaci na mapě: víme, že Praha leží na západ od Brna.
;; Má smysl zkoumat cesty vedoucí na východ?
;;
;; A* rozšiřuje Dijkstru o HEURISTIKU h : String -> Number,
;; která odhaduje "vzdálenost k cíli" z daného vrcholu.
;;
;; Fronta se pak řadí podle  f(v) = g(v) + h(v) , kde:
;;   g(v) = skutečná cena cesty z origin do v  (Dijkstrova složka)
;;   h(v) = heuristický odhad ceny z v do dest (nová složka)
;;
;; Dijkstra je speciální případ A* s h(v) = 0 pro všechna v.
;;
;; Podmínka správnosti: heuristika musí být PŘÍPUSTNÁ —
;; h(v) nesmí nikdy přeceňovat skutečnou cenu.
;; Příklad přípustné heuristiky na mapě: vzdušná vzdálenost,
;; protože vzdušná vzdálenost ≤ silniční vzdálenost vždy platí.
;;
;; Změna v kódu oproti Dijkstrovi je jediná — řádek se sort:
;;
;;   Dijkstra:
;;     (lambda (a b) (< (dijk-item-cost a)
;;                      (dijk-item-cost b)))
;;
;;   A*:
;;     (lambda (a b) (< (+ (dijk-item-cost a) (h (dijk-item-name a)))
;;                      (+ (dijk-item-cost b) (h (dijk-item-name b)))))


;; ============================================================
;; Procvičovací úlohy
;; ============================================================

;; 1) Přidejte do wgraph nové hrany a ověřte, že find-cheapest stále
;;    vrátí správný výsledek (porovnejte ručně s výpočtem).

;; 2) Napište find-all-paths : String String WGraph -> [List-of (list Path Number)]
;;    která vrátí VŠECHNY cesty seřazené od nejlevnější.
;;    Nápověda: místo ukončení při nalezení cíle pokračujeme dál.

;; 3) Mapa ČR: definujte wgraph pro 6 měst (Praha, Brno, Ostrava,
;;    Plzeň, Liberec, Olomouc) se skutečnými vzdálenostmi v km
;;    (cesta autem po dálnici).
;;    Ověřte, že find-cheapest najde správnou trasu.
;;
;;    Graf je záměrně ŘÍDKÝ — ne každé dvě města jsou přímo spojena

;; 4) (Bonus) Implementujte A* pro mapu z úlohy 3.
;;    Jako heuristiku použijte vzdušnou vzdálenost k cíli —
;;    nadefinujte souřadnice měst a vypočítejte ji pomocí
;;    odmocniny ze součtu čtverců rozdílů souřadnic.
;;    (sqrt je v ISL+ dostupný.)

;; 5) (Těžší) Dijkstra může mít ve frontě tentýž vrchol vícekrát
;;    s různými cenami. Jak byste "odpadní" záznamy minimalizovali?
;;    Nápověda: uchovávejte v visited i nejlepší dosud nalezenou cenu.
