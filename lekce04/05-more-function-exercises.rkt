;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 05-more-function-exercises) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; =============================================================================
;; CVIČENÍ 1: Výpočet nákladů na cloudovou infrastrukturu
;; =============================================================================
;; Startup potřebuje vypočítat měsíční náklady na cloud infrastructure.
;; Cena se skládá z:
;; - Základní poplatek za službu: $150 měsíčně
;; - Poplatek za výpočetní výkon: $0.08 za každou CPU hodinu
;; - Poplatek za úložiště: $0.12 za každý GB měsíčně
;; - Síťový poplatek: 3% z celkové částky (před připočtením síťového poplatku)
;;
;; Napište funkci (cloud-cost cpu-hours storage-gb) která vypočítá celkové
;; měsíční náklady na cloud infrastrukturu.
;;




;; =============================================================================
;; CVIČENÍ 2: Výpočet výkonu procesoru
;; =============================================================================
;; Výkonnostní tým měří výkon procesoru při různých frekvencích a teplotách.
;; Zjistili následující závislosti:
;;
;; Základní výkon procesoru při 3.0 GHz a 50°C je 1000 bodů (benchmark score).
;;
;; Vliv frekvence:
;; - Za každých 0.1 GHz nad 3.0 GHz se výkon zvýší o 25 bodů
;; - Za každých 0.1 GHz pod 3.0 GHz se výkon sníží o 25 bodů
;;
;; Vliv teploty (throttling):
;; - Za každý °C nad 50°C se výkon sníží o 3 body (procesor throttluje)
;; - Za každý °C pod 50°C se výkon zvýší o 2 body (lepší chlazení)
;;
;; Napište funkci (cpu-performance frequency-ghz temperature-celsius) která
;; vypočítá celkový výkon procesoru v bodech.
;;



