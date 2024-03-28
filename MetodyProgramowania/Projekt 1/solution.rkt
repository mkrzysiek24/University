#lang racket

(provide (struct-out column-info)
         (struct-out table)
         (struct-out and-f)
         (struct-out or-f)
         (struct-out not-f)
         (struct-out eq-f)
         (struct-out eq2-f)
         (struct-out lt-f)
         table-insert
         table-project
         table-sort
         table-select
         table-rename
         table-cross-join
         table-natural-join)

(define-struct column-info (name type) #:transparent)

(define-struct table (schema rows) #:transparent)

(define cities
  (table
   (list (column-info 'city    'string)
         (column-info 'country 'string)
         (column-info 'area    'number)
         (column-info 'capital 'boolean))
   (list (list "Wrocław" "Poland"  293 #f)
         (list "Warsaw"  "Poland"  517 #t)
         (list "Poznań"  "Poland"  262 #f)
         (list "Berlin"  "Germany" 892 #t)
         (list "Munich"  "Germany" 310 #f)
         (list "Paris"   "France"  105 #t)
         (list "Rennes"  "France"   50 #f))))

(define countries
  (table
   (list (column-info 'country 'string)
         (column-info 'population 'number))
   (list (list "Poland" 38)
         (list "Germany" 83)
         (list "France" 67)
         (list "Spain" 47))))

(define (empty-table columns) (table columns '()))

; sprawdza czy x należy do listy

(define (member x lst)
  (cond [(empty? lst) #f]
        [(equal? (first lst) x) #t]
        [else (member x (rest lst))]))

;; Append

(define (my-append xs ys)
  (foldr cons ys xs))


; Sprawdzanie typu

(define (check-type x)
  (cond [(number? x) 'number]
        [(string? x) 'string]
        [(symbol? x) 'symbol]
        [(boolean? x) 'boolean]))

;<><><><><><><><><><><><><><><><><><><><><><><><><><>Table-Insert<><><><><><><><><><><><><><><><><><><><><><><><><>

; Sprawdzanie czy wiersz odpowiada schematowi

(define (check-insert row schema)
  (cond [(and (empty? row) (empty? schema)) #t]
        [(empty? row) #f]
        [(empty? schema) #f]
        [(equal? (check-type (first row)) (column-info-type (first schema))) (check-insert (rest row) (rest schema))] ;wywolanie rekurencyjne sprawdza za kazdym wywolaniem inna kolumne
        [else #f]))

;Wstawianie

(define (table-insert row tab)
  (if (check-insert row (table-schema tab))
      (table (table-schema tab) (cons row (table-rows tab)))
      (error "Zle wstawienie")))

;<><><><><><><><><><><><><><><><><><><><><><><><><><>Table-Project<><><><><><><><><><><><><><><><><><><><><><><><><>

; Dodawanie kolumny col1 do zbioru kolumn col2
(define (column-insert col1 col2)
  (cond [(empty? col1) '()]
        [(empty? col2) (cons (cons (first col1) '()) (column-insert (rest col1) '()))]
        [else (cons (cons (first col1) (first col2)) (column-insert (rest col1) (rest col2)))]))

;Zwraca tabele wierszy tylko z kolumnami cols

(define (table-project-rows cols schema rows)
  (cond [(empty? schema) (map (lambda x '()) rows)]
        [(member (column-info-name (first schema)) cols) ; jesli pierwszy element schemy nalezy do cols 
         (column-insert (map (lambda x (first (first x))) rows)
                        (table-project-rows cols (rest schema) (map (lambda x (rest (first x))) rows)))]
        [else (table-project-rows cols (rest schema) (map (lambda x (rest (first x))) rows))]))

;Zwraca schemat tylko z kolumnami zawartymi w cols

(define (table-project-schema cols schema)
  (cond [(empty? schema) '()]
        [(member (column-info-name (first schema)) cols) ; jesli pierwszy element schemy nalezy do cols
         (cons (first schema) (table-project-schema cols (rest schema)))] ; laczymy element pierwszy schemy z wywolaniem schemy mniejszej o pierwszy element
        [else (table-project-schema cols (rest schema))]))

(define (table-project cols tab)
  (table (table-project-schema cols (table-schema tab))
         (table-project-rows cols (table-schema tab) (table-rows tab))))

;<><><><><><><><><><><><><><><><><><><><><><><><><><>Table-Sort<><><><><><><><><><><><><><><><><><><><><><><><><>><><><>

;Struktura przetrzymujaca informacje o elementach po ktorych bedziemy sortowac

(define-struct index-info (index type) #:transparent)

;funkcja, ktora pobiera typ elementu do sortowania i wywoluje funkcje odpowiednia dla danego typu

(define (compare-func-calling x y indexes)
  (if (empty? indexes)
      #f
      (let [(type (index-info-type (first indexes)))]
        (cond 
          [(equal? 'string type) (compare-string x y indexes)]
          [(equal? 'number type) (compare-number x y indexes)]
          [(equal? 'boolean type) (compare-boolean x y indexes)]
          [(equal? 'symbol type) (compare-number x y indexes)]))))

;Funkcje porownawcze dla kazdego typu

(define (compare-number x y indexes)
  (let [(index (index-info-index (first indexes)))]
    (cond [(equal? (list-ref x index) (list-ref y index)) (compare-func-calling x y (rest indexes))]
          [(< (list-ref x index) (list-ref y index)) #t]
          [else #f])))

(define (compare-boolean x y indexes)
  (let [(index (index-info-index (first indexes)))]
  (cond [(equal? (list-ref x index) (list-ref y index)) (compare-func-calling x y (rest indexes))]
        [(equal? (list-ref x index) #f) #t]
        [else #f])))

(define (compare-string x y indexes)
  (let [(index (index-info-index (first indexes)))]
  (cond [(equal? (list-ref x index) (list-ref y index)) (compare-func-calling x y (rest indexes))]
        [(string<? (list-ref x index) (list-ref y index)) #t]
        [else #f])))

(define (compare-symbol x y indexes)
  (let [(index (index-info-index (first indexes)))]
  (cond [(equal? (list-ref x index) (list-ref y index)) (compare-func-calling x y (rest indexes))]
        [(symbol<? (list-ref x index) (list-ref y index)) #t]
        [else #f])))

;Funkcja, ktora zwraca funkcje dwuargumentowa porownujaca te dwa elementy

(define (compare-func indexes)
  (lambda (x y) (compare-func-calling x y indexes)))

;Funkcja zwracajaca (index-info index type) jesli istnieje columna o takiej nazwie, w przeciwnym wypadku #f

(define (index col schema)
  (define (pom num col schema)
    (cond [(empty? schema) #f]
          [(equal? col (column-info-name (first schema))) (index-info num (column-info-type (first schema)))]
          [else (pom (+ num 1) col (rest schema))]))
  (pom 0 col schema))

;Funkcja zwracajaca liste indeksow w postaci ((index-info index type) ... )

(define (indexes-func cols schema)
  (if (empty? cols)
      '()
      (let ([index (index (first cols) schema)])
        (if (equal? index #f)
            (indexes-func (rest cols) schema)
            (cons index (indexes-func (rest cols) schema))))))

(define (table-sort cols tab)
  (table (table-schema tab) (sort (table-rows tab) (compare-func (indexes-func cols (table-schema tab)))))) ;korzystamy z sorta wbudowanego

;<><><><><><><><><><><><><><><><><><><><><><><><><><>Table-Select<><><><><><><><><><><><><><><><><><><><><><><><><>><><><>

;Definicja struktur

(define-struct and-f (l r))
(define-struct or-f (l r))
(define-struct not-f (e))
(define-struct eq-f (name val))
(define-struct eq2-f (name name2))
(define-struct lt-f (name val))

;Odczytywanie formuly

(define (formula row form name-schema)
  (cond [(and-f? form) (and (formula row (and-f-l form) name-schema) (formula row (and-f-r form) name-schema))]
        [(or-f? form) (or (formula row (or-f-l form) name-schema) (formula row (or-f-r form) name-schema))]
        [(not-f? form) (not (formula row (not-f-e form) name-schema))]
        [(eq-f? form) (equal? (eq-f-val form) (list-ref row (index-of name-schema (eq-f-name form))))]
        [(eq2-f? form) (equal? (list-ref row (index-of name-schema (eq2-f-name form))) (list-ref row (index-of name-schema (eq2-f-name2 form))))]
        [(lt-f? form) (let [(value (list-ref row (index-of name-schema (lt-f-name form))))]
                   (cond [(number? value) (< value (lt-f-val form))]
                         [(string? value) (string<? value (lt-f-val form))]
                         [(symbol? value) (symbol<? value (lt-f-val form))]
                         [(boolean? value) (if (and (equal? value #f) (equal? (lt-f-val form) #t))
                                               #t
                                               #f)]))]))

;Tworzenie funkcji jednoargumentowej, ktora sprawdza czy wiersz spelnia warunki formuly

(define (formula-to-func form tab)
  (lambda (row) (if (formula row form (map (lambda x (column-info-name (first x))) (table-schema tab)))
                    row
                    #f)))

;Selekcja

(define (table-select form tab)
  (define (table-select-rows form rows)
    (if (empty? rows)
        '()
        (let [(row (form (first rows)))]
          (if (equal? row #f)
              (table-select-rows form (rest rows)) ;jesli wiersz nie spelnia formuly wywolujemy funkcje table-select-rows dla reszty wierszy
              (cons row (table-select-rows form (rest rows))))))) ;jesli wiersz spelnia formule to dodajemy ten wiersz i wywolujemy table-select-rows dla reszty wierszy
  (table (table-schema tab) (table-select-rows (formula-to-func form tab) (table-rows tab))))


;<><><><><><><><><><><><><><><><><><><><><><><><><><>Table-Rename<><><><><><><><><><><><><><><><><><><><><><><><><>><><><>

;Zmiana nazwy kolumny w schemie

(define (table-rename-schema col ncol schema)
  (cond [(not (symbol? ncol)) (error "Zly typ wartosci na ktora chcemy zmienic")] ;Sprawdzamy czy ncol to symbol
        [(empty? schema) (error "Nie istnieje taka kolumna")] 
        [(equal? (column-info-name (first schema)) col ) (cons (column-info ncol (column-info-type (first schema))) (rest schema))] ; Jesli znalezlismy kolumne o tej samej nazwie to dodajemy zmieniona kolejne i reszte schemy 
        [else (cons (first schema) (table-rename-schema col ncol (rest schema)))])) ; Dodajemy pierwszy element schemy i wywolujemy funkcje dla reszty schemy

;Zmiana nazwy kolumny w tabeli

(define (table-rename col ncol tab)
  (table (table-rename-schema col ncol (table-schema tab)) (table-rows tab)))


;<><><><><><><><><><><><><><><><><><><><><><><><><><>Table-Cross-Join<><><><><><><><><><><><><><><><><><><><><><><><><>><><><>

;Dodawanie schema1 i schema2 do siebeie

(define (table-cross-join-schema schema1 schema2) ; Wywoluje rekurencyjnie po elementach schema1, jak skoncza sie elementy to dodaje na koniec schema2
  (cond [(empty? schema1) schema2]
        [else (cons (first schema1) (table-cross-join-schema (rest schema1) schema2))]))

;Funkcja dodaje wiersz x na koniec wiersza row

(define (join-row row x)
  (cond [(empty? row) (first x)]
        [else (cons (first row) (join-row (rest row) x))]))

;Funkcja zwracjaca konkatentacje wierszy rows1 z rows2

(define (table-cross-join-rows rows1 rows2)
  (cond [(empty? rows1) '()]
        [(empty? rows2) '()]
        [else (my-append (map (lambda x (join-row (first rows1) x)) rows2) ; Dodajemy do pierwszego wiersza rows1 wszystkie wierzsze rows2
                         (table-cross-join-rows (rest rows1) rows2))])) ; Wywolujemy funkcje dla reszty rows1

;Zlaczenie kartezjanskie

(define (table-cross-join tab1 tab2)
  (table (table-cross-join-schema (table-schema tab1) (table-schema tab2))
         (table-cross-join-rows (table-rows tab1) (table-rows tab2))))


;<><><><><><><><><><><><><><><><><><><><><><><><><><>Table-Natural-Join<><><><><><><><><><><><><><><><><><><><><><><><><>><><><>
;<><><><><><><><><><><><><><><><><><><><><><><><><><><><Zaawansowana><><><><><><><><><><><><><><><><><><><><><><><><>><><><>

; Funkcja sprawdzajaca typ i wywolujaca odpowiednie funkcje porownawcze
; Funkcja ta zostala przepisana odrobine wzgledem sorta.
; Nie wykorzystalem funkcji porownawczej z sorta, poniewaz w odroznieniu do sorta potrzebowalem funkcji, ktora zwraca mniejszosc, rownosc oraz wiekszosc

(define (compare-func-calling2 x y indexes)
  (if (empty? indexes)
      0 ; Jesli indexes empty to x y rowne
      (let [(type (check-type (list-ref x (duplicate-info-index1 (first indexes)))))]
        (cond [(equal? 'string type) (compare-string2 x y indexes)]
              [(equal? 'number type) (compare-number2 x y indexes)]
              [(equal? 'boolean type) (compare-boolean2 x y indexes)]
              [(equal? 'symbol type) (compare-number2 x y indexes)]))))

;Porownanie number

(define (compare-number2 x y indexes)
  (let [(val1 (list-ref x (duplicate-info-index1 (first indexes))))
        (val2 (list-ref y (duplicate-info-index2 (first indexes))))]
  (cond [(equal? val1 val2) (compare-func-calling2 x y (rest indexes))]
        [(< val1 val2) -1]
        [else 1])))

;Porownanie boolean

(define (compare-boolean2 x y indexes)
  (let [(val1 (list-ref x (duplicate-info-index1 (first indexes))))
        (val2 (list-ref y (duplicate-info-index2 (first indexes))))]
  (cond [(equal? val1 val2) (compare-func-calling2 x y (rest indexes))]
        [(equal? val1 #f) -1]
        [else 1])))

;Porownanie string

(define (compare-string2 x y indexes)
  (let [(val1 (list-ref x (duplicate-info-index1 (first indexes))))
        (val2 (list-ref y (duplicate-info-index2 (first indexes))))]
  (cond [(equal? val1 val2) (compare-func-calling2 x y (rest indexes))]
        [(string<? val1 val2) -1]
        [else 1])))

;Porownanie symbol

(define (compare-symbol2 x y indexes)
  (let [(val1 (list-ref x (duplicate-info-index1 (first indexes))))
        (val2 (list-ref y (duplicate-info-index2 (first indexes))))]
  (cond [(equal? val1 val2) (compare-func-calling2 x y (rest indexes))]
        [(symbol<? val1 val2) -1]
        [else 1])))

; Funkcja grupujaca wiersze (Rowne na kolumnach zduplikowanych) w listy
; Na wyjsciu lista pogrupowanych wierszy

(define (grouping indexes tab)
  (define (group-pom group rest-tab)
  (cond [(empty? rest-tab) '()]
        [(or (empty? (rest rest-tab)) (not (duplicate-equal (first rest-tab) (second rest-tab) indexes))) (cons (cons (first rest-tab) group) (grouping indexes (rest rest-tab)))]
        [else (group-pom (cons (first rest-tab) group) (rest rest-tab))]))
  (group-pom '() tab))

; Funkcja zwracajaca index jesli w liscie lst zawiera sie x, w przeciwnym wypadku #f

(define (member-index x lst)
  (define (member-pom num lst)
    (cond [(empty? lst) #f]
          [(equal? (first lst) x) num]
          [else (member-pom (+ num 1) (rest lst))]))
  (member-pom 0 lst))

; Struktura na kolumny zduplikowane

(define-struct duplicate-info (name index1 index2) #:transparent)

; Funkcja zwracajaca liste duplicatow w postaci (list (duplicate-info nazwa_kolumny index_w_1_tabeli index_w_2_tabeli)...)

(define (duplicate tab1 tab2)
  (define (duplicate-pom num names-tab1 names-tab2)
    (if (empty? names-tab1)
        '()
        (let [(index (member-index (first names-tab1) names-tab2))]
          (if index
              (cons (duplicate-info (first names-tab1) num index ) (duplicate-pom (+ num 1) (rest names-tab1) names-tab2))
              (duplicate-pom (+ num 1) (rest names-tab1) names-tab2)))))
  (duplicate-pom 0 (map (lambda x (column-info-name (first x))) (table-schema tab1)) (map (lambda x (column-info-name (first x))) (table-schema tab2))))

; Funkcja sprawdza czy 2 wiersze sa rowne na kolumnach zduplikowanych

(define (duplicate-equal row1 row2 indexes)
  (if (empty? indexes)
      #t
      (let [(index (first indexes))]
        (if (equal? (list-ref row1 index) (list-ref row2 index))
            (duplicate-equal row1 row2 (rest indexes))
            #f))))

; Funkcja dla dwoch list, gdzie kazda posiada elementy rowne w kolumnach zduplikowanych
; Porownuje pierwszy element listy group1 z pierwszym elementem listy group2

(define (compare group1 group2 duplicate)
  (compare-func-calling2 (first group1) (first group2) duplicate))

;Funkcja generujaca wiersze table-natural-join 

(define (join duplicate tab1 tab2)
  (if (or (empty? tab1) (empty? tab2))
      '()
      (let [(comp (compare (first tab1) (first tab2) duplicate))] ; Porownujemy elementy 1 grupy z tab1 oraz 1 grupy z tab2
        (cond [(equal? comp 0) (my-append (table-cross-join-rows (first tab1) (first tab2)) ; Jesli comp 0 to kolumny zduplikowane w 1 grupie tab1 oraz w 1 grupie tab2 sa rowne, wywolujemy wtedy table-cross-join na tych grupach
                                          (join duplicate (rest tab1) (rest tab2)))] ; wywolanie funkcji ze zmniejszonymi tab1 i tab2
              [(equal? comp 1) (join duplicate tab1 (rest tab2))] ; Jesli comp 1 to kolumny zduplikowane w 1 grupie tab1 sa wieksze od 1 grupy tab2, zmniejszamy tab2
              [else (join duplicate (rest tab1) tab2)])))) ;kolumny zduplikowane w 1 grupie tab1 sa mniejsze od 1 grupy tab2, zmniejszamy tab1

;Zmiana nazwy, dodajemy 2_ na poczatek nazwy symbolu

(define (rename-col col)
  (string->symbol (list->string (cons #\2 (cons #\_ (string->list (symbol->string col)))))))

; Funkcja zmieniajaca nazwy duplikatow w schema2 

(define (rename-duplicates key-sort schema1 schema2)
  (if (empty? key-sort)
      (append schema1 schema2)
      (rename-duplicates (rest key-sort)
                         schema1
                         (table-rename-schema (first key-sort) (rename-col (first key-sort)) schema2)))) ;; Funkcja korzysta z table-rename i wywoluje rekurencyjnie do momentu, az schema2 nie bedzie pusta. Wtedy dodaje schema1 do schema2

;Funkcja zwraca wszystkie kolumny bez duplikatow, uzywane w table-project

(define (to-project tab1 tab2)
  (remove-duplicates (my-append (map (lambda x (column-info-name (first x))) (table-schema tab1))
             (map (lambda x (column-info-name (first x))) (table-schema tab2)))))

; Złączenie

(define (table-natural-join tab1 tab2)
  (let* [(to-p (to-project tab1 tab2)) ; kolumny do table-project
         (dup (duplicate tab1 tab2)) ;lista duplikatow
         (key-sort (map (lambda x (duplicate-info-name (first x))) dup)) ;;lista nazw duplikatow
         (index1 (map (lambda x (duplicate-info-index1 (first x))) dup)) ;lista indexow duplikatow w tab1
         (index2 (map (lambda x (duplicate-info-index2 (first x))) dup))] ; lista indexow duplikatow w tab2
    (table-project to-p ; Wywolanie table-project w celu usuniecia zduplikowanych kolumn
                   (table (rename-duplicates key-sort (table-schema tab1) (table-schema tab2)) ; Zmiana nazwy kolumn zduplikowanych w schema2
                          (join dup ;; natural-join na wierszach tab1 tab2
                                         (grouping index1 (table-rows (table-sort key-sort tab1))) ;; najpierw sortujemy tabele wedlug klucza duplikatow, a pozniej grupujemy rowne wiersze w listy
                                         (grouping index2 (table-rows (table-sort key-sort tab2)))))))) ;; najpierw sortujemy tabele wedlug klucza duplikatow, a pozniej grupujemy rowne wiersze w listy

;<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
