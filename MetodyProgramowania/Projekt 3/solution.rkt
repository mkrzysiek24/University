#lang plait

;; abstract syntax -------------------------------

(define-type-alias Value Number)

(define-type Op
  (add) (sub) (mul) (leq))

(define-type Exp
  (defineE [d : (Listof Exp)] [e : Exp])
  (funE [name : Symbol] [x : (Listof Symbol)] [e : Exp])
  (numE [n : Number])
  (varE [x : Symbol])
  (opE [l : Exp] [op : Op] [r : Exp])
  (ifE [b : Exp] [l : Exp] [r : Exp])
  (letE [x : Symbol] [e1 : Exp] [e2 : Exp])
  (appE [e1 : Symbol] [e2 : (Listof Exp)]))

;; parse ----------------------------------------

;Funkcja pomocnicza do sprawdzania czy w liscie symboli nie ma powtorzen
(define (unique? [lst : (Listof Symbol)]) : (Listof Symbol)
  (cond [(empty? lst) '()]
        [(member (first lst) (rest lst)) (error 'parse-unique? "Lista symboli nie jest unikatowa")]
        [else (cons (first lst) (unique? (rest lst)))]))

;Funkcja pomocnicza wywolywana na wyrazeniu po parsowaniu, w celu sprawdzenia
;czy w wyrazeniu znajduja sie zmienne wolne lub odwolania do niezdefiniowanych funkcji
;w takich przypadkach zwracamy blad
(define (fvf-sieve [exp : Exp] [fN : (Listof Symbol)] [vN : (Listof Symbol)]) : Exp
  (type-case Exp exp
    [(numE n) (numE n)]
    [(varE x) (if (member x vN)
                  (varE x)
                  (error 'fvf-sieve "Nieznana zmienna"))]
    [(opE l op r) (opE (fvf-sieve l fN vN)
                       op
                       (fvf-sieve r fN vN))]
    [(ifE b l r) (ifE (fvf-sieve b fN vN)
                      (fvf-sieve l fN vN)
                      (fvf-sieve r fN vN))]
    [(letE x e1 e2) (letE x
                          (fvf-sieve e1 fN vN)
                          (fvf-sieve e2 fN (cons x vN)))]
    [(appE e1 e2) (appE (if (member e1 fN) e1 (error 'fvf-sieve "Nieznana funkcja"))
                        (map (λ (x) (fvf-sieve x fN vN)) e2))]
    [(funE name x e) (funE name
                           x
                           (fvf-sieve e fN (append x vN)))]
    [(defineE d e) (defineE (map (λ (x) (fvf-sieve x fN vN)) d)
                            (fvf-sieve e fN vN))]))

;Wszystko zdefiniowane w naszym jezyku zaczyna sie od define
;W funkcji parse sprawdzamy czy nasz jezyk zaczyna sie od takiej konstrukcji
(define (parse [s : S-Exp]) : Exp
  (if (s-exp-match? `{define {ANY ...} for ANY} s)
      (let* ([funcList (map parse-function (s-exp->list (second (s-exp->list s))))] ;parsujemy kazda funkcje
             [funcNames (unique? (map (λ (x) (funE-name x)) funcList))]) ;wyciagamy wszystkie nazwy funkcji, aby sprawdzic czy wystepuja w wyrazeniu odwolania do funkcji niezdefiniowanych oraz sprawdzamy ich unikatowosc
        (fvf-sieve ;przeszukujemy cale wyrazenie i sprawdzamy czy wystepuja wolne zmienne i niezdefiniowane funkcje
         (defineE funcList (parse-else (fourth (s-exp->list s))))
         funcNames
         '()))
      (error 'parse "Program nie rozpoczyna sie od poprawnej konstrukcji define")))

;Funkcja do parsowania funkcji
(define (parse-function [s : S-Exp]) : Exp
  (if (s-exp-match? `[fun SYMBOL (SYMBOL ...) = ANY] s) ;Sprawdzamy czy typ funkcji sie zgadza
      (funE (s-exp->symbol (second (s-exp->list s))) ;parsujemy nazwe funkcji
            (unique? (map (λ (x) (s-exp->symbol x))
                          (s-exp->list (third (s-exp->list s))))) ;Parsujemy parametry oraz sprawdzamy czy sa unikatowe
            (parse-else (fourth (rest (s-exp->list s))))) ;Parsujemy cialo funkcji
      (error 'parse-function "Zła struktrua funkcji")))

;Funkcja do parsowania wyrazenia e w naszym jezyku
(define (parse-else [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(s-exp-match? `{ANY SYMBOL ANY} s)
     (opE (parse-else (first (s-exp->list s)))
          (parse-op (s-exp->symbol (second (s-exp->list s))))
          (parse-else (third (s-exp->list s))))]
    [(s-exp-match? `{ifz ANY then ANY else ANY} s)
     (ifE (parse-else (second (s-exp->list s)))
          (parse-else (fourth (s-exp->list s)))
          (parse-else (fourth (rest (rest (s-exp->list s))))))]
    [(s-exp-match? `SYMBOL s)
     (varE (s-exp->symbol s))]
    [(s-exp-match? `{let SYMBOL be ANY in ANY} s)
     (letE (s-exp->symbol (second (s-exp->list s)))
           (parse-else (fourth (s-exp->list s)))
           (parse-else (fourth (rest (rest (s-exp->list s))))))]
    [(s-exp-match? `{ANY ANY} s)
     (appE (s-exp->symbol (first (s-exp->list s)))
           (map parse-else (s-exp->list (second (s-exp->list s)))))]
    [else (error 'parse-else "Zla konstrukcja wyrazenia e")]))

;Parsowanie operatorow
(define (parse-op [op : Symbol]) : Op
  (cond
    [(eq? op '+) (add)]
    [(eq? op '-) (sub)]
    [(eq? op '*) (mul)]
    [(eq? op '<=) (leq)]
    [else (error 'parse "Nieznany operator")]))

;; environments

;Srodowisko znane z wykladow - sluzy do przechowywania zmiennych
(define-type varBinding
  (varBind [name : Symbol]
           [val : Value]))

(define-type-alias varEnv (Listof varBinding))

(define mt-env empty)
(define (extend-varEnv [env : varEnv] [x : Symbol] [v : Value]) : varEnv
  (cons (varBind x v) env))

(define (lookup-varEnv [n : Symbol] [env : varEnv]) : Value
  (type-case (Listof varBinding) env
    [empty (error 'lookup-var "Niezwiazana zmienna")]
    [(cons b rst-env) (cond
                        [(eq? n (varBind-name b))
                         (varBind-val b)]
                        [else (lookup-varEnv n rst-env)])]))

;Srodowisko na wzor poprzedniego, sluzy do przechowywania funkcji
(define-type funcBinding
  (funcBind [name : Symbol] ;nazwa funkcji
            [val : ((Listof Symbol) * Exp)])) ;lista parametrow oraz cialo funkcji

(define-type-alias funcEnv (Listof funcBinding))

(define (extend-funcEnv [env : funcEnv] [name : Symbol] [var : (Listof Symbol)] [body : Exp]) : funcEnv
  (cons (funcBind name (pair var body)) env))

(define (lookup-funcEnv [n : Symbol] [env : funcEnv]) : ((Listof Symbol) * Exp)
  (type-case (Listof funcBinding) env
    [empty (error 'lookup-func "Niezdefiniowana funkcja")]
    [(cons b rst-env) (cond
                        [(eq? n (funcBind-name b))
                        (funcBind-val b)]
                        [else (lookup-funcEnv n rst-env)])]))


;; eval --------------------------------------

;
(define (op-num-bool->proc [f : (Number Number -> Boolean)]) : (Value Value -> Value)
  (λ (v1 v2) (if (f v1 v2) 0 1)))

(define (op->proc [op : Op]) : (Value Value -> Value)
  (type-case Op op
    [(add) +]
    [(sub) -]
    [(mul) *]
    [(leq) (op-num-bool->proc <=)])) ;tworzymy procedure dla <=

;Funkcja eval przyjmuje wyrazenie oraz 2 srodowiska
;Odpowiednio: srodowisko zmiennych oraz srodowisko funkcji
(define (eval [e : Exp] [vE : varEnv] [fE : funcEnv]) : Value
  (type-case Exp e
    [(numE n) n]
    [(varE x)
     (lookup-varEnv x vE)]
    [(opE l o r)
     ((op->proc o) (eval l vE fE) (eval r vE fE))]
    [(ifE b l r)
     (if (equal? (eval b vE fE) 0) (eval l vE fE) (eval r vE fE))]
    [(letE x e1 e2)
     (eval e2 (extend-varEnv vE x (eval e1 vE fE)) fE)] ; rozszerzamy srodowisko zmiennych o e1 oraz wywolujemy eval na e2
    [(appE e1 e2)
     (let ([func (lookup-funcEnv e1 fE)]) ;wyciagamy funkcje ze srodowiska
       (eval (snd func) ;wywolujemy eval dla wnetrza funkcji
             (extend-varEnv-list (map (λ (x) (eval x vE fE)) e2) (fst func) vE) ; oraz rozszerzonego srodowiska zmiennych o parametry funkcji z zaaplikowanymi do nich wartosciami
             fE))]
    [(defineE d e) (eval e vE (extend-funcEnv-list d fE))]
    [(funE name x e) (error 'eval "Blad w eval")])) ;funE jest wykonywana podczas define, dlatego tutaj zwracamy blad

;Funkcja dodajaca do srodowiska kazda funkcje z listy funcLst
(define (extend-funcEnv-list [funcLst : (Listof Exp)] [fE : funcEnv]) : funcEnv
  (if (empty? funcLst)
      fE
      (let ([fir (first funcLst)]) ; przechowujemy funkcje w fir
      (extend-funcEnv-list (rest funcLst)
                           (extend-funcEnv fE (funE-name fir) (funE-x fir) (funE-e fir)))))) ;rozszerzamy srodowisko o pierwsza funkcje i wywolujemy extend-funcEnv-list na reszcie listy

;Funkcja sluzaca do wiazania listy parametrow z lista wartosci
(define (extend-varEnv-list [argLst : (Listof Value)] [varLst : (Listof Symbol)] [vE : varEnv]) : varEnv
  (cond [(and (empty? argLst) (empty? varLst)) vE] ; jesli obie sa puste to zwracamy srodowisko
        [(empty? argLst) (error 'extend-varEnv-list "Mniejsza ilosc argumentow niz zmiennych")] ; jesli, ktoras z list jest liczniejsza zwracamy blad
        [(empty? varLst) (error 'extend-varEnv-list "Mniejsza ilosc zmiennych niz argumentow")]
        [else (extend-varEnv-list (rest argLst) ; w przeciwnym przypadku wiazemy pierwszy parametr z pierwsza wartoscia
                                  (rest varLst) ; i wywolujemy funkcje na reszcie argumentow
                                  (extend-varEnv vE (first varLst) (first argLst)))]))

(define (run [s : S-Exp]) : Value
  (eval (parse s) mt-env mt-env))