#lang eopl
;;INTERPRETADOR
;;Natalia Lopez Osorio - 2025618
;;Carolain JImenez Bedoya - 2071368
;;Juan Steban Diaz - 2024147
;;Hernando Lopez Rincon - 2022318
;;Gabriel Franco Betancourt - 2024200

;******************************************************************************************

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <programa>     ::= <expresion>
;;                     <un-programa (exp)>

;;  <expresion>    ::= <numero>
;;                     <numero-lit  (num)>

;;                 := "\""<texto> "\""
;;                     <texto-lit (txt)>

;;                 ::= <identificador>
;;                     <id-exp (id)>

;;                 ::= "false"
;;                     <false-exp>

;;                 ::= "true"
;;                     <true-exp>


;;                 ::= <primitiva>(<expression>*(,))
;;                     <primapp-exp (expPrim)>

;;                 ::= if <expresion-bool> then {<expresion>} else {<expression>} end
;;                      <condicional-exp (test-exp true-exp false-exp)>

;;                 := procedimiento (<identificador>*',') haga <expresion> finProc
;;                    <procedimiento-ex (ids cuerpo)>

;;                 := evaluar <expresion>(<expresion> ",")* finEval
;;                    <app-exp(exp exps)>

;;                 ::= letrec  {identifier ({identifier}*(,)) = <expression>}* in <expression>
;;                     <letrec-exp proc-names idss bodies bodyletrec>

;;                 ::= var {<identificador> = <expresion> }*(;) in <expresion>
;;                     <var-exp idsVar expsVar cuerpoVar>

;;                 ::= const {<identificador> = <expresion> }*(;) in <expresion>
;;                     <const-exp idsConst expsConst cuerpoConst>

;;                 ::= [{<expresiones>} *(;)]
;;                     <lista expsLista>

;;                 ::= tupla[{<expresion>}*(;)]
;;                     <tupla expsTupla>

;;                 ::= {{<identificador> = <expresion>} +(;) }
;;                     <registro idsReg expReg>

;;                 ::= begin {<expresion>}+(;) end
;;                     <secuencia expSec>

;;                 ::= while <expresion-bool> do { <expresion>}done
;;                     <while-exp expBoolWhile expWhile>

;;                 ::= for <identificador> = <expresion>  to <expresion> do {<expresion>} done
;;                     <for-exp idFor inicioFor finFor cuerpoFor>

;;                 ::= set <identificador> = <expresion>
;;                     <set-exp idSet expSet> 

;;  <expresion-bool> ::= <pred-prim> ( <expresion> , <expresion> )
;;                       <predicado-no-condicional expre1 expre2>

;;                   ::= <oper-bin-bool> ( <expresion-bool> , <expresion-bool> )
;;                      <predicado-bin-condicional expre1 expre2>

;;                   ::= <oper-un-bool> (<expresion-bool> )
;;                      <predicado-un-condicional expre>

;;  <primitiva>   ::= + (primitiva-suma)
;;                ::= ~ (primitiva-resta)
;;                ::= / (primitiva-div)
;;                ::= * (primitiva-multi)
;;                ::= % (primitiva-mod)
;;                ::= concat(primitiva-concat)
;;                ::= longitud(primitiva-longitud)
;;                ::= add1(primitiva-add1)
;;                ::= sub1(primitiva-sub1)
;;                ::= null (primitiva-null)
;;                ::= null? (primitiva-null?)
;;                ::= head (primitiva-head)
;;                ::= tail (primitiva-tail)
;;                ::= append (primitiva-append)
;;                ::= lista? (primitiva-lista?)
;;                ::= tupla? (primitiva-tupla?)
;;                ::= registro? (primitiva-registro?)

;; <pred-prim>    ::= < (pred-prim-menor)
;;                ::= > (pred-prim-mayor)
;;                ::= <= (pred-prim-menor-igual)
;;                ::= >= (pred-prim-mayor-igual)
;;                ::= == (pred-prim-igual)
;;                ::= != (pred-prim-dif)

;;<oper-bin-bool> ::= and (and-oper-bool)
;;                ::= or (or-oper-bool)

;;<oper-un-bool> ::= not (not-oper-bool) 


;******************************************************************************************

;******************************************************************************************


;******************************************************************************************

;Especificación Léxica

(define scanner-spec-simple-interpreter
'(
  (white-sp    (whitespace) skip)
  (comentario     ("#" (arbno (not #\newline))) skip)
  (identificador  ("@" letter (arbno (or letter digit))) symbol)
  (texto        (letter (arbno (or letter digit ":" "?" "=" "'" "#" "$" "&" "." "," ";" "*" "!" "¡" "¿" "-" "_"))) string)
  (numero       (digit (arbno digit)) number)
  (numero       ("-" digit (arbno digit)) number)
  (numero       (digit (arbno digit) "." digit (arbno digit)) number)
  (numero       ("-" digit (arbno digit) "." digit (arbno digit)) number)
 )
)

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '(
    ;;Programa
    
    (programa ((arbno class-decl) expresion) un-programa)

    ;;class-decl
    
    (class-decl ("class" identificador "extends" identificador (arbno "field" identificador ) (arbno method-decl)) a-class-decl)

    (method-decl ("method" identificador "("  (separated-list identificador  ",") ")" expresion )  a-method-decl)

    ;;Expresiones para Objetos

    (expresion ("new" identificador "(" (separated-list expresion ",") ")") new-object-exp)

    (expresion ("send" expresion identificador "("  (separated-list expresion ",") ")") method-app-exp)

    (expresion ("super" identificador   "("  (separated-list expresion ",") ")")  super-call-exp)
    

    ;;Expresion
    
    (expresion (numero)   numero-lit)
    
    (expresion (identificador)   id-exp)

    (expresion ("\""texto"\"")   texto-lit)

    (expresion ("false") false-exp)
    
    (expresion ("true") true-exp)

    (expresion (primitiva "(" (separated-list expresion ",") ")")  primapp-exp)

    (expresion ("if" expresion-bool "then""{" expresion "}" "else" "{" expresion "}" "end") condicional-exp)

    (expresion ("procedimiento" "(" (separated-list identificador ",") ")" "haga" expresion "finProc" ) procedimiento-ex)
    
    (expresion ("evaluar"  expresion "("(separated-list expresion ",") ")" "finEval") app-exp)

    (expresion ("letrec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion )  "in" expresion) letrec-exp)

    (expresion ("var" "{" (arbno identificador "=" expresion ";") "}" "in" expresion) var-exp)
    
    (expresion ("const" "{" (arbno identificador "=" expresion ";") "}" "in" expresion) const-exp)

    (expresion ("[" (separated-list expresion ",") "]") lista)

    (expresion ("tupla" "[" (separated-list expresion ",") "]") tupla)

    (expresion ("{" "{"identificador "=" expresion "}"";" (arbno "{"identificador "=" expresion "}"";") "}") registro)

    (expresion ("begin" "{" expresion ";" (arbno expresion ";") "}" "end") secuencia-exp)

    (expresion ("set" identificador "=" expresion) set-exp)

    (expresion ("while" expresion-bool "do" "{" expresion "}" "done" ) while-exp)

    (expresion ("for" identificador "=" expresion "to" expresion "do" "{" expresion "}""done") for-exp)

    ;;Expresion bool

    (expresion-bool (pred-prim "("expresion "," expresion")") predicado-no-condicional)
    (expresion-bool (oper-bin-bool "(" expresion-bool "," expresion-bool ")") predicado-bin-condicional)
    (expresion-bool (oper-un-bool "(" expresion-bool ")") predicado-un-condicional )
    ;(expresion-bool ("false") false-exp-bool)
    ;(expresion-bool ("true") true-exp-bool)

    ;;pred-prim
    (pred-prim ("<") pred-prim-menor)
    (pred-prim (">") pred-prim-mayor)
    (pred-prim ("<=") pred-prim-menor-igual)
    (pred-prim (">=") pred-prim-mayor-igual)
    (pred-prim ("==") pred-prim-igual)
    (pred-prim ("!=") pred-prim-dif)

    ;;oper-bin-bool
    (oper-bin-bool ("and") and-oper-bool)
    (oper-bin-bool ("or") or-oper-bool)

    ;;oper-un-bool
    (oper-un-bool ("not") not-oper-bool) 

    
    ;;Primitiva

    ;;Primitiva numeros

    (primitiva ("+")      primitiva-suma)
    (primitiva ("~")      primitiva-resta)
    (primitiva ("/")      primitiva-div)
    (primitiva ("*")      primitiva-multi)
    (primitiva ("%")      primitiva-mod)
    (primitiva ("add1")   primitiva-add1)
    (primitiva ("sub1")   primitiva-sub1)

    ;;Primitiva cadenas
    
    (primitiva ("concat") primitiva-concat)
    (primitiva ("longitud")  primitiva-longitud)

    ;;Primitiva Listas y tuplas
    
    (primitiva ("null") primitiva-null)
    (primitiva ("null?") primitiva-null?)
    (primitiva ("head") primitiva-head)
    (primitiva ("tail") primitiva-tail)

    ;;primitiva lista
    (primitiva ("lista?") primitiva-lista?)
    (primitiva ("cons") primitiva-crear-lista)
    (primitiva ("append") primitiva-append)
    (primitiva ("ref-list") primitiva-ref-list)
    (primitiva ("set-list") primitiva-set-list)

    ;;primiiva tupla
    (primitiva ("tupla?") primitiva-tupla?)
    (primitiva ("crear-tupla") primitiva-crear-tupla)
    (primitiva ("ref-tupla") primitiva-ref-tupla)

    ;;primitiva registro
    (primitiva ("registro?") primitiva-registro?)
    ;(primitiva ("registro") primitiva-crear-registro)    
   
  )
)

;*******************************************************************************************
;Tipos de datos para la sintaxis abstracta de la gramática construidos automáticamente:

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)
  )
)

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter)
)

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter)
)

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
                         
    (lambda (pgm) (eval-programa  pgm))
    
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)
   )
 )

;*******************************************************************************************
;El Interprete

;eval-programa: <programa> -> expresion
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (c-decls exp)
                 (elaborate-class-decls! c-decls)
                 (eval-expresion exp (init-env))
      )
    )
  )
)

; Ambiente inicial

(define init-env
  (lambda ()
    (extend-env
      '(@a @b @c @d @e)
      (list 1 2 3 "Hola" "FLP")
      (empty-env)
    )
  )
)

;**************************************************************************************
;Definición tipos de datos referencia y blanco

(define-datatype target target?
  (direct-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?)))

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))


;eval-expresion: <expresion> <enviroment> ->  
; evalua la expresión en el ambiente de entrada, para cada caso (numero-lit,var-exp,texto-lit, condicional-exp, variableLocal-exp
;procedimiento-ex, app-exp, letrec, primapp-bin-exp, primapp-un-exp) devuelve algo diferente dependiendo del caso de la expresión.

(define eval-expresion
  (lambda (exp env)
    (cases expresion exp
      
      (numero-lit (numero) numero)
      
      (id-exp (id) (apply-env env id))
      
      (texto-lit (txt) txt)

      (true-exp () #t)
      
      (false-exp () #f)

      (primapp-exp (prim exp)
                   (let ((args (eval-primapp-exp-rands exp env)))
                     (apply-primitiva prim args env)))

      (lista (exp) (let ((args (eval-rands exp env)))
                     (apply-lista (list->vector args) )))

      (tupla (exp) (let ((args (eval-rands exp env)))
                     (list (car args) (cadr args) )))

      (registro (id exp list-id list-exp)
                (let (
                      (args (eval-rands list-exp env))
                      (arg (eval-rand exp env))
                      )
                     (apply-registro id arg list-id args ))

      )
      
      (condicional-exp (exp-bool true-exp false-exp)
                       (if (eval-expresion-bool exp-bool env)
                           (eval-expresion true-exp env)
                           (eval-expresion false-exp env)
                       ))

      (letrec-exp (proc-names idss bodies letrec-body)
                  (eval-expresion letrec-body
                                   (extend-env-recursively proc-names idss bodies env))) 

      (app-exp (exp exps)
               (let ((proc (eval-expresion exp env))
                     (args (eval-rands exps env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expresion "Attempt to apply non-procedure ~s" proc)
                  )
               )
       )

      (procedimiento-ex (ids cuerpo) (cerradura ids cuerpo env))

      (var-exp (ids exps cuerpo)
               (let ((args (eval-let-exp-rands exps env)))
                    (eval-expresion cuerpo (extend-env ids args env))
               )
       )

      (set-exp (id rhs-exp)
               (begin
                 (primitive-setref!
                  (apply-env-ref env id)
                  (eval-expresion rhs-exp env))
                 1))
      
      
      (secuencia-exp (exp exps) 
                 (let loop ((acc (eval-expresion exp env))
                             (exps exps))
                    (if (null? exps) 
                        acc
                        (loop (eval-expresion (car exps) 
                                               env)
                              (cdr exps)))))

      (while-exp (exp-bool exp)
                 (let
                     loop ((bool (eval-expresion-bool exp-bool env) ))

                    (if (not bool)
                        1
                        (eval-expresion exp env)
                       )

                 
                 ))

      ;;para objetos

      (new-object-exp (class-name rands)
        (let ((args (eval-rands rands env))
              (obj (new-object class-name)))
          (find-method-and-apply
            '@initialize class-name obj args)
          obj))

      
      (method-app-exp (obj-exp method-name rands)
        (let ((args (eval-rands rands env))
              (obj (eval-expresion obj-exp env)))
          (find-method-and-apply
            method-name (object->class-name obj) obj args)))

      
      (super-call-exp (method-name rands)
        (let ((args (eval-rands rands env))
              (obj (apply-env env 'self)))
          (find-method-and-apply
            method-name (apply-env env '%super) obj args)))

      
      (else #t)

      
                    
     )
   )
)

(define eval-expresion-bool
  (lambda (exp-bool env)
    (cases expresion-bool exp-bool
      
      (predicado-no-condicional (pred-prim exp1 exp2)
                               (apply-pred-prim pred-prim (eval-expresion exp1 env) (eval-expresion exp2 env) ))
      
      (predicado-bin-condicional (pred-bin-prim exp1 exp2)
                                (apply-bin-prim pred-bin-prim (eval-rand-bool exp1 env) (eval-rand-bool exp2 env) ) )
      
      (predicado-un-condicional (pred-un-prim exp)
                                (apply-un-prim pred-un-prim (eval-rand-bool exp env) ))

      )
    )
  )



; funciones auxiliares para aplicar eval-expression-bool a cada elemento de una 
; lista de operandos (expresiones booleanas)
(define eval-rand-bool
  (lambda (rand env)
    (eval-expresion-bool rand env)))




(define apply-lista
  (lambda (exp)
     exp
    )
  )

(define apply-registro
  (lambda (id arg list-id args)
    (list (cons id list-id) (cons arg args))
    
    )
  )


; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (cases expresion rand
      

      (lista (exp)
             (indirect-target
                (let ((ref (apply-env-ref env exp)))
                  (cases target (primitive-deref ref)
                    
                    (direct-target (expval) ref)
                    (indirect-target (ref1) ref1)
                    
                   )
                 )
               )

       ) 
      
      (else
       (direct-target (eval-expresion rand env))))))




(define eval-primapp-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-expresion x env)) rands)))

(define eval-let-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-let-exp-rand x env))
         rands)))

(define eval-let-exp-rand
  (lambda (rand env)
    (direct-target (eval-expresion rand env))))



(define apply-primitiva
  (lambda (prim exps env)
    
    (cases primitiva prim
      
      ;para numeros
      
      (primitiva-suma () (+ (car exps) (cadr exps)))
      (primitiva-resta () (- (car exps) (cadr exps)))
      (primitiva-div () (/ (car exps) (cadr exps)))
      (primitiva-multi () (* (car exps) (cadr exps)))
      (primitiva-mod () (modulo (car exps) (cadr exps)))
      (primitiva-add1 () (+ (car exps) 1))
      (primitiva-sub1 () (- (car exps) 1))
      
      ;para cadenas
      (primitiva-concat () (string-append (car exps) (cadr exps) ))
      (primitiva-longitud () (string-length (car exps)))

      ;para listas y tuplas
      (primitiva-null () '())
      (primitiva-null? () (if (null? (car exps)) #t #f))
      (primitiva-head () (car (car exps)))
      (primitiva-tail () (cdr (car exps)))

      ;para listas
      (primitiva-lista? () (if (vector? (car exps)) #t #f ))
      (primitiva-append () (append (car exps) (cadr exps) ))
      (primitiva-crear-lista () (cons (car exps) (cadr exps) ))
      (primitiva-ref-list () (vector-ref (car exps) (cadr exps)) )
      (primitiva-set-list ()
                          (begin
                            (vector-set! (car exps) (cadr exps) (caddr exps) )
                            1)
      )

      ;para tupla
      (primitiva-tupla? () (if (and (list? (car exps) ) (= (length (car exps)) 2) ) #t #f ))
      (primitiva-crear-tupla () (list (car exps) (cadr exps)) )
      (primitiva-ref-tupla () (list-ref (car exps) (cadr exps)))

      ;para registro      
      (primitiva-registro? () #t)
      ;(primitiva-crear-registro () #t)

      
     
    )
  )
)

(define apply-pred-prim
  (lambda (prim exp1 exp2)
    (cases pred-prim prim
      
       (pred-prim-menor () (< exp1 exp2) )
       (pred-prim-mayor () (> exp1 exp2) )
       (pred-prim-menor-igual () (<= exp1 exp2) )
       (pred-prim-mayor-igual () (>= exp1 exp2) )
       (pred-prim-igual () (= exp1 exp2) )
       (pred-prim-dif ()  (not(= exp1 exp2)))

      

    )
  )
 )

(define apply-bin-prim
  (lambda (prim exp1 exp2)
    (cases oper-bin-bool prim      
       (and-oper-bool () (and exp1 exp2) )
       (or-oper-bool () (or exp1 exp2) )
    )
  )
)

(define apply-un-prim
  (lambda (prim exp)
    (cases oper-un-bool prim      
       (not-oper-bool () (not exp) )
       
    )
  )
)


;*******************************************************************************************
;Procedimientos

;se crea el tipo de dato procval
(define-datatype procval procval?
  (cerradura
   (lista-ID (list-of symbol?))
   (exp expresion?)
   (amb environment?)))

;apply-procedure: <process> <arguments> -> <>
;proposito: Evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente

(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (cerradura (ids body env)
               (eval-expresion body (extend-env ids args env))))))



;;;;;;;;;;;;;;;; declarations ;;;;;;;;;;;;;;;;


(define class-decl->class-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        class-name))))

(define class-decl->super-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        super-name))))

(define class-decl->field-ids
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        field-ids))))

(define class-decl->method-decls
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        m-decls))))

(define method-decl->method-name
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) method-name))))

(define method-decl->ids
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) ids))))

(define method-decl->body
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) body))))

(define method-decls->method-names
  (lambda (mds)
    (map method-decl->method-name mds)))

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (direct-target (cerradura ids body env))))
            (iota len) idss bodies)
          env)))))
;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let
        loop ((next 0))
        (if (>= next end)
            '()
            (cons next (loop (+ 1 next)))
        )

     )
   )
 )

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    ;(begin
     ; (display env)
      ;(display "jajajaj ")
      (deref (apply-env-ref env sym))))
    ;)
(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))

;*******************************************************************************************
;Blancos y Referencias

(define expval?
  (lambda (x)
    (or (or (number? x) (procval? x) ) list? x)
  )
)

(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (indirect-target (v) #f)))))))

(define deref
  (lambda (ref)
    (cases target (primitive-deref ref)
      (direct-target (expval) expval)
      (indirect-target (ref1)
                       (cases target (primitive-deref ref1)
                         (direct-target (expval) expval)
                         (indirect-target (p)
                                          (eopl:error 'deref
                                                      "Illegal reference: ~s" ref1)))))))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref expval)
    (let
        ((ref (cases target (primitive-deref ref)
                (direct-target (expval1) ref)
                (indirect-target (ref1) ref1))))
      (primitive-setref! ref (direct-target expval))
    )
  )
)

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))


;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente


(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;^; new for ch 5
(define extend-env-refs
  (lambda (syms vec env)
    (extended-env-record syms vec env)))

;****************************************************************************************
;Para Objetos

;****************************************************************************************
;;;;;;;;;;;;;;;; class environments ;;;;;;;;;;;;;;;;

;;; we'll just use the list of class-decls.

(define the-class-env '())

(define elaborate-class-decls!
  (lambda (c-decls)
    (set! the-class-env c-decls)))

(define lookup-class
  (lambda (name)
    (let loop ((env the-class-env))
      (cond
        ((null? env)
         (eopl:error 'lookup-class
           "Unknown class ~s" name))
        ((eqv? (class-decl->class-name (car env)) name) (car env))
        (else (loop (cdr env)))))))

;;;;;;;;;;;;;;;; selectors of all sorts ;;;;;;;;;;;;;;;;

(define part->class-name
  (lambda (prt)
    (cases part prt
      (a-part (class-name fields)
        class-name))))

(define part->fields
  (lambda (prt)
    (cases part prt
      (a-part (class-name fields)
        fields))))

(define part->field-ids
  (lambda (part)
    (class-decl->field-ids (part->class-decl part))))

(define part->class-decl
  (lambda (part)
    (lookup-class (part->class-name part))))

(define part->method-decls
  (lambda (part)
    (class-decl->method-decls (part->class-decl part))))

(define part->super-name
  (lambda (part)
    (class-decl->super-name (part->class-decl part))))

(define class-name->method-decls
  (lambda (class-name)
    (class-decl->method-decls (lookup-class class-name))))

(define class-name->super-name
  (lambda (class-name)
    (class-decl->super-name (lookup-class class-name))))

(define object->class-name
  (lambda (parts)
    (part->class-name (car parts))))

;;




;; evaluar
(define aux
   (lambda (x)
     x))

(define-datatype part part? 
  (a-part
    (class-name symbol?)
    (fields vector?)))

(define new-object
  (lambda (class-name)
    (if (eqv? class-name '@object)
      '()
      (let ((c-decl (lookup-class class-name)))
        (cons
          (make-first-part c-decl)
          (new-object (class-decl->super-name c-decl)))))))

(define make-first-part
  (lambda (c-decl)
    (a-part
      (class-decl->class-name c-decl)
      (make-vector (length (class-decl->field-ids c-decl))))))

;;;;;;;;;;;;;;;; methods ;;;;;;;;;;;;;;;;

;;; methods are represented by their declarations.  They are closed
;;; over their fields at application time, by apply-method.

(define find-method-and-apply
  (lambda (m-name host-name self args)
    (if (eqv? host-name '@object)
      (eopl:error 'find-method-and-apply
        "No method for name ~s" m-name)
      
      (let ((m-decl (lookup-method-decl m-name
                      (class-name->method-decls host-name))))
        (if (method-decl? m-decl)
          (apply-method m-decl host-name self args)
          (find-method-and-apply m-name (class-name->super-name host-name) self args)
        )
      )
    )
  )
)

(define view-object-as
  (lambda (parts class-name)
    (if (eqv? (part->class-name (car parts)) class-name)
      parts
      (view-object-as (cdr parts) class-name))))

(define apply-method
  (lambda (m-decl host-name self args)
    (let ((ids (method-decl->ids m-decl))
          (body (method-decl->body m-decl))
          (super-name (class-name->super-name host-name)))
      (eval-expresion body
        (extend-env
          (cons '%super (cons 'self ids))
          (cons super-name (cons self args))
          (build-field-env 
            (view-object-as self host-name)))))))

(define build-field-env
  (lambda (parts)
    (if (null? parts)
      (empty-env)
      (extend-env-refs
        (part->field-ids (car parts))
        (part->fields    (car parts))
        (build-field-env (cdr parts))))))

;;;;;;;;;;;;;;;; method environments ;;;;;;;;;;;;;;;;

;; find a method in a list of method-decls, else return #f

(define lookup-method-decl 
  (lambda (m-name m-decls)
    (cond
      ((null? m-decls) #f)
      ((eqv? m-name (method-decl->method-name (car m-decls)))
       (car m-decls))
      (else (lookup-method-decl m-name (cdr m-decls))))))

;;ejemplos

;lista
;head([1,2,3])
;tail([1,2,3])
;cons(2, [])
;lista?([3,6,5,8])
;set-list([3,4,5], 1, 5)

;;tuplas
;tupla[1,2,3]
;head(tupla[2,3])
;tail(tupla[1,2])
;ref-tupla( tupla[1,3], 1)
;tupla?(tupla[3,4])
;tupla?([3,4,5])

;;registro
;;{{@a=4}; {@c=5};}

;;if
;if >(6,5) then {3} else {1} end
;if >=(6,6) then {3} else {1} end
;if !=(6,6) then {3} else {1} end
;if and(!=(6,3) , <(3,6)) then {3} else {1} end
;if and(!=(6,6) , <(3,6)) then {3} else {1} end
;if or(!=(6,6) , >(3,6)) then {3} else {1} end
;if or(not(!=(6,6)) , >(3,6)) then {3} else {1} end

;;letrec
;letrec
;       @sumar(@a,@b) = if !=(@a,0) then { add1(evaluar @sumar(sub1(@a),@b)finEval)} else{ @b } end
;       in
;       evaluar @sumar(4,5) finEval


;procedimiento
;procedimiento (@x,@y,@z) haga +(+(@x,@y),@z) finProc

;var
;var { @hola = 3 ; } in @hola
;var { @hola = 3 ;} in var {@hola = 10;} in @hola
;var {@l = [1,23,3];} in @l
;var { @lista = [2,3,4,5];} in begin { set-list(@lista, 1, 5); @lista; } end

;Begin y set
;var{ @x = 5;} in begin {set @x = 4; set @x = +(@x, 9); @x;} end

;var { @lista = [2,3,4,5];
;      @proc = procedimiento(@l) haga set-list(@l, 0, 10) finProc ;  }
;in
;   begin { evaluar @proc(@lista) finEval ; @lista ; } end

;var { @num = 4;
;       @proc = procedimiento(@n) haga set @n=5 finProc ;  }
;  in
;    begin { evaluar @proc(@num) finEval ; @num ; } end

;    var {@a=2;}
;    in
;    begin
;    {       while >(@a,0) do
;            {
;             begin {
;             set @a = sub1(@a);
;             5;    
;             } end    
    
;            } done ;
    
;    } end

;class @c1 extends @object
;        field @x
;        field @y

;        method @initialize()
;           begin{
;           set @x = 1;
;           set @y = 2;
;           }end
;        method @m1() @x

;        method @m2() @y
; var
;  { @o1 = new @c1() ; }
;  in
;  send @o1 @m1()
