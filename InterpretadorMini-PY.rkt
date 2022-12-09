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
    
    (programa (expresion) un-programa)

    ;;Expresion
    
    (expresion (numero)   numero-lit)
    
    (expresion (identificador)   id-exp)

    (expresion ("\""texto"\"")   texto-lit)

    (expresion ("false") false-exp)
    
    (expresion ("true") true-exp)

    (expresion (primitiva "(" (separated-list expresion ",") ")")  primapp-exp)

    (expresion ("if" expresion-bool "then""{" expresion "}""else""{" expresion "}" "end") condicional-exp)

    (expresion ("declarar" "(" (separated-list identificador "=" expresion ";") ")" "{" expresion "}" ) variableLocal-exp)

    (expresion ("procedimiento" "(" (separated-list identificador ",") ")" "haga" expresion "finProc" ) procedimiento-ex)

    (expresion ("evaluar"  expresion "("(separated-list expresion ",") ")" "finEval") app-exp)

    (expresion ("letrec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "in" expresion) letrec-exp)

    (expresion ("var" "{" (arbno identificador "=" expresion ";") "}" "in" expresion) var-exp)
    
    (expresion ("const" "{" (arbno identificador "=" expresion ";") "}" "in" expresion) const-exp)

    (expresion ("[" (separated-list expresion ",") "]") lista)

    (expresion ("tupla" "[" (separated-list expresion ",") "]") tupla)

    (expresion ("{" "{"identificador "=" expresion "}"";" (arbno "{"identificador "=" expresion "}"";") "}") registro)

    (expresion ("begin" "{" expresion ";" (arbno expresion ";") "}" "end") secuencia-exp)

    (expression ("set" identificador "=" expresion) set-exp)

    (expresion ("while" expresion-bool "do" "{" expresion "}" "done" ) while-exp)

    (expresion ("for" identificador "=" expresion "to" expresion "do" "{" expresion "}""done") for-exp)

    ;;Expresion bool

    (expresion-bool (pred-prim "("expresion "," expresion")") predicado-no-condicional)
    (expresion-bool (oper-bin-bool "(" expresion-bool "," expresion-bool ")") predicado-bin-condicional)
    (expresion-bool (oper-un-bool "(" expresion-bool ")") predicado-un-condicional ) 


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

    (primitiva ("+")      primitiva-suma)
    (primitiva ("~")      primitiva-resta)
    (primitiva ("/")      primitiva-div)
    (primitiva ("*")      primitiva-multi)
    (primitiva ("%")      primitiva-mod)
    (primitiva ("concat") primitiva-concat)
    (primitiva ("null") primitiva-null)
    (primitiva ("null?") primitiva-null?)
    (primitiva ("head") primitiva-head)
    (primitiva ("tail") primitiva-tail)
    (primitiva ("append") primitiva-append)

    (primitiva ("lista?") primitiva-lista?)
    (primitiva ("tupla?") primitiva-tupla?)
    (primitiva ("registro?") primitiva-registro?)

    ;(primitiva ("cons") primitiva-crear-lista)
    ;(primitiva ("tupla") primitiva-crear-tupla)
    ;(primitiva ("") primitiva-crear-registro)

    (primitiva ("longitud")  primitiva-longitud)
    
    (primitiva ("add1") primitiva-add1)
    
    (primitiva ("sub1") primitiva-sub1)

    ;;Primitiva Lista
    ;unaria vacio, vacio?, crear-lista, lista?, cabeza, cola
    ;binaria append 

    ;;Primitiva Tupla

    ;;Primitiva Registro
    
   
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

