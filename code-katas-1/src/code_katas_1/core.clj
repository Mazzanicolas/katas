(ns code-katas-1.core)

(defn filter-odd
  "Escribir una funcion que retorne solamente los numeros impares de
   una secuencia"
  [s]
  (filter odd? s)) ; Works :D , filter better than "(for [x s] (if (odd? x) x)))" <- noobs crap, also even? are nil.


(defn nil-key
  "Escribir una funcion que dada una clave y un mapa, devuelva true, solamente si el mapa
   contiene una entrada con esa clave, y su valor es nil"
  [k m]
  (and (contains? m k) (= (get m k) nil)) ; if this doesn't return false test would fail
 )
 
; - ^ - ^ - ^ - ^ Everything Works - ^ - ^ - ^ - ^ - At least that seems - -

(defn range
  "Escribir una funcion que cree una lista de enteros en un rango dado.
   Restricciones: range"
  [start end]
  )

(defn compress-sequence
  "Escribir una funcion que elimine los duplicados consecutivos
   de una secuencia"
  [s]
  )

(defn max-value
  "Escribir una funcion que reciba un numero variable de parametros
   y retorne el que tenga el valor mayor
   Restricciones: max y max-key"
  [& args]
  (def x (sort args)); x become the args sorted list
  (last x) ; And we take the last one, is the higher number :D !.
  )

(defn split-two
  "Escribir una funcion que parta una secuencia en dos partes
   Restricciones: split-at"
  [length s]
  )

(defn inter-two
  "Escribir una funcion que reciba dos secuencias y retorne el primero de cada una,
   luego el segundo de cada una, luego el tercero, etc.
   Restricciones: interleave"
  [s1 s2]
  )

(defn retrieve-caps
  "Escribir una funcion que reciba un string y devuelva un nuevo string conteniendo
   solamente las mayusculas."
  [text]
  )

; Source: http://stackoverflow.com/questions/3249334/test-whether-a-list-contains-a-specific-value-in-clojure
(defn list-contains? [coll value]
  (let [s (seq coll)]
    (if s
      (if (= (first s) value) true (recur (rest s) value))
      false)))
; -^ StackOverflow method, i'll replace it later

(defn find-truth
  "Escribir una funcion que tome un numero variable de booleans, y devuelva true
   solamente si alguno de los parametros son true, pero no todos son true. En otro
   caso debera retornar false"
  [& xs]
 (and (list-contains? xs true) (list-contains? xs false))
  )

(defn zip-map
  "Escribir una funcion que reciba un vector de claves y un vector de valores, y
   construya un mapa a partir de ellos.
   Restricciones: zipmap"
  [k v]
  )
