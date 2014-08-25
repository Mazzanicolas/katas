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
 

(defn range
  "Escribir una funcion que cree una lista de enteros en un rango dado.
   Restricciones: range"
  [start end]
  ((fn funcionAnonimaConNombre [x start end]
  (if (not(= start end))(funcionAnonimaConNombre (conj x start) (inc start) end) x)) [] start end) 
  )
; - ^ - ^ - ^ - ^ Everything Works - ^ - ^ - ^ - ^ - At least that seems - -
(defn compress-sequence
  "Escribir una funcion que elimine los duplicados consecutivos
   de una secuencia"
  [s]
  (defn clojure-potter [x] ;el metodo tiene hambre y se come la ultima letra, igual esperamos que le agarre daibetes y ahì 
  ;nos da lo que queremos
    (conj (for [i (range 0 (-(.length x)1)) 
                                    :let [a (nth x i)]
                                    :when (not(=(nth x (inc i)) a ))] 
                                 a ) (last x) )
    )
  
    (concat (drop 1 (clojure-potter s))(take 1 (clojure-potter s)));diabetes
       
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
  (conj (conj '() (drop length s)) (take length s))
  )

(defn inter-two
  "Escribir una funcion que reciba dos secuencias y retorne el primero de cada una,
   luego el segundo de cada una, luego el tercero, etc.
   Restricciones: interleave"
  [s1 s2]
  (defn moarGrande [p1 p2] (if (< (.length s1)(.length s2))s1 s2))
  (defn primeros [x1 x2] (concat (take 1 x1)(take 1 x2)))
  (reduce concat 
  (for [i (range 0 (.length (moarGrande s1 s2)))]
  (concat (take 1(primeros (drop i s1)(drop i  s2)))(drop 1(primeros (drop i s1)(drop i  s2)) )))
  )  
 )

(defn retrieve-caps
  "Escribir una funcion que reciba un string y devuelva un nuevo string conteniendo
   solamente las mayusculas."
  [text]
  (apply str (filter #(Character/isUpperCase %) (seq text)))
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
    (apply assoc {} (interleave k v))	
  )
