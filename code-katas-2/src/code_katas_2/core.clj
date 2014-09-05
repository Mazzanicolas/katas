(ns code-katas-2.core)


(defn unpartial
  "Escribir una funcion que acepte una funcion parcial con cantidad de argumentos desconocida,
   retornar una funcion equivalente de n argumentos"
  [f]
  (fn  [& args] (reduce #(%1 %2) f args))
  )


(defn search
  "Dado un numero cualquiera de secuencias, cada una ya ordenada de menor a mayor, encontrar el numero
   mas chico que aparezca en todas las secuencias, las secuencias pueden ser infinitas."
  [& seqs]
  (if(not(=(first(sort(map first seqs)))(last(sort(map first seqs)))))(do
   (apply search (for [element seqs] (if (= (first element) (first(sort(map first seqs))))(rest element) element))
     )
    )
    (first(sort(map first seqs)))
   )
  )

(defn intercalar 
  "Escriba una funcion que tome un predicado de 2 argumentos, un valor y una coleccion, y
   retorne una nueva coleccion donde el valor es insertado intercalado cada dos argumentos
   que cumplan el predicado"
  [predicado valor secuencia]
   (flatten
   (for [ i (range) :while (not(nil? (first(drop i secuencia)))) ] 
       (if (not(nil?(first (drop (inc i)secuencia))))
        (if (predicado (nth secuencia i)(nth secuencia (inc i)))
         (concat [(nth secuencia i)] [valor])  (nth secuencia i)       )   (nth secuencia i)    )   )  ) 
  )


(defn tartamudeo
  "Escriba una funcion que retorne una secuencia lazy que comprima el tartamudeo de una secuencia de numeros.
   Comprimir el tartamudeo se refiere a que [1 1 1] se exprese como [3 1] y a su vez [3 1] se exprese como [1 3 1 1].

   La funcion debe aceptar una secuencia inicial de numeros, y devolver una secuencia infinita de compresiones, donde
   cada nuevo elemento es el elemento anterior comprimido."
  [secuencia]
  )
