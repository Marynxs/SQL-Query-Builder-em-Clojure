(ns construtor.consulta
  (:require [clojure.string :as str]))

(defn- nome-campo [x]
  (cond
    (keyword? x) (name x)
    (symbol? x)  (name x)
    :else        (str x)))

(defn- valor-sql [v]
  (cond
    (string? v)  (str "\"" (str/replace v "\"" "\\\"") "\"")
    (keyword? v) (str "'" (name v) "'")
    (symbol? v)  (str "'" (name v) "'")
    (boolean? v) (if v "true" "false")
    (nil? v)     "NULL"
    (coll? v)    (str "[" (str/join ", " (map valor-sql v)) "]")
    :else        (str v)))

(defn operador-de [op]
  (fn [c v]
    (str (nome-campo c) " " op " " (valor-sql v))))

(def igual?      (operador-de "="))
(def maior?      (operador-de ">"))
(def menor?      (operador-de "<"))
(def diferente?  (operador-de "<>"))
(def parecido?   (operador-de "LIKE"))

(defn- in? [campo valores]
  (str (nome-campo campo) " IN " (valor-sql valores)))

(defn- aplicar-comparador [{:keys [campo igual_a valor maior_que menor_que 
                                   diferente_de like em]}]
  (cond
    (or igual_a valor)    (igual? campo (or igual_a valor))
    maior_que             (maior? campo maior_que)
    menor_que             (menor? campo menor_que)
    diferente_de          (diferente? campo diferente_de)
    like                  (parecido? campo like)
    em                    (in? campo em)))

(defn- bloco->sql [x]
  (if (map? x)
    (aplicar-comparador x)
    x))

;; reduce pra concatenar com and
(defn e_s [predicados]
  (reduce #(if (empty? %1) 
             (bloco->sql %2) 
             (str %1 " and " (bloco->sql %2))) 
          "" 
          predicados))

;; reduce pra concatenar com OR
(defn ou_s [predicados]
  (str "OR (" 
       (reduce #(if (empty? %1) 
                  (bloco->sql %2) 
                  (str %1 " OR " (bloco->sql %2))) 
               "" 
               predicados)
       ")"))

(defn busca_tabela [t]
  {:tabela (nome-campo t) :campos :* :filtro nil})

;; map e filter
(defn campos [consulta lista-campos]
  (assoc consulta :campos (map nome-campo (filter some? lista-campos))))

(defn filtros [consulta where]
  (assoc consulta :filtro 
    (when where
      (if (map? where)
        (aplicar-comparador where)
        where))))

(defn gerar-sql [{:keys [tabela campos filtro]}]
  (let [cols (if (= campos :*) "*" (str/join ", " campos))
        base (str "SELECT " cols " FROM " tabela)]
    (if filtro (str base " WHERE " filtro) base)))

;; Existe um bug no mycompiler que nao funciona acentos
;; teste
(def consulta
  (-> (busca_tabela "usuario")
      (campos ["abc" "xyz"])
      (filtros
        (e_s
          [{:campo 'nome :igual_a "José"}
           {:campo 'idade :maior_que 20}
           {:campo 'id :em [10 20 30]}
           {:campo 'status :igual_a true}
           (ou_s [{:campo 'camiseta :valor 'verde}
                  {:campo 'camiseta :valor 'azul}])]))))

(println (gerar-sql consulta))

(def consulta2
  (-> (busca_tabela "usuario")
      (filtros
        (e_s
          [{:campo 'nome :igual_a "Jose"}
           {:campo 'idade :menor_que 20}
           {:campo 'id :em [10 20 30]}
           {:campo 'status :igual_a true}
           (ou_s [{:campo 'camiseta :valor 'verde}
                  {:campo 'camiseta :valor 'azul}])]))))

(println (gerar-sql consulta2))

;; (def teste2
;;   (-> (busca_tabela "posts")
;;       (filtros {:campo 'autor :igual_a "Maria"})))
;; (println (gerar-sql teste2))
