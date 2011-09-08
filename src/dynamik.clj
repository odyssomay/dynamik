(ns dynamik
  (:use dynamik.core))

(defn dynamik-panel [& {:as options}]
  {:pre [(contains? options :create-content)
         (contains? options :types)]}
  (tile (merge {:default-type (first (:types options))
                :menu? true} 
               options)))
