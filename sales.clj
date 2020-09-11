(ns sales.sales 
   (:gen-class)
(require [clojure.string :as str]) 
)

(def cust (slurp "cust.txt"))
(def prod (slurp "prod.txt"))
(def sales (slurp "sales.txt"))
(def menu)

(defn dispCust []
   (def temp(map #(str/split % #"\|")(str/split-lines cust)))
   (def mapData (map #(hash-map (first %1) (vec (rest %1))) temp))
   (def mergeData (apply merge mapData))
   (into (sorted-map) mergeData))
(defn dispProd []
   (def temp(map #(str/split % #"\|")(str/split-lines prod)))
   (def mapData (map #(hash-map (first %1) (vec (rest %1))) temp))
   (def mergeData (apply merge mapData))
   (into (sorted-map) mergeData))
(defn dispSales []
   (def temp(map #(str/split % #"\|")(str/split-lines sales)))
   (def mapData (map #(hash-map (first %1) (vec (rest %1))) temp))
   (def mergeData (apply merge mapData))
   (into (sorted-map) mergeData))
 
(defn totalPurchase [presentName prodName qty price]
   (def multiplication (* (Float/parseFloat qty) (Float/parseFloat price)))   
   (def totalSum (+ multiplication totalSum))  
)

(def custData(dispCust))
(def prodData(dispProd))
(def salesData(dispSales))

(defn userSale []
  (println "Enter customer name?")
 (def totalSum 0)
 (let[custName (read-line)]
     (doseq [[k v] salesData]
    (def presentName(get (get custData (get v 0)) 0))
   
           (def prodName(get (get prodData (get v 1)) 0))
           (def qty(get v 2))
          (def price(get (get prodData (get v 1)) 1))
          (if (= (str/lower-case (list custName)) (str/lower-case (list presentName))) (totalPurchase presentName prodName qty price) ))
         
         (println custName ":" (format "$%.2f"(double totalSum)))        
))


(defn totalSale []
  (def totalproduct 0)
  (def matchItem)
  (println "Enter product name?")
  (let[itemName (read-line)]
     (doseq [[k v] prodData]
    
      (def matchItem (get v 0))
      ;(println "value of matchitem" matchItem)
       (if (= (get v 0) (str itemName))
         (do
         (def matchProd(get v 0))       
           (doseq [[k v] salesData]
             (if (=(get (get prodData (get v 1)) 0) matchProd)
               (do
               (def qtyProd(Integer/parseInt(get v 2)))
               (def totalproduct(+ totalproduct qtyProd))
                 ))))
     )) (println itemName":"totalproduct)))


(defn menu []
(println "")

   (println  "*** Sales Menu ***")
  (println  "------------------")
  (println  "1. Display Customer Table")
  (println  "2. Display Product Table")
  (println  "3. Display Sales Table")
  (println  "4. Total Sales for Customer")
  (println  "5. Total Count for Product")
  (println  "6. Exit")
(println "Enter an option?")
(println "")
 (let [myinput (read-line)]
   (case myinput 
     "1"  (doseq [[k v] custData]
            (println k":" "("(get v 0) "," (get v 1) "," (get v 2) ")"))
     "2" (doseq [[k v] prodData]
            (println k":" "("(get v 0) "," (get v 1) ")"))
     "3" (doseq [[k v] salesData]
            (println k":" "("(get (get custData (get v 0)) 0) "," (get (get prodData (get v 1)) 0) "," (get v 2) ")"))
     "4" (userSale)
     "5" (totalSale)
     "6" ((println "Good Bye")(System/exit 0))
  (do(println "Please enter valid choice.")
       ))) (menu))

(menu)