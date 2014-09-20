 (ns mps)

;;Goal: to generate a valid mps file for a solver to run

(def cost {:type "N" :name "COST"           :coeff {:XONE 1 :YTWO 4  :ZTHREE 9}})
(def lim1 {:type "L" :name "LIM1"   :rhs 5  :coeff {:XONE 1 :YTWO 4           }})
(def lim2 {:type "G" :name "LIM2"   :rhs 10 :coeff {:XONE 1          :ZTHREE 1}})
(def myeqn {:type "E" :name "MYEQN" :rhs 7  :coeff {        :YTWO -1 :ZTHREE 1}})
(def eqns (list cost lim1 lim2 myeqn))

(def ub1 {:type "UP" :variable :XONE :value 4 })
(def lb2 {:type "LO" :variable :YTWO :value -1 })
(def ub2 {:type "UP" :variable :YTWO :value 1})
(def bnds (list ub1 lb2 ub2))

(defn space-it[text width]
  (clojure.string/join
    (take width
          (str text
               (clojure.string/join
                 (repeat width " "))))))

(defn header [name]
   (str (space-it "NAME" 14) name "\n"))

(defn print-equation [equation]
  (str " " (:type equation) "  " (:name equation) "\n"))

(defn rows [equations]
  (str "ROWS\n" (clojure.string/join "" (mapcat print-equation equations))))

(defn get-vars [equations]
  (sort (into #{} (mapcat #(keys  (:coeff %)) eqns))))

(defn get-coeff [equation var]
  (get-in equation [:coeff var]))

(defn get-columns [equations]
  (filter
    #(not (nil? (:coeff %)))
  (let [vars (get-vars equations)]
    (for [var vars eqn equations]
        {:var (name var) :eqn (:name eqn) :coeff (get-coeff eqn var)} ))))

(defn print-column [m]
  (str (space-it "" 4) (space-it (:var m) 10) (space-it (:eqn m) 10) " " (:coeff m) "\n"))

(defn columns [equations]
  (let [cols (get-columns equations)]
    (str "COLUMNS\n"
         (clojure.string/join (map print-column cols)))))


 (defn get-rhs [equations]
   (filter #(not (nil? (second %))) (map (fn [e] [(:name e) (:rhs e)]) equations)))

 (defn print-rhs [rhs]
   (str (space-it "" 4) (space-it "RHS1" 10) (space-it (first rhs) 10 ) " " (second rhs) "\n"))

(defn rhs [equations]
   (str "RHS\n"
        (clojure.string/join (map print-rhs (get-rhs equations)))))

(get-rhs eqns)
(printf (rhs eqns))

 (defn print-bound [bound]
   (str " " (:type bound) (space-it " BND1" 10) (space-it (name (:variable bound)) 10) (:value bound) "\n"))


(print-bound (first bnds))

 (defn bounds [bnds]
   (str "BOUNDS\n"
        (clojure.string/join (map print-bound bnds))))

 (printf (bounds bnds))

(defn footer []
   "ENDATA")

 (defn make-file [name equations]
   (str
     (header name)
     (columns equations)
     (rows equations)
     (rhs equations)
     (footer)))

 (defn test-it []
   (printf (make-file "TEST" eqns)))

(test-it)