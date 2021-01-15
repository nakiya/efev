(ns duminda.efev
  (:require [instaparse.core :as insta]
            [clojure.edn :as edn]
            [com.stuartsierra.dependency :as dep]
            [clojure.walk :refer [prewalk postwalk]]))

;;Incomplete. But this suffices as a PoC
(def formula-grammar
                                        ;;;from here and modified: https://notebooks.gesis.org/binder/jupyter/user/bhugueney-binder-test-y69wi9ts/notebooks/Clojure.ipynb : lang0-parser
                                        ;;;number regex here: https://stackoverflow.com/questions/2811031/decimal-or-numeric-values-in-regular-expression-validation/39399503#39399503
  "prog = stmt+;
    stmt = spaces cellref spaces <'='> spaces expr spaces <';'>;
    <expr> = add-sub;
    <add-sub> = mult-div | add | sub;
    add = add-sub spaces <'+'> spaces mult-div;
    sub = add-sub spaces <'-'> spaces mult-div;
    <mult-div> = factor | mult | div;
    mult = mult-div spaces <'*'> spaces factor;
    div = mult-div spaces <'/'> spaces factor;
    <factor> = funcall | number | cellref | <'('> spaces expr spaces <')'>;
    <spaces> = <#'\\s*'>;
    number = #'^[-~]?(0|[1-9]\\d*)?(\\.\\d+)?(?<=\\d)';
    cellref = colid <'.'> rowid;
    <rowid> = #'[0-9]+' | ident;
    <colid> = ident;
    <ident> = #'[a-zA-Z]\\w*';
    <funname> = ident;
    funcall = funname spaces <'('> arglist <')'> spaces;
    arglist = (spaces expr spaces <','> spaces)* expr | cellrange | Epsilon;
    cellrange = cellref <':'> cellref;")

(def formula-parser (insta/parser formula-grammar))

(comment
  (formula-parser "2 + 3 / 5" :trace true)
  (formula-parser "A.1 = 2 + 3;")
  (formula-parser "A.1 = 2 + 3; B.1 = A.1 + 2;")
  (insta/disable-tracing!)
  ,)

(comment
  (insta/visualize (formula-parser "A.1 = SQRT(SUM(78 + A.12) + B.22); B.1 = A.1 + 3;"))
  ;; (insta/visualize (formula-parser "SUM(1,2, 3 , SQRT(SUM(9,8))) + SUM()"))
  ;; (insta/visualize (formula-parser "2 + 3 * (24 -5)"))
  ;; (insta/visualize (formula-parser "2 + 3 / 5"))
  ;; (insta/visualize (formula-parser "5 * 2 - 6"))
  ;; (insta/visualize (formula-parser "SUM(A.1, A.2, B.1, B.2) * 12"))
  ;; (insta/visualize (formula-parser "1.34 + 3.45"))
  ;; (insta/visualize (formula-parser "-0.3 * .56 + -.7"))
  ;; (insta/visualize (formula-parser "SUM(A.1:B.5)"))
  ;; (insta/visualize (formula-parser "SUM(t2.Notional:t4.Notional)"))
  ;; (insta/visualize (formula-parser "45 + ~34"))
  )

;; Model is a map here.
(def model {"A.1"         1    "A.2"         2    "A.3"         3 "A.4" 4
            "B.1"         5    "B.2"         6    "B.3"         7 "B.4" 8
            "t1.Notional" 1000 "t2.Notional" 2000 "t3.Notional" 3000})

(def custom-functions
  {"SUM"    +
   "SQRT"   #(Math/sqrt %)
   "CONCAT" str})

(defn lookup-cell
  ([col row]
   (model (str col "." row)))
  ([[_ col row]]
   (println "lookup-cell 2nd called")
   (lookup-cell col row)))

(defn call-function
  "Form of the second argument: [:arglist 80 54]. So we destructure here and drop the first item"
  [funname [_ & args :as orig-args]]
  (println "call-function:" funname "with" orig-args "args = " args)
  (apply (custom-functions funname) (flatten args)))

(defn expand-range [[_ start-col start-row] [_ end-col end-row]]
                                        ;(println "unroll-range" start-col "." start-row " to " end-col "." end-row)
                                        ;There's one concern here: How to unroll something like t1.xxx:t5.xxx. Perhaps handle t[row] specially?
                                        ;Only do ranges of the form A.xx:C.yy for now.
  (if (= 2 (count (filter #(re-matches #"[A-Z]" %) [start-col end-col])))
    ;;Note that if this is true, row will be a number.
    (let [col->int                 (fn [col] (int (first col)))
          row->int                 (fn [row] (Integer/parseInt row))
          get-ends                 (fn [a b] (if (< a b) [a b] [b a]))
          start-col-i              (col->int start-col)
          end-col-i                (col->int end-col)
          [begin-col-i term-col-i] (get-ends start-col-i end-col-i)
          col-range                (map (comp str char) (range begin-col-i (inc term-col-i)))
          start-row-i              (row->int start-row)
          end-row-i                (row->int end-row)
          [begin-row-i term-row-i] (get-ends start-row-i end-row-i)
          row-range                (map str (range begin-row-i (inc term-row-i)))]
      (->> (for [col col-range row row-range]
             [col row])
           (map #(apply lookup-cell %))))))

(comment
  (expand-range [:cellref "A" "1"] [:cellref "B" "1"])
  (expand-range [:cellref "B" "2"] [:cellref "A" "11"])
  (expand-range [:cellref "B" "1"] [:cellref "A" "3"])
  ,)

(def transform-map {:number    edn/read-string
                    :add       +        :sub   - :mult * :div /
                    :colid     identity :rowid identity
                    :cellref   lookup-cell
                    :cellrange expand-range
                    :funcall   call-function
                    :prog      identity})

(comment
  (formula-parser "A.1 = SQRT(SUM(A.1:B.3) + 24);")
  ,)

;;We can use zippers or tree walkers here. But instaparse's built in fn suffices here.
(comment
  (insta/transform transform-map (formula-parser "A.1 = SQRT(SUM(A.1:B.3) + 24);"))
  (insta/transform transform-map (formula-parser "B.1 = SUM(A.1, A.2, B.1, B.2) * 12;"))
  ,)

(comment
  (def g1 (-> (dep/graph)
              (dep/depend :b :a)
              (dep/depend :c :b)
              (dep/depend :c :a)
              (dep/depend :d :c)))

  (dep/topo-sort g1)

  ,)

(comment
  (formula-parser "A.1 = 2 + 3; B.1 = A.1 + 2;")
;; => [:prog [:stmt [:cellref "A" "1"] [:add [:number "2"] [:number "3"]]] [:stmt [:cellref "B" "1"] [:add [:cellref "A" "1"] [:number "2"]]]]

  (tree-seq (fn [node]
              (println node)
              (if (vector? node)
                (not= (first node) :cellref)
                false)) seq
            [:stmt [:cellref "B" "1"] [:add [:cellref "A" "1"] [:number "2"]]])

  (->> '( 2 (7 nil nil) (88 (5 nil nil) nil) )
       (tree-seq list? rest))
  ,)

(defmulti get-dependencies first)

(defmethod get-dependencies :stmt [[_ assign-cell expr]]
  [assign-cell (filter #(and (vector? %) (= :cellref (first %)))
                       (tree-seq (fn [node]
                                   (if (vector? node)
                                     (not= (first node) :cellref)
                                     false)) seq expr))])

(defmethod get-dependencies :prog [prog]
  (map get-dependencies
       (filter #(= :stmt (and (vector? %) (first %))) prog)))

(defn get-eval-order [parse-tree]
  (->> (get-dependencies parse-tree)
       (mapcat (fn [[assign-cell dep-cells]]
                 (map (fn [d] (vector assign-cell d)) dep-cells)))
       (remove empty?)
       (reduce (fn [g [a b]] (dep/depend g a b)) (dep/graph))
       (dep/topo-sort)))

(comment
  (get-dependencies [:stmt [:cellref "B" "1"] [:add [:cellref "A" "1"] [:number "2"]]])
  (get-eval-order
    (formula-parser "A.1 = 2; A.2 = A.1 + 5; B.1 = A.1 + A.2; B.2 = B.1;"))
;; => #com.stuartsierra.dependency.MapDependencyGraph{:dependencies {[:cellref "A" "2"] #{[:cellref "A" "1"]}, [:cellref "B" "1"] #{[:cellref "A" "1"] [:cellref "A" "2"]}, [:cellref "B" "2"] #{[:cellref "B" "1"]}}, :dependents {[:cellref "A" "1"] #{[:cellref "A" "2"] [:cellref "B" "1"]}, [:cellref "A" "2"] #{[:cellref "B" "1"]}, [:cellref "B" "1"] #{[:cellref "B" "2"]}}}

  ,)
