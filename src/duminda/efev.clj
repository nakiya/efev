(ns duminda.efev
  (:require [instaparse.core :as insta]
            [com.stuartsierra.dependency :as dep]
            [clojure.walk :refer [prewalk postwalk]]
            [rhizome.viz :as viz]
            [clojure.set :as cs]))

;;Incomplete. But this suffices as a PoC
;;TODO: Doesn't handle strings yet. Do we need formulas with strings in them?
;;TODO: Use of semicolon as a statement terminator will cause issue. One solution might be to send seperate formulas, parse individually and then combine into a single tree.
(def formula-grammar
                                        ;;;from here and modified: https://notebooks.gesis.org/binder/jupyter/user/bhugueney-binder-test-y69wi9ts/notebooks/Clojure.ipynb : lang0-parser
                                        ;;;number regex here: https://stackoverflow.com/questions/2811031/decimal-or-numeric-values-in-regular-expression-validation/39399503#39399503
  "prog = assign+;
    assign = spaces cellref spaces <'='> spaces expr spaces <';'>;
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

(insta/defparser basic-parser formula-grammar)

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
           (map #(vector :cellref (first %) (second %)))))))

(comment
  (expand-range [:cellref "A" "1"] [:cellref "B" "1"])
  (expand-range [:cellref "B" "2"] [:cellref "A" "11"])
  (expand-range [:cellref "B" "1"] [:cellref "A" "3"])
  ,)

(defn expand-range-inline [parse-tree]
  (->> parse-tree
       (prewalk (fn [n] (if (and (vector? n) (= (first n) :cellrange))
                          (expand-range (nth n 1) (nth n 2))
                          n)))
       (prewalk (fn [n] (if (and (vector? n) (= (first n) :arglist) (seq? (second n)))
                          (into [:arglist] (second n))
                          n)))))

(def formula-parser (comp expand-range-inline basic-parser))

(comment
  (formula-parser "A.1 = 2 + 3 / 5;" :trace true)
  (formula-parser "A.1 = A.2;")
  (formula-parser "A.1 = 5.3 + 3;")
  (formula-parser "A.1 = SUM(B.1:B.3);")
  (formula-parser "A.1 = SUM(B.1:D.4);")
  (insta/disable-tracing!)
  ,)

(comment
  (insta/visualize (formula-parser "A.1 = SQRT(SUM(78 + A.12) + B.22); B.1 = A.1 + 3;"))
  (insta/visualize (formula-parser "A.1 = 2; A.2 = A.1 + 5; B.1 = SUM(A.1, A.2); B.2 = B.1;"))
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


(comment
  (formula-parser "A.1 = SQRT(SUM(A.1:B.3) + 24);")

  (def g1 (-> (dep/graph)
              (dep/depend :b :a)
              (dep/depend :c :b)
              (dep/depend :c :a)
              (dep/depend :d :c)))

  (dep/topo-sort g1)

  (formula-parser "A.1 = 2 + 3; B.1 = A.1 + 2;")
  ;; => [:prog [:assign [:cellref "A" "1"] [:add [:number "2"] [:number "3"]]] [:assign [:cellref "B" "1"] [:add [:cellref "A" "1"] [:number "2"]]]]

  (tree-seq (fn [node]
              (println node)
              (if (vector? node)
                (not= (first node) :cellref)
                false)) seq
            [:assign [:cellref "B" "1"] [:add [:cellref "A" "1"] [:number "2"]]])

  (->> '( 2 (7 nil nil) (88 (5 nil nil) nil) )
       (tree-seq list? rest))

  ,)

(defmulti get-dependencies first)

(defmethod get-dependencies :assign [[_ assign-cell expr]]
  [assign-cell (filter #(and (vector? %) (= :cellref (first %)))
                       (tree-seq (fn [node]
                                   (if (vector? node)
                                     (not= (first node) :cellref)
                                     false)) seq expr))])

(defmethod get-dependencies :prog [prog]
  (map get-dependencies
       (filter #(= :assign (and (vector? %) (first %))) prog)))

(defn cell-deps [parse-tree]
  "Get cell dependencies"
  (->> (get-dependencies parse-tree)
       (mapcat (fn [[assign-cell dep-cells]]
                 (map (fn [d] (vector assign-cell d)) dep-cells)))
       (remove empty?)
       (reduce (fn [g [a b]] (dep/depend g a b)) (dep/graph))))

(defn get-eval-order [deps]
  "Get the topological ordering of deps"
  (dep/topo-sort deps))

(defn visualize-dependencies [deps]
  "Show the dependency graph as a graphviz graph."
  (let [dep-g     (->> deps
                       :dependencies
                       (map (fn [[k v]] (vector k (into [] v))))
                       (into {})
                       (prewalk #(if (and (vector? %) (= (first %) :cellref)) (str (nth % 1) "." (nth % 2)) %)))
        dist      (->> (seq dep-g)
                       flatten
                       distinct
                       (into #{}))
        dep-g-ks  (into #{} (keys dep-g))
        missing   (cs/difference dist dep-g-ks)
        missing-m (reduce (fn [m k] (assoc m k [])) {} missing)
        dep-g     (merge dep-g missing-m)]
    (viz/view-graph (keys dep-g) dep-g :node->descriptor (fn [n] {:label n}))
    dep-g))

(comment
  (cell-deps
    (formula-parser "A.1 = 2; A.2 = A.1 + 5; B.1 = SUM(A.1, A.2); B.2 = B.1;"))

  ;; => #com.stuartsierra.dependency.MapDependencyGraph{:dependencies {[:cellref "A" "2"] #{[:cellref "A" "1"]}, [:cellref "B" "1"] #{[:cellref "A" "1"] [:cellref "A" "2"]}, [:cellref "B" "2"] #{[:cellref "B" "1"]}}, :dependents {[:cellref "A" "1"] #{[:cellref "A" "2"] [:cellref "B" "1"]}, [:cellref "A" "2"] #{[:cellref "B" "1"]}, [:cellref "B" "1"] #{[:cellref "B" "2"]}}}

  ;; Note that A.3 does not appear in the graph. Basically we don't care about the evaluation order of A.3.
  (visualize-dependencies (cell-deps
                            (formula-parser "A.1 = 2; A.2 = A.1 + 5; B.1 = SUM(A.1, A.2); B.2 = B.1;")))
  ;; => {"A.1" ["A.2" "B.3" "B.1"], "A.2" ["B.1"], "B.1" ["B.2"], "B.2" ["B.3"], "B.3" []}

  (visualize-dependencies (cell-deps
                            (formula-parser "C.1 = SUM(A.1:A.3); A.1=A.2; A.3=A.4+A.2;")))

  (get-eval-order (cell-deps (formula-parser "A.1 = 2; A.2 = A.1 + 5; B.1 = SUM(A.1, A.2); B.2 = B.1;")))
;; => ([:cellref "A" "1"] [:cellref "A" "2"] [:cellref "B" "1"] [:cellref "B" "2"] [:cellref "B" "3"])

  ;Self reference
  (cell-deps (formula-parser "A.1 = A.1;"))

  ;Circular dependency
  (cell-deps (formula-parser "A.1 = B.1; A.2 = A.1; B.1 = A.2;"))

  (let [parse-tree (formula-parser "A.1 = 2; A.2 = A.1 + 5; B.1 = SUM(A.1, A.2); B.2 = B.1;")
        eval-order (get-eval-order (cell-deps parse-tree))]
    [parse-tree eval-order])
  ;; => [[:prog [:assign [:cellref "A" "1"] [:number "2"]] [:assign [:cellref "A" "2"] [:add [:cellref "A" "1"] [:number "5"]]] [:assign [:cellref "B" "1"] [:funcall "SUM" [:arglist [:cellref "A" "1"] [:cellref "A" "2"]]]] [:assign [:cellref "B" "2"] [:cellref "B" "1"]]] ([:cellref "A" "1"] [:cellref "A" "2"] [:cellref "B" "1"] [:cellref "B" "2"])]

  ,)


(def custom-functions
  {"SUM"    +
   "SQRT"   #(Math/sqrt %)
   "CONCAT" str})

(defn lookup-cell-fn [model]
  (fn lookup-cell
    ([col row]
     (println "lookup-cell: " (str col "." row) ", returned: " (@model (str col "." row)))
     (@model (str col "." row)))
    ([[_ col row]]
     (println "lookup-cell 2nd called")
     (lookup-cell col row))))

(defn call-function
  "Form of the second argument: [:arglist 80 54]. So we destructure here and drop the first item"
  [funname [_ & args :as orig-args]]
  (println "call-function:" funname "with" orig-args "args = " args)
  (apply (custom-functions funname) (flatten args)))

(defn get-transform-map [model]
  {:number         #(Integer/parseInt %)
   :add            (fn [& args]
                     (println "+ called with : " args)
                     (apply + args))
   :sub            -
   :mult           *
   :div            /
   :colid          identity
   :rowid          identity
   :cellref        (lookup-cell-fn model)
   :funcall        call-function
   :prog           identity
   :cellref-assign #(str %1 "." %2)
   :assign         (fn [k v]
                     (println "Called assign with : " k ": " v)
                     (swap! model assoc k v))
   })

(defn eval-tree [parse-tree initial-model]
  "Evaluates a parse tree with supplied model."
  (let [model              (atom initial-model)
        eval-order         (get-eval-order (cell-deps parse-tree))
        eval-order-map     (apply hash-map (interleave eval-order (range (count eval-order))))
        statements         (filter #(and (vector? %) (= :assign (first %))) parse-tree)
        ordered-statements (sort-by #(eval-order-map (second %)) statements)
        ;;Convert assignment cell to diff type
        ordered-statements (map (fn [[stmt [kw col row :as cr] expr]]
                                  [stmt [:cellref-assign col row] expr]) ordered-statements)
        transform-map      (get-transform-map model)]
    (doall (map (fn [assign]
                  (postwalk
                    (fn [node]
                      (if (vector? node)
                        (let [f (transform-map (first node))
                              _ (println "Evaluating " node "with " f)]
                          (if f (apply f (rest node)) node))
                        node)
                      ) assign))
                ordered-statements))
    @model))

(comment
  ;; Model is a map here.
  (def test-model {"A.1" 1 "A.2" 2 "A.3" 3 "A.4" 4
                   "B.1" 5 "B.2" 6 "B.3" 7 "B.4" 8})

  (eval-tree (formula-parser "A.2= 5;C.1 = SUM(A.1:A.3); A.1=A.2; A.3=A.4+A.2;") test-model)
;; => {"B.4" 8, "A.2" 5, "B.1" 5, "A.1" 5, "B.2" 6, "A.4" 4, "A.3" 9, "B.3" 7, "C.1" 19}


  ,)
