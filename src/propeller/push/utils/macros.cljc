(ns propeller.push.utils.macros
  (:require [propeller.push.core :as push]
            [propeller.push.utils.helpers :refer [get-vector-literal-type]]))

(defn def-instruction
  "Defines a Push instruction as a keyword-function pair, and adds it to the
   instruction table"
  [instruction function]
  (swap! push/instruction-table assoc instruction function))

(defn make-metadata
  "Given a generic function, e.g. _dup, and a stack type to instantiate it for,
   e.g. :char, returns the appropriate stack metadata for that function instance"
  [function stack]
  (->> (:stacks (meta function))
       (replace {:elem (get-vector-literal-type stack)})
       (cons stack)
       set
       (assoc-in (meta function) [:stacks])
       (#(dissoc % :name))))

(defn generate-instructions
  "Given a sequence of stacks, e.g. [:float :integer], and a sequence of suffix
   function strings, e.g. [_add, _mult, _eq], automates the generation of all
   possible combination instructions, which here would be :float_add, :float_mult,
   :float_eq, :integer_add, :integer_mult, and :integer_eq, also transferring
   and updating the generic function's stack-type metadata. For some vector
   instructions, the placeholder :elem will be replaced with the stack of the
   corresponding element type (e.g. for :vector_integer, with :integer)"
  [stacks functions]
  (doseq [stack stacks
          func functions]
    (let [instruction-name (keyword (str (name stack) (:name (meta func))))
          metadata (make-metadata func stack)
          new-func (with-meta (partial func stack) metadata)]
      (println [instruction-name new-func (meta new-func)])
      (def-instruction instruction-name new-func))))