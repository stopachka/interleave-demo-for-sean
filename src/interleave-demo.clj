(ns interleave-demo)

;; Mechanism to delay forms. Just wraps the form in a lambda
(defmacro my-delay [& forms]
  `(memoize (fn [] ~@forms)))

(defn my-force [f]
  (f))

;; Leaves the cdr delayed, which let us build up lazy sequences

(defmacro cons-stream [a b]
  `[~a (my-delay ~b)])

(defn car-stream [s] (first s))
(defn cdr-stream [s]
  (my-force (second s)))

;; Ex: here's how we can create an infinite list of numbers 

(defn numbers-starting-from [n]
  (cons-stream n (numbers-starting-from (+ 1 n))))
(def integers (numbers-starting-from 1))

;; Now, I wanted to create an interleave macro, that delayed the second stream
;; I thought it would delay, because it would be in the second portion of cons-stream 

(defmacro rec-interleave [s1 s2]
  `(cons-stream (car-stream ~s1)
                (rec-interleave ~s2 (cdr-stream ~s1))))

;; Buut, writing it this way causes an infinite loop. My theory is: 
;; rec-interleave is constantly evaluated, even though it's in the "delayed" part of cons-stream
;; This is because when unwraps delays into (memoize (fn [] (form))), this doesn't do much to the interpreter 
;; Clojure just goes on and looks at what `form` is, which has `interleave`, and bam recursive loop

(comment 
  (rec-interleave integers integers))

;; If I instead move the recursion into a function, and just leave the macro to do the delay, all works fine

(defn interleave* [s1 delayed-s2]
  (cons-stream (car-stream s1) (interleave* (my-force delayed-s2) (my-delay (cdr-stream s1)))))

(defmacro interleave [s1 s2]
  `(interleave* ~s1 (my-delay ~s2)))


(comment
  (interleave integers integers))
