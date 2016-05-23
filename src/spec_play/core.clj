(ns spec-play.core
  (:require [clojure.spec :as s]
            [clojure.spec.gen]
            [clojure.test.check.generators :as gen]))

(s/def ::even? (s/and integer? even?))
(s/def ::odd? (s/and integer? odd?))
(s/def ::a integer?)
(s/def ::b integer?)
(s/def ::c integer?)

(def s (s/cat :forty-two #{42}
              :odds (s/+ ::odd?)
              :m (s/keys :req-un [::a ::b ::c])
              :oes (s/* (s/cat :o ::odd? :e ::even?))
              :ex (s/alt :odd ::odd? :even ::even?)))

(s/conform s [42 11 13 15 {:a 1 :b 2 :c 3} 1 2 3 42 43 44 11])
(s/conform ::odd? 3)

(s/conform ::a 3)

(s/describe ::a)

;;  e  ::= x | (if e e e) | (lambda (x :- t) e) | (e e*) | #f | n? | add1
;;  t  ::= [x : t -> t] | (not t) | (or t t) | (and t t) | #f | N | Any
;;  p  ::= (is e t) | (not p) | (or p p) | (and p p) | (= e e)

;;;;;;;;;;;;;;;;;;;;
;; Parse Propositions
;;;;;;;;;;;;;;;;;;;;

; (is e t)
(s/def ::parse-is-p (s/cat :is #{'is}
                           :exp ::parse-e
                           :type ::parse-t))

; (not p)
(s/def ::parse-not-p (s/cat :not #{'not}
                            :prop ::parse-p))

; (or p*)
(s/def ::parse-or-p (s/cat :or #{'or}
                           :props (s/* ::parse-p)))

; (and p*)
(s/def ::parse-and-p (s/cat :and #{'and}
                            :props (s/* ::parse-p)))

;;  p  ::= (is e t) | (not p) | (or p p) | (and p p) | (= e e)
(s/def ::parse-p
  (s/or :is  ::parse-is-p
        :not ::parse-not-p
        :or  ::parse-or-p
        :and ::parse-and-p))

;;;;;;;;;;;;;;;;;;;;
;; Parse Expressions
;;;;;;;;;;;;;;;;;;;;

(s/def ::sym symbol?)
(s/def ::false false?)

; var
(s/def ::parse-var-e   ::sym)

; false
(s/def ::parse-false-e ::false)

; (if e e e)
(s/def ::parse-if-e (s/cat :if #{'if}
                           :test ::parse-e
                           :then ::parse-e
                           :else ::parse-e))

; (lambda (x :- t) e)
(s/def ::parse-lambda-e (s/cat :lambda #{'lambda}
                               :binder (s/spec
                                         (s/cat :name ::sym
                                                :turnstile #{:-}
                                                :type ::parse-t))
                               :body ::parse-e))

; (e e*)
(s/def ::parse-app-e (s/cat :fn ::parse-e
                            :args (s/* ::parse-e)))

; add1
(s/def ::parse-add1-e #{'add1})

; n?
(s/def ::parse-n?-e #{'n?})

;;  e  ::= x | (if e e e) | (lambda (x :- t) e) | (e e*) | #f | n? | add1
(s/def ::parse-e
  (s/or ;; add1/n? are special variables
        :add1   ::parse-add1-e
        :n?     ::parse-n?-e
        :var    ::parse-var-e
        :if     ::parse-if-e
        :lambda ::parse-lambda-e
        :false  ::parse-false-e
        ;; app is a catch-all for lists
        :app    ::parse-app-e))

;;;;;;;;;;;;;;;;;;;;
;; Parse Types
;;;;;;;;;;;;;;;;;;;;

(s/def ::vec vector?)

; [x :- t -> t]
(s/def ::parse-fn-t 
  (s/and ::vec
         (s/cat :name ::sym
                :turnstile #{:-}
                :dom ::parse-t
                :arrow #{'->}
                :rng ::parse-t)))

; (not t)
(s/def ::parse-not-t 
  (s/cat :not #{'not}
         :type ::parse-t))

; N
(s/def ::parse-num-t #{'N})

; (or t*)
(s/def ::parse-or-t 
  (s/cat :or #{'or}
         :types (s/* ::parse-t)))

; (and t*)
(s/def ::parse-and-t 
  (s/cat :and #{'and}
         :types (s/* ::parse-t)))

; false
(s/def ::parse-false-t ::false)

; Any
(s/def ::parse-any-t #{'Any})

;;  t  ::= [x :- t -> t] | (not t) | (or t t) | (and t t) | #f | N | Any
(s/def ::parse-t
  (s/or :num   ::parse-num-t
        :false ::parse-false-t
        :any-t ::parse-any-t
        :fn    ::parse-fn-t
        :not   ::parse-not-t
        :or    ::parse-or-t
        :and   ::parse-and-t))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro assert-valid [& body]
  `(assert (s/valid? ~@body)
           (with-out-str
             (s/explain ~@body))))

(assert-valid ::parse-is-p '(is e N))

(assert-valid ::parse-not-p '(not (is e N)))

(assert-valid ::parse-false-e false)
(assert-valid ::parse-e false)
(assert-valid ::parse-var-e 'blah)
(assert-valid ::parse-e 'blah)
(assert-valid ::parse-if-e '(if blah blah blah))
(assert-valid ::parse-e '(if blah blah blah))
(assert-valid ::parse-lambda-e '(lambda (x :- N) blah))
(assert-valid ::parse-e '(lambda (x :- N) blah))
(assert-valid ::parse-add1-e 'add1)
(assert-valid ::parse-e 'add1)
(assert-valid ::parse-e '(add1 blah))

(assert-valid ::parse-n?-e 'n?)
(assert-valid ::parse-e '(n? blah))

(assert-valid ::parse-app-e '((lambda (x :- N) blah) blah))
(assert-valid ::parse-e '((lambda (x :- N) blah) blah))

(assert-valid ::parse-p '(not (is e N)))

(assert-valid ::parse-p '(or (not (is e N)) (is e N)))
(assert-valid ::parse-p '(and
                           (or (not (is e N)) (is e N))))

(assert-valid ::parse-any-t 'Any)
(assert-valid ::parse-t 'Any)
(assert-valid ::parse-false-t false)
(assert-valid ::parse-t false)
(assert-valid ::parse-num-t 'N)
(assert-valid ::parse-t 'N)
(assert-valid ::parse-fn-t '[x :- N -> N])
(assert-valid ::parse-t '[x :- N -> N])
(assert-valid ::parse-not-t '(not [x :- N -> N]))
(assert-valid ::parse-t '(not [x :- N -> N]))

(assert-valid ::parse-or-t '(or (not [x :- N -> N])))
(assert-valid ::parse-t '(or (not [x :- N -> N])))
(assert-valid ::parse-and-t '(and (or (not [x :- N -> N]))))
(assert-valid ::parse-t '(and (or (not [x :- N -> N]))))

(assert-valid ::parse-t '(and (or (not [x :- N -> N]))))

(defn parse-e [e] (s/conform ::parse-e e))
(defn parse-p [e] (s/conform ::parse-p e))
(defn parse-t [e] (s/conform ::parse-t e))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unparse Propositions
;;;;;;;;;;;;;;;;;;;;;;;;;

(def zero-nth #(nth % 0))

(defmacro defmethod-destruct
  "Transforms

   (defmethod n v [b] body...)

  into the equivalent of 

   (defmethod n v [[_ b]] body...)
  "
  [nme v [b] & body]
  `(defmethod ~nme ~v
     [g#]
     (let [~b (nth g# 1)]
       ~@body)))

(declare unparse-e unparse-t)

(defmulti unparse-p zero-nth)

(defmethod-destruct unparse-p :is
  [{:keys [exp type]}]
  (list 'is 
        (unparse-e exp)
        (unparse-t type)))

(defmethod-destruct unparse-p :not
  [{:keys [prop]}]
  (list 'not (unparse-p prop)))

(defmethod-destruct unparse-p :or
  [{:keys [props]}]
  (list* 'or (map unparse-p props)))

(defmethod-destruct unparse-p :and
  [{:keys [props]}]
  (list* 'and (map unparse-p props)))

(defmulti unparse-t zero-nth)

(defmethod-destruct unparse-t :num [name] name)

(defmulti unparse-e zero-nth)

(defmethod-destruct unparse-e :var [name] name)
(defmethod-destruct unparse-e :app 
  [{:keys [fn args]}]
  (list* (unparse-e fn)
         (map unparse-e args)))

(unparse-p (parse-p '(not (is e N))))
(unparse-p (parse-p '(or (not (is e N)))))
(unparse-p (parse-p '(and (not (is e N)))))

(unparse-e (parse-e '(blah blah)))

;; ???
(defonce e-gen (delay (s/gen ::parse-e)))
;(gen/sample @e-gen 1)
;(gen/sample (s/gen ::parse-is-p) 4)

;(s/registry)

(def my-symbol symbol)

(s/fdef my-symbol
  :args (s/alt :separate (s/cat :ns string? :n string?)
               :str string?
               :sym symbol?)
  :ret symbol?)

(s/instrument-all)

;(my-symbol 1)
