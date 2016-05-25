;; same grammar, using destructuring
(ns spec-play.destruct
  (:require [clojure.spec :as s]
            [clojure.spec.gen]
            [clojure.test.check.generators :as gen]))

(defmacro parse-spec
  "Given a spec and value s, applies the spec to s
  which should return a vector [tag map].
  The body should be of a (case tag ...)
  where rhs is bound to map.
  
  (parse-spec ::a foo
    rhs
    ::blah (let [res (:bar rhs)] ...))
  "
  [spec s rhs & body]
  `(let [s# ~s
         prs# (s/conform ~spec s#)]
     (assert (not= ::s/invalid prs#) 
             (with-out-str
               (s/explain ~spec s#)))
     (assert (and (vector? prs#)
                  (== 2 (count prs#))))
     (let [~rhs (nth prs# 1)]
       (case (nth prs# 0)
         ~@body))))

(defmacro defmultispec
  "Defines a multimethod n suitable for multi-spec,
  where pairs each define a new defmethod.
  
  eg. (defmultispec f ::type
        ::type/a symbol?
        ::type/b (s/keys :req [::b/types]))"
  [n k & pairs]
  (assert (even? (count pairs)))
  `(do (defmulti ~n ~k :default ::s/invalid)
       (defmethod ~n ::s/invalid [_#] nil)
       ~@(map (fn [[k spec]]
                `(defmethod ~n ~k [_#]
                   ~spec))
              (partition 2 pairs))))

;;  e  ::= x | (if e e e) | (lambda (x :- t) e) | (e e*) | #f | n? | add1
;;  t  ::= [x : t -> t] | (not t) | (or t t) | (and t t) | #f | N | Any

;;  e  ::= x | (if e e e) | (lambda (x :- t) e) | (e e*) | #f | n? | add1
(defmacro parse-exp-template
  "Returns a spec for expressions, but we can vary the
  meaning of `e` and `t` in the body. This allows us to
  define specs that just match one level, or all levels."
  [e t]
  `(s/or ; false
         ::exp|parse-false false?
         ; x
         ::exp|parse-var symbol?
         ; (if e-e e-e e)
         ::exp|parse-if (s/cat ::exp|if #{'if}
                               ::if|test ~e
                               ::if|then ~e
                               ::if|else ~e)
         ; (lambda (x :- t) e)
         ::exp|parse-lambda
         (s/cat ::exp|lambda '#{~'lambda}
                ::lambda|binder (s/spec
                                  (s/cat ::lambda|binder|name symbol?
                                         ::turnstile #{:-}
                                         ::lambda|binder|type ~t))
                ::lambda|body ~e)

         ; (e e*)
         ::exp|parse-app
         (s/cat ::app|fn ~e
                ::app|args (s/* ~e))))

; e ::= x | (if any any any)
;; match one level, useful for destructuring
(s/def ::exp|parse1
  (parse-exp-template ::s/any ::s/any))

; e ::= x | (if e e e)
;; match all levels, useful for gen testing
(s/def ::exp|parse
  (parse-exp-template ::exp|parse ::type|parse))

(declare parse-type)

; exp->ast
(defn parse-exp [s]
  (parse-spec ::exp|parse1 s
    rhs
    ::exp|parse-if (let [{:keys [::if|test ::if|then ::if|else]} rhs]
                     (assert (every? some? [if|test if|then if|else])
                             (filterv some? [if|test if|then if|else]))
                     {::exp|type ::exp|if
                      ::if|test (parse-exp if|test)
                      ::if|then (parse-exp if|then)
                      ::if|else (parse-exp if|else)})
    ::exp|parse-false {::exp|type ::exp|false}
    ::exp|parse-var {::exp|type ::exp|var 
                     ::var|name rhs}
    ::exp|parse-lambda (let [{:keys [::lambda|binder ::lambda|body]} rhs
                             {:keys [::lambda|binder|name ::lambda|binder|type]} lambda|binder]
                         (assert (every? some? [lambda|body
                                                lambda|binder|name
                                                lambda|binder|type])
                                 (filterv some? [lambda|body lambda|binder|name]))
                         {::exp|type ::exp|lambda
                          ::lambda|name lambda|binder|name
                          ::lambda|type (parse-type lambda|binder|type)
                          ::lambda|body (parse-exp lambda|body)})
    ::exp|parse-app (let [{:keys [::app|fn ::app|args]} rhs]
                      {::exp|type ::exp|app
                       ::app|fn   (parse-exp app|fn)
                       ::app|args (mapv parse-exp app|args)})))

(s/def ::exp|type keyword?)


(defmultispec exp-type
  ::exp|type
  ::exp|false (s/keys :req [::exp|type])
  ::exp|if (s/keys :req [::exp|type
                         ::if|test
                         ::if|then
                         ::if|else])
  ::exp|var (s/keys :req [::exp|type
                          ::var|name])
  ::exp|app (s/keys :req [::exp|type
                          ::app|fn
                          ::app|args])
  ::exp|lambda (s/keys :req [::exp|type
                             ::lambda|name
                             ::lambda|type
                             ::lambda|body]))

(s/def ::ast|exp (s/multi-spec exp-type ::exp|type))

(s/def ::if|test ::ast|exp)
(s/def ::if|then ::ast|exp)
(s/def ::if|else ::ast|exp)

(s/def ::var|name symbol?)

(s/def ::lambda|name symbol?)
(s/def ::lambda|type ::ast|type)
(s/def ::lambda|body ::ast|exp)

(s/def ::app|fn ::ast|exp)
(s/def ::app|args (s/coll-of ::ast|exp []))

(s/fdef parse-exp
  :args (s/cat :s ::exp|parse)
  :ret ::ast|exp)

;;  t  ::= [x :- t -> t] | (not t) | (or t t) | (and t t) | #f | N | Any
(defmacro parse-type-template
  "Defines the spec for types, with varying `t`."
  [t]
  `(s/or ; N
         ::type|N     '#{~'N}
         ; false
         ::type|false false?
         ; Any
         ::type|Any   '#{~'Any}
         ; [x :- t -> t]
         ::type|fn    (s/tuple
                        ;::name 
                        symbol?
                        ;::turnstile 
                        #{:-}
                        ;::dom 
                        ~t
                        ;::arrow 
                        '#{~'->}
                        ;::rng 
                        ~t)
         ; (not t)
         ::type|not   (s/cat ::not #{'not}
                             ::not|type ~t)
         ::type|or    (s/cat ::or #{'or}
                             ::or|types (s/* ~t))
         ::type|and   (s/cat ::and #{'and}
                             ::types (s/* ~t))))

;  t  ::= [x :- any -> any] | (not any) | (or any*) | (and any*) | #f | N | Any
;; suitable for destructuring
(s/def ::type|parse1
  (parse-type-template ::s/any))

;  t  ::= [x : t -> t] | (not t) | (or t t) | (and t t) | #f | N | Any
;; suitable for gen testing
(s/def ::type|parse
  (parse-type-template ::type|parse))

;; type->ast
(defn parse-type [s]
  (parse-spec ::type|parse1 s
    rhs
    ::type|Any {::type|type ::type|Any}
    ::type|N {::type|type ::type|N}
    ::type|false {::type|type ::type|false}
    ::type|fn (let [[name _ dom _ rng] rhs]
                {::type|type ::type|fn
                 ::fn|name name
                 ::fn|dom (parse-type dom)
                 ::fn|rng (parse-type rng)})
    ::type|not (let [{:keys [::not|type]} rhs]
                 {::type|type ::type|not
                  ::not|type (parse-type not|type)})
    ::type|or (let [{:keys [::or|types]} rhs]
                {::type|type ::type|or
                 ::or|types (mapv parse-type or|types)})
    ::type|and (let [{:keys [::and|types]} rhs]
                 {::type|type ::type|and
                  ::and|types (mapv parse-type and|types)})))

(s/def ::type|type keyword?)

(defmultispec type-type 
  ::type|type
  ::type|false (s/keys :req [::type|type])
  ::type|Any (s/keys :req [::type|type])
  ::type|N (s/keys :req [::type|type])
  ::type|not (s/keys :req [::type|type
                           ::not|type])
  ::type|or
  (s/keys :req [::type|type
                ::or|types])
  ::type|and
  (s/keys :req [::type|type
                ::and|types])
  ::type|fn
  (s/keys :req [::type|type
                ::fn|name
                ::fn|dom
                ::fn|rng]))

(s/def ::and|types (s/coll-of ::ast|type []))

(s/def ::or|types (s/coll-of ::ast|type []))

(s/def ::not|type ::ast|type)

(s/def ::fn|name symbol?)
(s/def ::fn|dom ::ast|type)
(s/def ::fn|rng ::ast|type)

(s/def ::ast|type (s/multi-spec type-type ::type|type))

(s/fdef parse-type
  :args (s/cat :s ::type|parse)
  :ret ::ast|type)

;; Tests

(s/instrument #'parse-exp)
(s/instrument #'parse-type)

(count
  (mapv parse-exp
        (binding [s/*recursion-limit* 2]
          (gen/sample (s/gen ::exp|parse) 20))))

(count
  (mapv parse-type
        (binding [s/*recursion-limit* 2]
          (gen/sample (s/gen ::type|parse) 20))))
