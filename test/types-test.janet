(use testament)

(import /pantagruel/stdlib)
(import /pantagruel/types)

(import /test/util :prefix "")

(defn is-type
  [t form env]
  (let [env (table/setproto (table ;(kvs env)) stdlib/base-env)
        resolved (types/resolve-type form env)]
    (is (== t resolved))))

(deftest singleton-test
  (is-type
    stdlib/Nat
    {:kind :sym :text "n"}
    {"n" {:kind :domain
          :type {:args {:tuple-of ()} :yields stdlib/Nat}}}))

(deftest domain-mention-test
  (is-type
    stdlib/Domain
    {:kind :sym :text "Nat"}
    {}))
(run-tests!)
