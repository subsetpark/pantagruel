(use testament)

(import spork/path)

(import /pantagruel)

(setdyn :exit-on-error false)

(def tests (->> (os/dir "priv") (filter |(string/has-suffix? ".pant" $))))

(deftest integration
  (each test tests
    (let [full-path (path/join "priv" test)
          src (slurp full-path)
          [succ _] (protect (pantagruel/handle-src full-path src))]

      (is succ))))

(run-tests!)
