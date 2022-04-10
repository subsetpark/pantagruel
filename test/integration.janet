(use testament)

(import spork/path)

(import /pantagruel)

(setdyn :exit-on-error false)

(def tests (->> (os/dir "priv") (filter |(string/has-suffix? ".pant" $))))

(deftest integration
  (each test tests
    (let [full-path (path/join "priv" test)
          src (slurp full-path)
          available-modules (pantagruel/populate-available-modules "priv")
          [succ res] (protect (pantagruel/handle-src full-path src available-modules))]

      (is succ (string/format "[%s] Integration test failure: %q" test res)))))

(run-tests!)
