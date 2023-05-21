(use testament)

(import spork/path)

(import /pantagruel)

(setdyn :exit-on-error false)

(def tests (->> (os/dir "priv") (filter |(string/has-suffix? ".pant" $))))

(deftest integration
  (each test tests
    (let [full-path (path/join "priv" test)
          error-path (string full-path ".error")
          src (slurp full-path)
          available-modules (pantagruel/populate-available-modules {"path" "priv"})
          eval-errors (try
                        (pantagruel/handle-src full-path src available-modules)
                        ([err fib]
                          (is (nil? err)
                              (string/format "[%s] Integration test execution failure: %q" test err))))]

      (if (os/stat error-path)
        (let [type-errors (map (fn [[_ err]] err) eval-errors)
              error-message (parse (slurp error-path))
              assert-message (string/format
                               "[%s]\nExpected type errors: %q\nFound type errors: %q\n"
                               test
                               error-message
                               type-errors)]
          (assert-equivalent error-message type-errors assert-message))

        (is (empty? eval-errors) (string/format "[%s] Integration test errors: %q" test eval-errors))))))

(run-tests!)
