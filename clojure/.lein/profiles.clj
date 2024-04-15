{:user
 {:plugins [;; Start a REPL with the given library in the classpath
            [lein-try/lein-try "0.4.3"]

            ;; Run the tests on change in source files
            [com.jakemccrary/lein-test-refresh "0.25.0"]

            ;; Print a complete leiningen project map
            [lein-pprint/lein-pprint "1.3.2"]

            ;; Interactive CLI for lein commands
            [io.github.ujihisa/lein-interactive "1.0.0"]

            ;; Run tests quick and nicely formatted
            [lein-eftest/lein-eftest "0.6.0"]

            ;; Run coverage tests
            [lein-cloverage/lein-cloverage "1.2.4"]

            ;; Run linting based on clj-kondo
            [com.github.clj-kondo/lein-clj-kondo "0.2.4"]]}}
