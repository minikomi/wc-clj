(def project 'wave-collapse)
(def version "0.1.0-SNAPSHOT")

(set-env! :resource-paths #{"resources" "src"}
          :source-paths   #{"test"}
          :dependencies   '[[org.clojure/clojure "RELEASE"]
                            [adzerk/boot-test "RELEASE" :scope "test"]
                            [samestep/boot-refresh "0.1.0" :scope "test"]
                            [metosin/bat-test "0.4.2"
                             :exclude #{org.clojure.tools.reader}]
                            [http-kit "2.3.0"]
                            ])

(require
 '[samestep.boot-refresh :refer [refresh]]
 '[metosin.bat-test :refer [bat-test]])

(task-options!
 pom {:project     project
      :version     version
      :description "FIXME: write description"
      :url         "http://example/FIXME"
      :scm         {:url "https://github.com/yourname/art-parties"}
      :license     {"Eclipse Public License"
                    "http://www.eclipse.org/legal/epl-v10.html"}})

(deftask cider "CIDER profile" []
  (alter-var-root #'clojure.main/repl-requires conj
                  '[sekistone.server.repl :refer [start! stop! restart!]])
  (require 'boot.repl)
  (swap! @(resolve 'boot.repl/*default-dependencies*)
         concat '[[cider/cider-nrepl "0.20.1-SNAPSHOT"]
                  [nrepl "0.5.3"]
                  [refactor-nrepl "2.4.0"]])
  (swap! @(resolve 'boot.repl/*default-middleware*)
         concat '[cider.nrepl/cider-middleware
                  refactor-nrepl.middleware/wrap-refactor])
  (repl :server true))

(deftask dev-repl
  []
  (comp
   (cider)
   (watch)
   (refresh)))
