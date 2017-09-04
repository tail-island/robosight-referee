(ns robosight.referee
  (:require   (clojure      [string        :as string])
              (clojure.core [async         :as async])
              (clojure.java [io            :as io])
              (clojure.math [combinatorics :as combinatorics]))
  (:import    (java.io PrintWriter))
  (:gen-class :name com.tail_island.robosight.Referee
              :main true))

(def ^:private encoding
  (System/getProperty "file.encoding"))

(defn- cleanup
  []
  (let [results-directory (io/file "./data/results")]
    (when (.exists results-directory)
      (doseq [file (reverse (file-seq results-directory))]
        (io/delete-file file)))))

(defn- fight
  []
  (doseq [[program-directory-1, program-directory-2] (->> (-> (io/file "./data/programs")
                                                              (.listFiles)
                                                              (sort)
                                                              (combinatorics/combinations 2))
                                                          (map #(cond-> %
                                                                  (> (rand) 0.5) (reverse))))]
    (Thread/sleep 3000)  ; CPUが過熱しているかもしれないので、少し休ませます。動いてないとクロックが落ちちゃう問題は、WindowsのPower PlanをHigh Performanceにして回避で。
    (println (format "%s vs. %s" (.getName program-directory-1) (.getName program-directory-2)))
    (let [process (-> (Runtime/getRuntime)
                      (.exec (format "java -jar ./bin/robosight-battlefield-0.1.0-standalone.jar \"cmd /C run.bat\" %s \"cmd /C run.bat\" %s"
                                     (.getPath program-directory-1)
                                     (.getPath program-directory-2))))]
      (doseq [[reader to-file] (map vector
                                    [(io/reader (.getInputStream process) :encoding encoding)
                                     (io/reader (.getErrorStream process) :encoding encoding)]
                                    (map #(io/file (format "%s/%s" %1 %2))
                                         (repeat (format "./data/results/%s-%s" (.getName program-directory-1) (.getName program-directory-2)))
                                         ["stdout.txt" "stderr.txt"]))]
        (io/make-parents to-file)
        (async/go
          (with-open [writer (PrintWriter. (io/writer to-file))]
            (loop []
              (when-let [s (.readLine reader)]
                (.println writer s)
                (recur))))))
      (.waitFor process)
      (.destroy process))))

(defn- results
  []
  (let [results (apply hash-map (->> (-> (io/file "./data/results")
                                         (.listFiles))
                                     (map (fn [game-directory]
                                            (let [[program-name-0 program-name-1] (re-seq #"\d+" (.getName game-directory))]
                                              [program-name-0
                                               program-name-1
                                               (with-open [reader (io/reader (format "%s/stdout.txt" (.getPath game-directory)))]
                                                 (let [winner (clojure.edn/read-string (last (iterator-seq (.iterator (.lines reader)))))]
                                                   (when-not (contains? winner :winner)
                                                     (throw (ex-info "wrong stdout..." {})))
                                                   (:winner winner)))])))
                                     (mapcat (fn [[program-name-0 program-name-1 winner]]
                                               (case winner
                                                 0 [[program-name-0 program-name-1] 1.0 [program-name-1 program-name-0] 0.0]
                                                 1 [[program-name-1 program-name-0] 1.0 [program-name-0 program-name-1] 0.0]
                                                 [[program-name-1 program-name-0] 0.5 [program-name-0 program-name-1] 0.5])))))
        program-names (->> (keys results)
                           (map first)
                           (distinct)
                           (sort))]
    (map (fn [program-name-0]
           (map (fn [program-name-1]
                  (get results [program-name-0 program-name-1]))
                program-names))
         program-names)))

(defn -main
  [& args]
  (cleanup)
  (fight)
  (let [results (results)]
    (doseq [columns results]
      (println (string/join "\t" (cons (reduce + (keep identity columns)) columns))))
    (clojure.pprint/pprint results)))
