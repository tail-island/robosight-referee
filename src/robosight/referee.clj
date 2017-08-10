(ns robosight.referee
  (:require   (clojure.core [async         :as async])
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
                                                              (combinatorics/combinations 2))
                                                          (map #(cond-> %
                                                                  (> (rand) 0.5) (reverse))))]
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
        (with-open [writer (PrintWriter. (io/writer to-file))]
          (async/go-loop []
            (when-let [s (.readLine reader)]
              (.println writer s)
              (recur)))
          (.waitFor process)
          (.destroy process))))))

(defn- report
  []
  (->> (-> (io/file "./data/results")
           (.listFiles))
       (map (fn [game-directory]
              (let [[program-name-0 program-name-1] (re-seq #"\d+" (.getName game-directory))]
                [program-name-0
                 program-name-1
                 (with-open [reader (io/reader (format "%s/stdout.txt" (.getPath game-directory)))]
                   (let [winner (clojure.edn/read-string (last (iterator-seq (.iterator (.lines reader)))))]
                     (when-not (contains? winner :winner)
                       (throw (ex-info "wrong stdout...")))
                     (:winner winner)))])))
       (mapcat (fn [[program-name-0 program-name-1 winner]]
                 (case winner
                   0 [[program-name-0 program-name-1 "〇"] [program-name-1 program-name-0 "×"]]
                   1 [[program-name-1 program-name-0 "〇"] [program-name-0 program-name-1 "×"]]
                   [[program-name-1 program-name-0 "ー"] [program-name-0 program-name-1 "ー"]])))
       (sort)
       (clojure.pprint/pprint)  ; TODO: まともに出力する……。
       ))

(defn -main
  [& args]
  (cleanup)
  (fight)
  (report))