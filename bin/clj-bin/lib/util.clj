(ns lib.util
  (:require [clojure.string :as string]
            [clojure.java.shell :as shell]))

(defn sh [cmd & fstrings]
  (->> (apply format cmd fstrings)
       (shell/sh "sh" "-c")
       (:out)
       (string/split-lines)))

(defn shh [& args]
  (first (apply sh args)))

(defn txtw [s]
  (int (if (System/getenv "ESHELL")
         (parse-long (string/trim (:out (shell/sh "elisp" (format "(ns/string-width \"%s\")" s)))))
         (+ (* 1 (count (filter #(< 255 (int %)) s)))
            (* 1 (count (filter #(> 255 (int %)) s)))))))

(defn longest-txtw [lines]
  (last (sort (map txtw lines))))

(comment
  (
   longest-txtw
   [
    "ars"
    "arst"
    ]
   )

  )
