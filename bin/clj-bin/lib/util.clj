(ns lib/util
  (:require [clojure.string :as string]
            [clojure.java.shell :as shell]))

(defn txtw [s]
  (int (if (System/getenv "ESHELL")
         (parse-long (string/trim (:out (shell/sh "elisp" (format "(ns/string-width \"%s\")" s)))))
         (+ (* 1 (count (filter #(< 255 (int %)) s)))
            (* 1 (count (filter #(> 255 (int %)) s)))))))
