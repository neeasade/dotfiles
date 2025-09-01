(ns lib.util
  (:require [clojure.string :as string]
            [clojure.java.shell :as shell]))

;; todo: add the time macro here used in elisp

(defn url-decode [s]
  (-> s
      (string/replace "+" " ")
      (string/replace #"%([0-9A-Fa-f]{2})"
                      (fn [[_ hex]]
                        (str (char (Integer/parseInt hex 16)))))))

(defn stderr
  "println to stderr"
  [s]
  (binding [*out* *err*]
    (println s )))

(defn home [path]
  (str (System/getenv "HOME") "/" path))

(defn sh
  ([cmd] (->> (shell/sh "bash" "-c" cmd)
              (:out)
              (string/split-lines)))
  ([& cmds]
   (->> (apply shell/sh cmds)
        (:out)
        (string/split-lines))))

(defn shh [& args]
  ;; todo: nil pun?
  (first (apply sh args)))

(defn has? [exec]
  (not (clojure.string/blank? (shh (str "which " exec)))))

(defn dmenu [lines]
  (shh "dmenu" :in (string/join "\n" (map str lines))))

(defn has? [executable]
  (not (string/blank? (shh "which" executable))))

(defn txtw [s]
  (int (if (System/getenv "ESHELL")
         (parse-long (string/trim (:out (shell/sh "elisp" (format "(ns/string-width \"%s\")" s)))))
         ;; (parse-long (elisp (/ (string-pixel-width s) (string-pixel-width "═"))))
         (+ (* 1 (count (filter #(< 255 (int %)) s)))
            (* 1 (count (filter #(> 255 (int %)) s)))))))

(defn longest-txtw [lines]
  (last (sort (map txtw lines))))

(comment

  (let [s "arst"]
    (parse-long (elisp (/ (string-pixel-width s) (string-pixel-width "═")))))

  (txtw "arst")

  (shh "echo a")
  (shh "echo" "a")

  (read-string (elisp (ns/emacs-to-theme)))

  )
