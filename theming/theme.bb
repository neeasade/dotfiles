#!/usr/bin/env bb
;; -*- mode: clojure -*-

(require '[clojure.java.shell :as shell]
         '[clojure.string :as string]
         '[clojure.tools.cli :as cli]
         '[selmer.parser :as template]
         '[clojure.walk :refer [postwalk prewalk]])

(defn flatten-map
  "{:a {:b 1}} -> {:a.b 1}"
  ([m] (flatten-map m "."))
  ([m separator]
   (into {} (flatten-map m separator nil)))
  ([m separator pre]
   (mapcat (fn [[k v]]
             (let [prefix (if pre (str pre separator (name k)) (name k))]
               (if (map? v)
                 (flatten-map v separator prefix)
                 [[(keyword prefix) v]])))
           m)))

(defn unflatten-map
  "{:a.b 1} -> {:a {:b 1}}"
  ([m] (unflatten-map m #"\."))
  ([m separator-re]
   (reduce
    (fn [m [k v]]
      (let [keyword-path (map keyword (string/split (name k) separator-re))]
        (assoc-in m keyword-path v)))
    {} m)))

(defn map-to-shell [m]
  (->> (flatten-map m "_")
       (map (fn [[k v]]
              (format "%s='%s'"
                      ;; todo: handle strings with "'"s in them
                      (-> k name (string/replace "-" "_"))
                      v)))
       (string/join "\n")))

(defn narrow [string-path m]
  (->> (string/split string-path #"\.")
       (map keyword)
       (get-in m)))

(defn promote [string-path m]
  (merge m (narrow string-path m)))

(defn str->int [str]
  (when (re-matches (re-pattern "\\d+") str)
    (read-string str)))

(defn missing-value-fn [tag context-map]
  (throw (Exception. (str "<Missing value: " (or (:tag-value tag) (:tag-name tag)) ">"))))

(defn retain-value-fn [tag context-map]
  (format "{{%s}}" (:tag-value tag)))

(selmer.util/set-missing-value-formatter! missing-value-fn)
;; (selmer.util/set-missing-value-formatter! retain-value-fn)

;; theme map dsl (resolved in this order):
;; :& [[:path :to :thing]] - paths to merge into current map
;; :> map (fn [[k v]]) to all nodes
;; <name>- : don't include in output
(defn get-theme []
  (let [home? (= "erasmus" (.. java.net.InetAddress getLocalHost getHostName))
        work? true

        colors {:colors ["#E8EBEC" "#006E96" "#007C00" "#C38418" "#0065C8" "#407EE7" "#C6007F" "#444748" "#6A6D6E" "#006E96" "#007C00" "#C38418" "#0065C8" "#407EE7" "#C6007F" "#444748"],
                :color {:focused {:background "#a4cfed", :foreground "#093553", :faded "#305b79", :primary "#ac0065", :assumed "#0053b6", :alt "#005e86", :strings "#006c00"}, :normal {:background "#e8ebec", :foreground "#444748", :faded "#6a6d6e", :primary "#c6007f", :assumed "#0065c8", :alt "#006e96", :strings "#007c00"}, :weak {:background "#d5d6d7", :foreground "#393a3b", :faded "#5f6061", :primary "#ba0073", :assumed "#005dc0", :alt "#00668e", :strings "#007400"}, :strong {:background "#ccc9ca", :foreground "#343132", :faded "#5a5758", :primary "#ac0065", :assumed "#0053b6", :alt "#005e86", :strings "#006c00"}, :cursor "#0065c8"}}

        base-conf
        {:font {:> (fn [[k v]]
                     {k (merge v
                               {:spec "{{family}}-{{size}}"
                                :spec-space "{{family}} {{size}}"})})
                :variable {:family "Droid Sans" :size 12}
                :mono {:family "Droid Sans Mono" :size 12}
                :icon {:family "Font Awesome 5 Free" :size 10}
                :panel {:family "{{font.variable.family}}" :size "{{font.variable.size}}"}}
         :bspwm {:window-gap 10
                 :split-ratio 0.5
                 :desktop-names "00 01 10 11"
                 :monocle-window-percent 0.53
                 :bspwmrc-extend ""
                 :border {:width 3
                          :normal "{{colors.0}}"
                          :active "{{colors.8}}"
                          :focused "{{colors.7}}"
                          :presel "{{colors.8}}"}}
         :mkb {:bar- "-+-"
               :complete (fn [tree] (-> tree (get-in [:mkb :bar-]) (nth 2) (str)))
               :sep (fn [tree] (-> tree (get-in [:mkb :bar-]) (nth 1) (str)))
               :empty (fn [tree] (-> tree (get-in [:mkb :bar-]) (nth 0) (str)))
               :start ""
               :end  ""}
         :gtk {:theme "Raleigh"}
         :st {:font "{{font.mono.spec}}"
              :cursorshape 2
              :cursorthickness 2
              :borderpx 0}
         :shell {:prompt "$"
                 :prompt_err "!"}
         :x {:padding 4}
         :picom {:frame {:opacity 1.0}
                 :shadow {:opacity 0.4
                          :enabled false
                          :radius 1
                          :offset-x 1
                          :offset-y 1
                          :red 0.0
                          :blue 0.0
                          :green 0.0}
                 :fade {:enabled false
                        :in-step 0.1
                        :out-step 0.1}}
         :mpd (when home?
                (fn [_]
                  (if (string/includes?
                       (:out (shell/sh "mount"))
                       (format "%s/usb" (System/getenv "HOME")))
                    {:music-dir "{{env.HOME}}/usb/Music"
                     :mpd-dir "{{env.HOME}}/.config/mpd_portable"}
                    {:music-dir "{{env.HOME}}/Music"
                     :mpd-dir "{{env.HOME}}/.config/mpd"})))
         :panel {:height 10}
         ;; :qutebrowser {:statusbar
         ;;               :font {:size (fn [conf-tree]
         ;;                              )}
         ;;               }
         }
        env {:env (->> (System/getenv)
                       (map (fn [[k v]] {(keyword k) v}))
                       (into {}))}

        home-theme (when home?
                     (unflatten-map
                      {:THEME_NAME "test"
                       :emacs.theme "tarp-mcfay"
                       :BG_COMMAND "${HOME}/.fehbg"
                       ;; :BG_COMMAND "hsetroot -solid '{{.color.focused.background}}'"
                       :font.mono.family "Triplicate T4c"
                       :font.variable.family "Equity Text B"
                       :mkb.bar- "AAH"
                       :shell.prompt "%"
                       :bspwm.window-gap 40
                       :x.padding 5
                       :bspwm.monocle-window-percent 0.55
                       :font.mono.size 12
                       :font.variable.size 13
                       :picom.shadow.enabled false
                       :bspwm.bspwmrc-extend "tag_borders &\n subscription_rules &"}))

        work-theme (when work?
                     (unflatten-map
                      {:THEME_NAME "test"
                       :emacs.theme "tarp-mcfay"
                       :BG_COMMAND "wallmac tile '{{env.HOME}}/Downloads/mpv-shot0095.jpg'"
                       :font.mono.family "Go Mono"
                       :font.variable.family "Charter"
                       :mkb.bar- "🌳🌻🌱"
                       :shell.prompt "🌳"
                       :shell.prompt_err "😞"
                       :bspwm.window-gap 24
                       :x.padding 5
                       :font.mono.size 18
                       :font.variable.size 18
                       :bspwm.monocle-window-percent 0.55}))
        ]
    (merge-with into
                base-conf
                colors
                env
                home-theme work-theme
                )))


(defn render-nodes-with-siblings [conf-map]
  (prewalk
   (fn [n]
     (if (and (map? n)
              (not-any? map? (keys n)))
       ;; sibling render pass
       (into {}
             (map (fn [[k v]]
                    {k (if (string? v)
                         (let [
                               ;; let top level map win conflicts
                               render-conf (merge conf-map (apply dissoc n (keys conf-map)))
                               render-result (-> v (template/render render-conf))]
                           ;; this is what allows us to make multiple passes:
                           (if (string/includes? render-result "{{")
                             v
                             (if-let [x (str->int render-result)]
                               x
                               render-result)))
                         v)}) n))
       n)) conf-map))

(defn process-theme [theme-tree]
  (->> theme-tree
       ;; call things that are functions with tree
       (postwalk
        (fn [n]
          (if (map? n)
            (->> n
                 (map (fn [[k v]]
                        {k (if (and (fn? v)
                                    (not (= k :>)))
                             (v theme-tree) v)}))
                 (into {}))
            n)))

       ;; merge bois :&
       (postwalk
        (fn [n]
          (if (and (map? n)
                   (:& n))
            (let [merge-maps (map (fn [path] (get-in theme-tree path))
                                  (:& n))]
              (dissoc (apply merge n merge-maps) :&))
            n)))

       ;; map bois :>
       (postwalk
        (fn [n]
          (if (and (map? n)
                   (:> n))
            (into {} (map (:> n) (dissoc n :>)))
            n)))

       ;; unsure how many passes to do
       (render-nodes-with-siblings)
       (render-nodes-with-siblings)

       ;; remove bois-
       (postwalk
        (fn [n]
          (if (map? n)
            (apply dissoc n (filter (fn [k] (string/ends-with? (pr-str k) "-"))
                                    (keys n)))
            n)))
       ((fn [tree] (dissoc tree :env)))))

(def cli-options
  [["-q" "--query PATH" "query path"]
   ["-p" "--promote PATH" "promote path"]
   ["-n" "--narrow PATH" "narrow map to path"]
   ["-R" "--render STRING" "render a string as a template"]
   ["-r" "--render-file FILE" "render a file as a template"
    ;; todo
    :validate [(fn [s] true) "File doesn't exist"]]
   ["-s" "--shell" "output as shell"]
   ["-k" "--keys" "dump the keys"]
   ["-h" "--help" "show the usage"]])

(defn usage []
  (->> cli-options
       (map #(string/join " " (take 3 %)))
       (string/join "\n")
       (println)))

(println
 (reduce
  (fn [theme [k v]]
    ;; (println ("arg: %s" v))
    (condp = k
      :help (reduced (usage))
      :narrow (narrow v theme)
      :query (narrow v theme)
      :promote (promote v theme)
      :shell (map-to-shell theme)
      :keys (->> theme flatten-map keys (map name) (string/join "\n"))
      :render (template/render v theme)
      :render-file (template/render (slurp v) theme)
      theme))
  (process-theme (get-theme))
  (:options (cli/parse-opts *command-line-args* cli-options))))
