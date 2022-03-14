#!/usr/bin/env bb

(require '[clojure.core :as core])
(require '[clojure.java.shell :as shell])
(require '[clojure.string :as string])
(require '[selmer.parser :as template])
(require '[clojure.walk :refer [postwalk prewalk walk]])

(defn missing-value-fn [tag context-map]
  ;; this can also return string if preferable
  ;; todo: might bubble up str exit status or something
  (throw (Exception. (str "<Missing value: " (or (:tag-value tag) (:tag-name tag)) ">"))))

(defn retain-value-fn [tag context-map]
  (format "{{%s}}"
          (:tag-value tag)))

(selmer.util/set-missing-value-formatter! missing-value-fn)

(selmer.util/set-missing-value-formatter! retain-value-fn)

(defn str->int [str]
  (when (re-matches (re-pattern "\\d+") str)
    (read-string str)))

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
                               ;; _ (println (format "looking at: %s %s" k v))
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

;; dsl:
;; <name>- : don't include in output
;; :& [[:path :to :thing]] - paths to merge into current map
;; :> map (fn [[k v]]) to all nodes

(let [
      colors
      {:colors ["#E8EBEC" "#006E96" "#007C00" "#C38418" "#0065C8" "#407EE7" "#C6007F" "#444748" "#6A6D6E" "#006E96" "#007C00" "#C38418" "#0065C8" "#407EE7" "#C6007F" "#444748"],
       :color {:focused {:background "#a4cfed", :foreground "#093553", :faded "#305b79", :primary "#ac0065", :assumed "#0053b6", :alt "#005e86", :strings "#006c00"}, :normal {:background "#e8ebec", :foreground "#444748", :faded "#6a6d6e", :primary "#c6007f", :assumed "#0065c8", :alt "#006e96", :strings "#007c00"}, :weak {:background "#d5d6d7", :foreground "#393a3b", :faded "#5f6061", :primary "#ba0073", :assumed "#005dc0", :alt "#00668e", :strings "#007400"}, :strong {:background "#ccc9ca", :foreground "#343132", :faded "#5a5758", :primary "#ac0065", :assumed "#0053b6", :alt "#005e86", :strings "#006c00"}, :cursor "#0065c8"}}
      base-conf
      {:font
       {:> (fn [[k v]]
             {k (merge v
                       {:spec "{{family}}-{{size}}"
                        :spec-space "{{family}} {{size}}"})})
        :variable {:family "Droid Sans" :size 12}
        :mono {:family "Droid Sans Mono" :size 12}
        :icon {:family "Font Awesome 5 Free" :size 10}
        :panel {:family "{{font.variable.family}}" :size "{{font.variable.size}}"}}
       :bspwm
       {:window-gap 10
        :split-ratio 0.5
        :desktop-names "00 01 10 11"
        :monocle-window-percent 0.53
        :bspwmrc-extend ""
        :border {:width 3
                 :normal "{{colors.0}}"
                 :active "{{colors.8}}"
                 :focused "{{colors.7}}"
                 :presel "{{colors.8}}"}
        }
       :mkb {:bar- "-+-"
             :complete (fn [tree] (-> tree (get-in [:mkb :bar-]) (nth 2) (str)))
             :sep (fn [tree] (-> tree (get-in [:mkb :bar-]) (nth 1) (str)))
             :empty (fn [tree] (-> tree (get-in [:mkb :bar-]) (nth 0) (str)))
             :start  ""
             :end  ""}
       :gtk {:theme "Raleigh"}
       :st {:font "{{font.mono.spec}}"
            :cursorshape 2
            :cursorthickness 2
            :prompt_char "$"}
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
                      :out-step 0.1}
               }
       :mpd (fn [_]
              (if (string/includes?
                   (:out (shell/sh "mount"))
                   (format "%s/usb" (System/getenv "HOME")))
                {:music-dir "{{env.HOME}}/usb/Music"
                 :mpd-dir "{{env.HOME}}/.config/mpd_portable"}
                {:music-dir "{{env.HOME}}/Music"
                 :mpd-dir "{{env.HOME}}/.config/mpd"}))
       ;; :qutebrowser {:statusbar
       ;;               :font {:size (fn [conf-tree]
       ;;                              )}
       ;;               }
       }
      env {:env (->> (System/getenv)
                     (map (fn [[k v]] {(keyword k) v}))
                     (into {}))}
      conf (merge base-conf colors env)
      ]
  ;; (get-in conf [:fonts :variable :family])
  (->> conf
       ;; call things that are functions with tree
       (postwalk
        (fn [n]
          (if (map? n)
            (->> n
                 (map (fn [[k v]]
                        {k (if (and (fn? v)
                                    (not (= k :>)))
                             (v conf) v)}))
                 (into {}))
            n)))

       ;; merge bois :&
       (postwalk
        (fn [n]
          (if (and (map? n)
                   (:& n))
            (let [merge-maps (map (fn [path] (get-in conf path))
                                  (:& n))]
              (dissoc
               (apply merge n merge-maps)
               :&))
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
       ((fn [tree] (dissoc tree :env)))
       (template/render "{{st.font}}")
       ;; (template/render "{{font.panel.family}}")
       )
  )

;; (template/render "{{name}}" {:name "test"})
