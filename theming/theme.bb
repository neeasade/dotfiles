#!/usr/bin/env bb

(require '[clojure.core :as core])
(require '[clojure.string :as string])
(require '[selmer.parser :as template])
(require '[clojure.walk :refer [postwalk]])

(defn missing-value-fn [tag context-map]
  ;; this can also return string if preferable
  ;; todo: might bubble up str exit status or something
  (throw (Exception. (str "<Missing value: " (or (:tag-value tag) (:tag-name tag)) ">"))))

(selmer.util/set-missing-value-formatter! missing-value-fn)

(defn str->int [str]
  (when (re-matches (re-pattern "\\d+") str)
    (read-string str)))

(defn render-nodes-with-siblings [conf-map]
  (postwalk
   (fn [n]
     (if (and (map? n)
              (not-any? map? (keys n)))
       ;; sibling render pass
       (into {}
             (map (fn [[k v]]
                    {k (if (string? v)
                         (let [render-result (template/render v (merge conf-map n))]
                           (if-let [x (str->int render-result)]
                             x
                             render-result))
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
      {:fonts
       {:> (fn [[k v]]
             {k (merge v
                       {:spec "{{family}}-{{size}}"
                        :spec-space "{{family}} {{size}}"})})
        :variable {:family "Droid Sans" :size 12}
        :mono {:family "Droid Sans Mono" :size 12}
        :icon {:family "Font Awesome 5 Free" :size 10}
        :panel {:family "{{fonts.variable.family}}" :size "{{fonts.variable.size}}"}}
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
       :mkb {:complete
             "temp"
             }
       :gtk {:theme "Raleigh"}
       }
      conf (merge base-conf colors)
      ]
  ;; (get-in conf [:fonts :variable :family])
  (->> conf
       (postwalk
        (fn [n]
          (if (map? n)
            (into {}
                  (map (fn [[k v]] {k v}) n))
            n)))

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
       (postwalk
        (fn [n]
          (if (and (map? n)
                   (:> n))
            (into {} (map (:> n) (dissoc n :>)))
            n)))
       ;; unsure how many passes to do
       (render-nodes-with-siblings)

       (postwalk
        (fn [n]
          (if (map? n)
            (apply dissoc n (filter (fn [k] (string/ends-with? (pr-str k) "-"))
                                    (keys n)))
            n)))
       (template/render "{{bspwm.border.normal}}")
       ;; (template/render "{{fonts.panel.family}}")
       )
  )

;; (template/render "{{name}}" {:name "test"})
