#!/usr/bin/env bb

(require '[clojure.core :as core])
(require '[clojure.string :as string])
(require '[selmer.parser :as template])
(require '[clojure.walk :refer [postwalk]])


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
                           (if (-> render-result read-string number?)
                             (read-string render-result)
                             render-result)
                           )
                         v
                         )}) n))
       n)) conf-map))

;; dsl:
;; <name>- : don't include in output
;; :& [[:path :to :thing]] - paths to merge into current map
;; :> map (fn [[k v]]) to all nodes

(let [conf
      {:fonts
       {:> (fn [[k v]]
             {k (merge v
                       {:spec "{{family}}-{{size}}"
                        :spec-space "{{family}} {{size}}"})})
        :variable {:family "Droid Sans" :size 12}
        :mono {:family "Droid Sans Mono" :size 12}
        :icon {:family "Font Awesome 5 Free" :size 10}
        :panel {:family "{{font.variable.family}}" :size ""}
        }
       ;; :gtk {:theme "Raleigh"}
       }
      ]
  (->> conf
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
       ;; (postwalk
       ;;  (fn [n]
       ;;    (if (and (map? n)
       ;;             (:> n))
       ;;      (into {} (map (:> n) (dissoc n :>)))
       ;;      n)))
       ;; unsure how many passes to do
       (render-nodes-with-siblings)

       (postwalk
        (fn [n]
          (if (map? n)
            (apply dissoc n (filter (fn [k] (string/ends-with? (pr-str k) "-"))
                                    (keys n)))
            n)))
       (template/render "{{fonts.variable.spec}}")
       )
  )

;; (template/render "{{name}}" {:name "test"})
