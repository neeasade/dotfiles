#!/usr/bin/env bb
;; I want one media button

(ns player.sh
  (:require [clojure.string :as string]
            [clojure.java.shell :as shell]
            [lib.util :refer [sh shh]]))

(defn get-players []
  ;; player shape: [:name playing?]
  (let [players (->> (interleave (map (fn [p] (keyword (first (string/split p #"\."))))
                                      (sh "playerctl -l"))
                                 (map #(= "Playing" %)
                                      (sh "playerctl -a status")))
                     (partition 2)
                     (map vec)
                     (distinct))]
    ;; mpv -> vlc -> other -> mpd
    (->> (concat (filter #(= :mpv (first %)) players)
                 (filter #(= :vlc (first %)) players)
                 (remove #(#{:mpd :mpv :vlc} (first %)) players)
                 (filter #(= :mpd (first %)) players))
         (sort-by second #(compare %2 %1)) ; prioritize playing players
         vec)))

(defn select-player [players]
  (let [mpd-playing? (:mpd (into {} players))]
    (if mpd-playing? "mpd"
        (name (ffirst players)))))

(defn percent [s]
  (Math/round
   (* (parse-double s)
      100)))

(let [cmd (first *command-line-args*)
      args (condp = cmd
             "toggle" ["play-pause"]
             "-f" ["metadata" "-f" (second *command-line-args*)]
             "volume" ["volume" (second *command-line-args*)]
             *command-line-args*
             ;; ["play-pause"]
             )]
  (println
   (loop [players (get-players)]
     (if (empty? players)
       "No players found!"
       (let [player (select-player players)
             result (apply shell/sh "playerctl" "-p" player args)
             success? (zero? (:exit result))]
         (if success?
           (if (= cmd "volume")
             (str player " " (percent (shh "playerctl" "-p" player "metadata" "-f" "{{volume}}")))
             (:out result))
           (recur (rest players))))))))
