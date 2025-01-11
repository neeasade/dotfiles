#!/usr/bin/env bb
;; I want one media button

(ns player.sh
  (:require [clojure.string :as string]
            [clojure.java.shell :as shell]
            [lib.util :refer [sh]]))

(defn get-players []
  (let [players (->> (interleave (map (fn [p] (keyword (first (string/split p #"\."))))
                                      (sh "playerctl -l"))
                                 (map #(= "Playing" %)
                                      (sh "playerctl -a status")))
                     (partition 2)
                     (map vec)
                     (distinct))]
    ;; ensure mpd at the end, mpv up front
    (vec (concat (filter #(= :mpv (first %)) players)
                 (->> players
                      (remove #(#{:mpd :mpv} (first %)))
                      ;; prioritize playing players
                      (sort-by second)
                      (reverse))
                 (filter #(= :mpd (first %)) players)))))

(defn select-player [players]
  (let [mpd-playing? (:mpd (into {} players))]
    (if mpd-playing? "mpd"
        (name (ffirst players)))))

(let [args (condp = (first *command-line-args*)
             "toggle" ["play-pause"]
             "-f" ["metadata" "-f" (second *command-line-args*)]
             "volume" ["volume" (second *command-line-args*)]
             ["play-pause"])]
  (println
   (loop [players (get-players)]
     (if (empty? players)
       "No players found!"
       (let [result (apply shell/sh "playerctl" "-p" (select-player players) args)
             success? (zero? (:exit result))]
         (if success?
           (:out result)
           (recur (rest players))))))))
