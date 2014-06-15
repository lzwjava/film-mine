(ns datamine.core
  (:require [org.httpkit.client :as http]
            [net.cgrand.enlive-html :as html])
  (:use [clojure.pprint])
  (:import java.net.URL
           java.lang.String
           org.jsoup.Jsoup))

(def filmId 12231)
(def baseUrl "http://movie.mtime.com/%d/comment")

(defn- contains-substring? [^String s ^String substring]
  (and s (<= 0 (.indexOf s substring))))

(defn formatUrl
  [id]
  (format baseUrl filmId))

(defn get-base-url
  []
  (formatUrl filmId))

(defn getHtmlFromUrl [url]
  (-> url URL. html/html-resource))

(defn getHtmlByFile []
  (html/html-resource "comment.html"))

(defn get-all-right-links
  [url pred]
  (let [id filmId
        htmlRes (getHtmlFromUrl url)
        lst (html/select htmlRes [:a])]
    (let [urls (map (fn [x]
                      (-> x :attrs :href))
                    (first (vector lst)))]
      (distinct (filter pred urls)))))

(defn right-link?
  [x]
  (and (not (nil? x))
       (not (contains-substring? x "#"))
       (contains-substring? x (str filmId))
       (contains-substring? x "review")))

;(get-all-right-links (getTestUrl) right-link?)

(defn get-all-review-links
  []
  (let [base-url (get-base-url)]
    (loop [i 1
           links []]
      (let [url (str base-url
                     (if (= i 1)
                       ""
                       (str "-" i)) ".html")]
        (if-let [right-links
                 (try
                   (get-all-right-links url right-link?)
                   (catch Exception e
                     (str "exception:" (.getMessage e))
                     nil))]
          (do
            (println (type right-links))
            (recur (inc i) (concat links right-links)))
          links)))))

(defn -main
  []
  (let [links (get-all-right-links)]))

(defn get-text-of-a-link
  []
  (let [res (getHtmlByFile)
        str (slurp "resources/comment.html")
        doc (-> (Jsoup/connect "http://movie.mtime.com/12231/reviews/912306.html")
                .get)]
    (let [matcher (->> (.body doc) .text (re-matcher #"[\"“]([a-zA-Z0-9,.，。：'\s]*)[\"”]"))]
      (loop [res (re-find matcher)
             acc []]
        (if res
          (let [next (re-find matcher)]
            ;(println res)
            (recur next (conj acc (res 1))))
          acc)))))

;(conj [] "1")
(re-find #"[\"“]([a-zA-Z0-9,.，。\s]*)[\"”]" "比如安迪在逃狱后给瑞德的信中所说的：“Remember， Hope is a good thing， maybe the best of things
and
no good thing ever dies。” 或许这正是作品和导演想告诉我们的。“Fear can hold you prisoner， Hope can set you free。”这句话已成名言。")
