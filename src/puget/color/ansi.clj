(ns puget.color.ansi
  "Coloring implementation that applies ANSI color codes to text designed to be
  output to a terminal. Now with hex color support for both foreground and background.
  Use with a `:color-markup` of `:ansi`."
  (:require
   [clojure.string :as str]
   [puget.color :as color]))

(def sgr-code
  "Map of symbols to numeric SGR (select graphic rendition) codes."
  {:none        0
   :bold        1
   :underline   3
   :blink       5
   :reverse     7
   :hidden      8
   :strike      9
   :black      30
   :red        31
   :green      32
   :yellow     33
   :blue       34
   :magenta    35
   :cyan       36
   :white      37
   :fg-256     38
   :fg-reset   39
   :bg-black   40
   :bg-red     41
   :bg-green   42
   :bg-yellow  43
   :bg-blue    44
   :bg-magenta 45
   :bg-cyan    46
   :bg-white   47
   :bg-256     48
   :bg-reset   49})

(defn hex-to-rgb
  "Convert a hex color string to RGB values"
  [hex]
  (let [hex (if (= \# (first hex)) (subs hex 1) hex)
        rgb (partition 2 hex)]
    (mapv #(Integer/parseInt (apply str %) 16) rgb)))

(defn esc
  "Returns an ANSI escape string which will apply the given collection of SGR
  codes or hex colors, including background colors prefixed with 'bg#'."
  [codes]
  (let [process-code (fn [code]
                       (cond
                         (keyword? code) (sgr-code code code)
                         (string? code)
                         (if (str/starts-with? code "bg#")
                           (let [[r g b] (hex-to-rgb (subs code 2))]
                             (str "48;2;" r ";" g ";" b))
                           (let [[r g b] (hex-to-rgb code)]
                             (str "38;2;" r ";" g ";" b)))
                         :else code))
        codes (map process-code codes)
        codes (str/join \; codes)]
    (str \u001b \[ codes \m)))

(defn escape
  "Returns an ANSI escape string which will enact the given SGR codes or hex colors."
  [& codes]
  (esc codes))

(defn sgr
  "Wraps the given string with SGR escapes to apply the given codes or hex colors, then reset
  the graphics."
  [string & codes]
  (str (esc codes) string (escape :none)))

(defn strip
  "Removes color codes from the given string."
  [string]
  (str/replace string #"\u001b\[[0-9;]*[mK]" ""))

(defmethod color/document :ansi
  [options element document]
  (if-let [codes (-> options :color-scheme (get element))]
    (let [codes (if (or (string? codes) (keyword? codes)) [codes] codes)]
      [:span [:pass (esc codes)] document [:pass (escape :none)]])
    document))

(defmethod color/text :ansi
  [options element text]
  (if-let [codes (-> options :color-scheme (get element))]
    (let [codes (if (or (string? codes) (keyword? codes)) [codes] codes)]
      (str (esc codes) text (escape :none)))
    text))
