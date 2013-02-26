(ns jaen-bot.core
  (:use [lamina.core]
        [aleph.tcp]
        [gloss.core]
        [clojure.java.io])
  (:require [net.cgrand.regex :as r]
            [clojure.string :as s]
            [cheshire.core :as json]))

(def config (atom {}))
(def state (atom :connected))
(def auth-lines (atom 0))
(def users-online (atom #{}))
(def pluspluses (atom {}))
(def parrot-talkbacks (atom #{}))
(def declensions (atom {}))
(def siema-count (atom 0))
(def global-irc-connection (atom nil))

; GOD WHERE ARE MY NAMED CAPTURES
; positionally [0] = (optional) from; [1] = (mandatory) command; [2] = (optional) body
(def line-regex #"^(?::([^\s]+))?([^:]+)(?::(.*))?$")
; [0] = nick [1] = realname
(def nick-regex #"^([^!]+)!~?([^@]+)@.+$")
(def directed-at-regex #"^\s*([^:\s]+)(?:(?::|\s\s*)(.+))?$")
(def point-regex #"\s*([^\+\s]+)(\+\+|--)s*")
(def kick-talkbacks (atom []))
(def talkbacks (atom [])) 

(defn ask-reply
  []
  (read-line))

(defn has-permission
      [realname permission]
      (or (some #{realname} ((@config :permissions) :all))
          (some #{realname} ((@config :permissions) permission))))

(defn directed-at
  [body]
  (if body
      (let [[_ nick message] (re-matches directed-at-regex body)]
            (if message nick))
      nil))

(defn extract-directed-body
  [body]
  (let [[_ nick message] (re-matches directed-at-regex body)]
    message))

(defn msg
  [where & lines]
  (vec (map #(format "PRIVMSG %s :%s" where %) lines)))

(defn merge-response-map
  [& maps]
  (let [new-replies (apply merge (map #(% :replies) maps))]
       (apply merge (flatten [maps {:replies new-replies}]))))

(defn do-declension
  [string]
  (let [[_ basic-declension] (re-find #"([\wąęółżźćńś]+?)owi" string)
        basic-has-alternate-declension (some #{basic-declension} (vals @declensions))
        alternate-declension (@declensions (keyword string))]
        (cond
          alternate-declension alternate-declension
          (and basic-declension (not basic-has-alternate-declension)) basic-declension)))

(defn protocol-reply
  [line]
  (do
    (let [match (re-matches line-regex line)
          from (match 1)
          command-match (s/split (s/trim (match 2)) #"\s+")
          command (first command-match)
          args (vec (rest command-match))
          body (match 3)
          [_ nick realname] (and from (re-matches nick-regex from))
          reply (if (= nick (@config :bot-nick)) ; is this is an acknowledgement?
                      (cond
                        (and (= command "MODE")
                             (let [[nick mode] args]
                                  (and (= nick (@config :bot-nick))
                                       (= mode (@config :bot-mode))))) (do
                                                                         (reset! state :joined)
                                                                         (vec (flatten [(map #(format "JOIN %s" %) (@config :bot-join-channels))]))))
                    (cond
                      (= command "PING") (format "PONG :%s" body)
                      
                      (= command "KICK") (do (println (@config :bot-nick) args)
                                             (if (= (@config :bot-nick) (args 1))
                                                 (vec (flatten [(format "JOIN %s" (args 0)) (msg (args 0) (format "%s: %s" nick (rand-nth @kick-talkbacks)))]))))
                      (= command "PART") (do (swap! users-online disj nick) [])
                      (= command "JOIN") (do (swap! users-online conj nick) [])
                      (= command "NICK") (do (swap! users-online disj nick) (swap! users-online conj (s/trim body)) [])
                      (= command "001") (do (reset! state :authed) [(format "PRIVMSG Q@CServe.quakenet.org :auth %s %s" (@config :bot-nick) (@config :quakenet-password))])
                      (= command "353") (do (doseq [nick (map #((re-matches #"[\+@]?([^@\+]+)" %) 1) (s/split body #"\s+"))] (swap! users-online conj nick)) [])
                      (and (>= @auth-lines 3)
                           (= @state :connected)) [(format "NICK %s" (@config :bot-nick)) (format "USER %s %s a b" (@config :bot-realname) (@config :bot-mode))]
                      (= command "NOTICE") (cond
                                               (= (args 0) "AUTH") (do (println "auth line") (swap! auth-lines inc) []))))]
      (if (not (nil? reply))
          {:line-data { :from [nick realname]
                        :command command
                        :args args
                        :body body }
           :replies { :protocol-reply (cond (vector? reply) reply
                                            true [reply])}}
          {:line-data { :from [nick realname]
                        :command command
                        :args args
                        :body body }} ))))

;OH GOD GLOBAL STATE
(defn rejoin
  [channel]
  (enqueue @global-irc-connection (format "JOIN %s" channel)))

(defn smart-say
  [words where]
  (println "smart-say")
  (let [split-words (s/split words #"\s+")]
        (cond
          (re-find #"(?i).pogoda\s+sosnowiec" words) (msg where "No chance in fucking hell!")
          (re-find #"(?i)cos|coś" (split-words 0)) (let [user (if (contains? split-words 1) (some #{(do-declension (s/replace (split-words 1) #"\?|!|\." ""))} @users-online))]
                                                        (if user
                                                            (msg where (format "%s: %s" user (s/trim (rand-nth @parrot-talkbacks))))
                                                            (msg where (s/trim (rand-nth @parrot-talkbacks)))))
          (some #{(do-declension (split-words 0))} @users-online) (msg where (format "%s: %s" (do-declension (split-words 0)) (s/trim (s/join " " (drop 1 split-words )))))
          :else (msg where words))))

(defn talkback
  [response-map]
  (let [line-data (or (response-map :line-data) {})
        command (line-data :command)
        args (line-data :args)
        body (line-data :body)
        body-lowercase (if body (s/lower-case body))
        [nick realname] (line-data :from)
        can-talkback (not (response-map :inhibit-talkback))
        reply (if can-talkback
                  (if (and (= command "PRIVMSG") (or (some #{(args 0)} (@config :bot-join-channels)) (has-permission realname :privmsg)))
                      (let [from-channel (args 0)
                            drunk-regexp #"(?:([^\s:]+):?\s+)?jest(?:e(?:s|ś))?\s+pijany"]
                        (cond
                           (= (directed-at body)
                              (@config :bot-nick)) (condp re-find body
                                                          #"kick\s+[^\s#]+(\s+#[^\s]+)?" (let [[_ whom from] (re-find #"kick\s+([^\s#]+)(\s+#[^\s]+)?" body)]
                                                                                               (if (and (has-permission realname :kick) (not (= whom (@config :bot-nick))))
                                                                                                    [(format "KICK %s %s :ION CANNONS ONLINE!" (or from from-channel) whom)]
                                                                                                    []))
                                                          #"przywitaj\s+si(e|ę)" (msg from-channel "no witam")
                                                          #"(powiedz|powiesz).*" (let [[_ say-what say-where] (re-find #"(?:powiedz|powiesz)(.*?)(#.*)?$" body)] (smart-say (s/trim say-what) (or say-where from-channel)))
                                                          #"dobry\s*bot!?" (msg from-channel (rand-nth ["^_^" "*_*" ";3" "PURRRR~!"]))
                                                          #"siema!?" (if (> @siema-count (+ 2 (rand-int 3)))
                                                                         (do
                                                                            (.start (Thread. (fn [] (Thread/sleep (+ 30000 (rand-int 30000)))
                                                                                                    (rejoin from-channel))))         
                                                                            (reset! siema-count 0)
                                                                            (Thread/sleep (+ 2200 (rand-int 1800)))
                                                                            (vec (flatten [(msg from-channel "a pierdolcie sie wszyscy!") (format "PART %s" from-channel)] )))
                                                                         (do
                                                                            (Thread/sleep (+ 1800 (rand-int 1200)))
                                                                            (swap! siema-count inc)
                                                                            (msg from-channel (rand-nth ["spierdalaj" "pierdol sie" "zamknie morde"]))))
                                                          nil)
                          (re-find #"party([!\.\?]?|\s|$)" body-lowercase)    (msg from-channel (rand-nth ["Party! Party!! PARTY!!!"]))
                          (re-find drunk-regexp body-lowercase) (let [drunk-name ((re-find drunk-regexp body) 1)]
                                                                (if drunk-name
                                                                     (vec (flatten [(msg from-channel drunk-name)
                                                                                    (msg from-channel "spierdalaj!")
                                                                                    (msg from-channel drunk-name)
                                                                                    (msg from-channel "jesteś pijany! SPIERDALAJ!")]))
                                                                     []))))))]
                                                             ;true (msg from-channel (format "%s: %s" nick (rand-nth @talkbacks))))))))]
  (if reply
      (merge-response-map response-map { :inhibit-parrot true :replies {:talkback-reply reply }})
      response-map)))

(defn parrot
  [response-map]
  (let [line-data (or (response-map :line-data) {})
        body (line-data :body)
        plain-body (if (contains? @users-online (directed-at body)) (extract-directed-body body) body)
        shall-store (<= (rand) (@config :parrot-register-probability))
        shall-reply (and (not (response-map :inhibit-parrot))
                         (or (and (<= (rand) (@config :parrot-reply-probability))
                             (not shall-store))
                             ;(and (= (directed-at body) (@config :bot-nick)) (re-find #"(powiedz\s+cos|ś)|(odezwij\s+sie|ę)|(daj\s+gl|łos)" body)))
                             (= (directed-at body) (@config :bot-nick))))
        reply (if (and (= (line-data :command) "PRIVMSG")
                          (some #{((line-data :args) 0)} (@config :bot-join-channels)))
                  (cond 
                    shall-store (do
                                  (swap! parrot-talkbacks conj plain-body)
                                  (println "Parrot talkback registered:" plain-body) nil)
                    shall-reply (msg ((line-data :args) 0) (s/trim (rand-nth @parrot-talkbacks)))))]
        (if reply
            (do
              (Thread/sleep (+ 1200 (rand-int 1800)))
              (merge-response-map { :replies { :parrot-reply reply } }))
            response-map)))

(defn word-join
  [& words]
  (let
    [commas (drop-last words)
     last-word (last words)]
    (cond
      (empty? commas) last-word
      (= (count commas) 4) (format "%s i %s" (s/join ", " commas) last-word)
      true (format "%s i ktośtam tam pewnie jeszcze" (s/join ", " words)))))

(defn count-pluspluses
  [response-map]
  (let [line-data (or (response-map :line-data) {})
        command (line-data :command)
        args (line-data :args)
        from-channel (if (= command "PRIVMSG") (args 0))
        body (and line-data (line-data :body))
        plus-command (if (do (and body (= (directed-at body) (@config :bot-nick))))
                         (cond
                                (not (nil? (re-find #"(stan plus(plus)?(ó|o)w|(plus)?plusy)" body))) >
                                (not (nil? (re-find #"(stan minus(ó|o)w|minusy)" body))) <
                                :else nil))]
        (if (and (= command "PRIVMSG") (some #{from-channel} (@config :bot-join-channels)))
            (do
              (if plus-command 
                (merge-response-map response-map { :inhibit-parrot true
                                                   :replies {
                                                     :count-pluspluses-reply (apply msg from-channel [(s/join ", " (map (fn [k] (format "%s: %s" k (@pluspluses k)))
                                                                                                      (take 10 (map first (filter #(contains?  @users-online (first %)) (sort-by last plus-command @pluspluses))))))])}
                                                  :inhibit-talkback true })
                (do
                  (let [line-data (or (response-map :line-data) {})
                      body (line-data :body)
                      [nick _] (line-data :from)
                      plusplus-matches (or (and body (re-seq point-regex body)) [])
                      nested-responses (do (map (fn [[_ plussed-nick what]] (if (and (contains? @users-online plussed-nick) (not (= plussed-nick nick)))
                                                                       (do
                                                                         (println @users-online plussed-nick nick)
                                                                         (let
                                                                           [op (cond (= what "++") inc (= what "--") dec)]
                                                                           (swap! pluspluses (fn [{value plussed-nick :as pluspluses}] (assoc pluspluses plussed-nick (op (or value 0))))))
                                                                         [])
                                                                       (if (= plussed-nick nick) [plussed-nick] []))) ; (if (= plussed-nick nick) :selfplus)]))
                                            plusplus-matches))
                      responses (if (empty? nested-responses) [] (reduce into nested-responses))]
                  (do
                    (if (not (empty? responses))
                      (let [self-plusses (filter (fn [[_ sp]] (not (nil? sp))) responses)
                            invalid-plusses (filter (fn [[_ sp]] (nil? sp)) responses)
                            self-plus-reply (if (not (empty? self-plusses)) (msg from-channel (format "%s: tylko Widzew się samoplusuje. Chcesz być jak Widzew?" nick)))
                            invalid-reply (msg from-channel (format "%s: ja, mhm, i jeszcze mi powiesz że %s %s? " nick (apply word-join responses) (if (= 1 (count responses)) "istnieje"  "wszyscy istnieją")))]
                        response-map);(merge-response-map response-map { :replies { :count-pluspluses-reply invalid-reply } }))
                      response-map))))))
            response-map)))

(defn foo
  []
  (let [irc-frame (string :utf-8 :delimiters ["\r\n"])
        irc-connection @(tcp-client {:host (@config :server) :port (@config :port) :frame irc-frame})
        irc-pipeline (fn [line] (run-pipeline { }
                                            (fn [response-map]
                                              (do
                                                ;(if (not (re-find #"#inf\.aei\.polsl\.pl" line))
                                                (println "IRC SAYS:" line)
                                                (assoc response-map :line line)))
                                            (fn [response-map]
                                              (merge-response-map response-map
                                                                  (protocol-reply line)))
                                            (fn [response-map]
                                              (count-pluspluses response-map))
                                            (fn [response-map]
                                              (talkback response-map))
                                            (fn [response-map]
                                              (parrot response-map))                                            
                                            (fn [{:keys [replies] :as response-map}]
                                              (do
                                                (doseq [[from reply] replies]
                                                  (doseq [line reply] 
                                                    (println (format "I REPLY (%s): %s" from line))
                                                    (enqueue irc-connection line)))))))]
    (reset! global-irc-connection irc-connection)
    (receive-all irc-connection irc-pipeline)))

(defn global-gay-say
  [what]
  (doseq [line what]
    (enqueue @global-irc-connection line)))

(defn return
      []
      (global-gay-say (vec (flatten [(map #(format "JOIN %s" %) (@config :bot-join-channels))]))))

(def default-config {
                      :server "irc.quakenet.org"
                      :port 6667
                      :bot-nick "failmarg"
                      :bot-realname "yuuki-bot"
                      :bot-mode "+iw"
                      :bot-join-channels ["#lobos"] ;["#inf.aei.polsl.pl" "#lobos"]
                      :kick-talkbacks ["wstydziłbyś się waćpan!" "KARRRRAMBA" "terefere mi stąd!" "idź mścij się na firemarku!" "how about a nice warm cup of fuck the hell off?"]
                      :talkbacks ["no siema" "hej" "witaj!" "i ty też terefere!" "; )" "miło mi" "why, hello there!"]
                      :declensions {"ktoskowi" "ktosiek"}
                    })

(defn -main
    "I don't do a whole lot."
    [& args]
    (let
      [cwd (System/getProperty "user.dir")
       merged-config (merge default-config (json/parse-stream (clojure.java.io/reader (format "%s/data/config.json" cwd)) #(keyword %)))
       stored-pluspluses (json/parse-stream (clojure.java.io/reader (format "%s/data/pluspluses.json" cwd)))
       stored-parrot-lines (json/parse-stream (clojure.java.io/reader (format "%s/data/parrot.json" cwd)))]
      (do
        (println "Cwd:" cwd)
        (reset! config merged-config)
        (println "Hello, World!" (@config :bot-nick))
        (println "Config:")
        (clojure.pprint/pprint @config)
        (reset! pluspluses stored-pluspluses)
        (reset! kick-talkbacks (@config :kick-talkbacks))
        (reset! parrot-talkbacks stored-parrot-lines)
        (reset! talkbacks (@config :talkbacks))
        (reset! declensions (@config :declensions))
        (.addShutdownHook (Runtime/getRuntime) (Thread. (fn [] (do (println "Going down with this ship...")
                                                                   (with-open [wrtr (writer (format "%s/data/pluspluses.json" cwd))]
                                                                              (.write wrtr (json/generate-string @pluspluses)))
                                                                   (with-open [wrtr (writer (format "%s/data/parrot.json" cwd))]
                                                                              (.write wrtr (json/generate-string @parrot-talkbacks { :pretty true })))))))
        (foo))))
