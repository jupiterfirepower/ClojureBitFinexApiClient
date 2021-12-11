(ns bitfinex-api.core
  (:require [clj-http.client :as client]
          [clojure.algo.generic.functor :refer [fmap]]
          [clojure.data.json :as json]
          [clojure.string :as s]
          [buddy.core.hash :as hash]
          [buddy.core.codecs :as codecs :refer :all]
          [buddy.core.mac :as mac]
          [buddy.core.bytes :as bytes]
          [buddy.core.keys :refer :all]
          [clojure.set :as set]
          [java-time :as t]
          [clojure.spec.alpha :as spec]
          [spec-dict :refer [dict dict*]]
          [clojure.java.io :as io])
  (:gen-class))

  (declare build-parameters get-request convert-to-floats
            parseDouble)

  (def protocol "https")
  (def public-host  "api-pub.bitfinex.com")
  (def version "v2")

  (def path-symbols "symbols")
  (def path-status "platform/status")
  (def path-ticker "ticker/%s")
  (def path-tickers "tickers")
  (def path-tickers-history "tickers/hist") ;;?symbols=%s
  (def path-trades "trades/%s/hist")
  (def path-book "book/Symbol/Precision")
  (def path-stats "stats1/Key:Size:Symbol:Side/Section")
  (def path-candles "candles/trade:TimeFrame:Symbol/Section")
  (def path-configs "conf/pub:Action:Object:Detail")
  (def path-derivatives-status "status/%s")
  (def path-derivatives-status-history "status/type/symbol/hist")
  (def path-liquidations "liquidations/hist")
  (def path-leaderboards "rankings/Key:Time_Frame:Symbol/Section")
  (def path-pulse-history "pulse/hist")
  (def path-pulse-profile "pulse/profile/%s")
  (def path-funding-stats "funding/stats/%s/hist")
  (def path-market-average-price "calc/trade/avg")
  (def path-foreign-exchange-rate "calc/fx")
  (def path-today "today/%s")
  ;;(def path-stats  "stats/%s")
  (def path-lendbook "lendbook/%s")
  (def path-orderbook "book/%s")
  (def ^:const default-timeout 5000)

  ;;(def ^:const api-key "BIQxXVVncCxUHVPTvBymaxIynWekCcyGW30wtUBxhaC")
  ;;(def ^:const api-secret-key "nAuvD5vdqthfVKEFRQwwN3f8DjxTILcn53f8fRGn89z")

  (def ^:const api-key "KuOTVyCD5MkrYLXd6VtgWFCzZDDblPlUFjALK14jtdp")
  (def ^:const api-secret-key "jaFCjXjBnneN7xEkxp6CnKFKuYhwB7Yw0fzAZkzI2pd")

  (def path-wallets "auth/r/wallets")
  (def path-retrieve-orders "auth/r/orders")
  (def path-submit-order "auth/w/order/submit")
  (def path-cancel-order "auth/w/order/cancel")
  (def path-cancel-orders "auth/w/order/cancel/multi")
  (def path-orders-history "auth/r/orders/%s/hist")
  (def path-orders-history-all "auth/r/orders/hist")
  (def path-order-trades "auth/r/order/Symbol:OrderId/trades")
  (def path-ptrades "auth/r/trades/%s/hist")
  (def path-ptrades-all "auth/r/trades/hist")
  (def path-pledgers "auth/r/ledgers/%s/hist")
  (def path-pledgers-all "auth/r/ledgers/hist")
  (def path-update-order "auth/w/order/update")

  (defn get-baseurl [host]
  (format "%s://%s/%s" protocol host version))

  (defn url-for
  ([path]
   (url-for  path nil nil))
  ([path path-arg]
   (url-for  path path-arg nil))
  ([path path-arg parameters]
   (let [url (format "%s/%s" (get-baseurl public-host) path)
         with-path-arg (if path-arg
                         (format url path-arg)
                         url)]
     (if parameters
       (format "%s?%s" with-path-arg (build-parameters parameters))
       with-path-arg))))

  (defn build-parameters [parameters]
    (let [p (into {} (filter #(-> % val (not= "")) parameters))
          r (into {} (filter #(-> % val (not= nil)) p))]
     (s/join "&"
         (map (fn [[k v]] (format "%s=%s" (name k) (str v)))
            r))))

  (defn get-request
  ([url timeout]
   (json/read-json
    (:body
     (client/get url {:accept :json
                      :conn-timeout timeout}))))
  ([url]
   (get-request url default-timeout)))

  (defn post-request
  ([url timeout]
    (post-request url {} "" default-timeout))
  ([url body timeout]
    (post-request url {} body default-timeout))
  ([url headers body timeout]
    (client/post url
               { ;;:debug true
                 ;;:debug-body true
                 :headers headers
                 :body body
                 :content-type :json
                 :socket-timeout timeout      ;; in milliseconds
                 :connection-timeout timeout  ;; in milliseconds
                 :accept :json }))
  )

  (defn post-with-sign [apiPath body timeout]
      (let [ nonce (* (.toEpochMilli (t/instant)) 1000)
             ;;nonce (* (.toEpochMilli (java.time.Instant/now)) 1000)
             sigdata (format "/api/%s/%s%d%s" version apiPath nonce body)
             sig (bytes->hex (mac/hash sigdata {:key api-secret-key :alg :hmac+sha384}))
             url (format "%s/%s" (get-baseurl public-host) apiPath) ]
       (json/read-json (:body (post-request url { "Content-Type" "application/json"
                                                  "bfx-nonce" (str nonce)
                                                  "bfx-apikey" api-key
                                                  "bfx-signature" sig } body timeout)))
  ))

  (defn replace-map
  "given an input string and a hash-map, returns a new string with all
  keys in map found in input replaced with the value of the key"
  [s m]
  (clojure.string/replace s
             (re-pattern (apply str (interpose "|" (map #(java.util.regex.Pattern/quote (name %)) (keys m)))))
             (fn [key-name]
                 (java.util.regex.Matcher/quoteReplacement (str (get m
                 (keyword key-name)))))))

  (defn filter-keys [m keys-vect]
  (let [t (vec (map name keys-vect))]
      (select-keys m (concat keys-vect t))))

  (defn pfilter-keys [m keys-vect]
       (let [param (filter-keys m keys-vect)
             p (into {} (filter #(-> % val (not= "")) param))]
           (into {} (filter #(-> % val (not= nil)) p))
           ))

  (defn platform-status []
  (get-request (url-for path-status)))

  (spec/check-asserts true)
  (spec/def :string/ne-string (spec/and string? (comp not s/blank?)))

  (defn parse-long [^String number-string]
  (try (Long/parseLong number-string)
     (catch Exception e nil)))

  (defn in? [coll elm]
  (some #(= elm %) coll))

  (spec/def :long/string (spec/and :string/ne-string #(parse-long %)))
  (spec/def :int/limit (spec/and int? pos?))
  (spec/def :int/sort int?)
  (def regexp-str-start-with #"^[tf]+.*$")

  (defn ticker [^String symbol]
  {:pre [ (spec/valid? (spec/and :string/ne-string #(re-matches regexp-str-start-with %)) symbol)]}
  (get-request (url-for path-ticker symbol)))

  (spec/def :map/ntnl (spec/and (comp not nil?) map?)) ;; ntnl notnil

  (spec/def ::usert-types
    (dict {:symbols :string/ne-string}))

  (defn symbols? [^String str]
  (every? #(re-matches regexp-str-start-with %) (map s/trim (clojure.string/split str #","))))

  (defn tickers [^String symbols]
  {:pre [(spec/valid? :string/ne-string symbols)
         (or (symbols? symbols) (= "ALL" symbols))]}
    (get-request (url-for path-tickers "" { :symbols symbols })))

  ;;(spec/def :string/symbols (or #(symbols? %) #(= "ALL" %)))

  (defn max? [val n] (<= val n))
  (defn max-250? [val] (max? val 250))

  (spec/def ::userth-types
    (dict { :symbols (spec/and :string/ne-string) } ;; :string/symbols
            ^:opt { :limit (spec/and :int/limit max-250?)  }
            ^:opt { :start :long/string }
            ^:opt { :end   :long/string }))

  (defn tickers-history [mqparams]
  {:pre [(spec/valid? :map/ntnl mqparams) (spec/valid? ::userth-types mqparams)
         (or (symbols? (:symbols mqparams)) (= "ALL" (:symbols mqparams)))]}
   (let [rmqparams (filter-keys mqparams [:symbols :limit :start :end ])]
     (get-request (url-for path-tickers-history "" mqparams))))

  (defn max-10000? [val] (max? val 10000))
  (defn max-5000? [val] (max? val 5000))
  (defn max-500? [val] (max? val 500))

  (spec/def ::usernblank-types
    (dict  ^:opt { :limit :int/limit }
           ^:opt { :start :long/string }
           ^:opt { :end   :long/string }
           ^:opt { :sort  :int/sort }))

  (spec/def ::userblank-types
     (dict  ^:opt { :limit (spec/and :int/limit max-10000?) }
            ^:opt { :start :long/string }
            ^:opt { :end   :long/string }
            ^:opt { :sort  :int/sort }))

  (defn trades [^String symbol mqparams]
  {:pre [(spec/valid? (spec/and :string/ne-string #(re-matches regexp-str-start-with %)) symbol)
         (spec/valid? :map/ntnl mqparams) (spec/valid? ::userblank-types mqparams)]}
   (let [rmqparams (filter-keys mqparams [:limit :start :end :sort])]
     (get-request (url-for path-trades symbol rmqparams))))

  (def vec-lvl-price-aggregation ["P0" "P1" "P2" "P3" "P4" "R0"])

  (spec/def ::userbm-types
    (dict { :Symbol (spec/and :string/ne-string #(re-matches regexp-str-start-with %)) }
          { :Precision (spec/and :string/ne-string #(in? vec-lvl-price-aggregation %)) }))

  (def vec-len [1 25 100])

  (spec/def ::userbq-types
    (dict ^:opt { :len (spec/and int? pos? #(in? vec-len %)) }))

  (defn book [m mqparams] ;;(book { :Symbol "tBTCUSD" :Precision "P0"} { :len "1" }) https://api-pub.bitfinex.com/v2/book/Symbol/Precision
  {:pre [(spec/valid? :map/ntnl m) (spec/valid? ::userbm-types m) (spec/valid? :map/ntnl mqparams) (spec/valid? ::userbq-types mqparams)]}
   (let [rmqparams (filter-keys mqparams [:len])]
     (get-request (url-for (replace-map path-book m) "" rmqparams))))

  (def stats-key ["funding.size" "credits.size" "credits.size.sym" "pos.size" "vol.1d" "vol.7d" "vol.30d" "vwap"])
  (spec/def ::stats-key #(in? stats-key %))
  (def stats-size ["1m" "30m" "1d"])
  (spec/def ::stats-size #(in? stats-size %))
  (def stats-side ["long" "short"])
  (spec/def ::stats-side #(in? stats-side %))
  (def stats-section ["last" "hist"])
  (spec/def ::stats-section #(in? stats-section %))

  (spec/def ::usersm-types
     (dict { :Key (spec/and :string/ne-string ::stats-key) }
           { :Size (spec/and :string/ne-string ::stats-size) }
           { :Symbol (spec/and :string/ne-string #(re-matches regexp-str-start-with %)) }
           { :Side  (spec/and :string/ne-string ::stats-side) }
           { :Section (spec/and :string/ne-string ::stats-section) }))

  (defn stats [m mqparams] ;;(stats {:Key "pos.size" :Size "1m" :Symbol "tBTCUSD" :Side "long" :Section "hist"} { :limit 10 :start "" :end "" :sort -1 })
  {:pre [(spec/valid? :map/ntnl m) (spec/valid? ::usersm-types m) (spec/valid? :map/ntnl mqparams) (spec/valid? ::userblank-types mqparams)]}
    (let [ rpath (replace-map path-stats m)
           rmqparams (filter-keys mqparams [:limit :start :end :sort]) ]
      (get-request (url-for rpath "" rmqparams))))

  (def vec-time-frame ["1m", "5m", "15m", "30m", "1h", "3h", "6h", "12h", "1D", "1W", "14D", "1M"])
  (spec/def ::time-frame #(in? vec-time-frame %))

  (spec/def ::usercm-types
     (dict { :TimeFrame (spec/and :string/ne-string ::time-frame) }
           { :Symbol (spec/and :string/ne-string #(re-matches regexp-str-start-with %))}
           { :Section (spec/and :string/ne-string ::stats-section) }))

  (defn candles [m mqparams] ;;(candles {:TimeFrame "1m" :Symbol "tBTCUSD" :Section "hist"} { :limit "" :start "" :end ""  :sort 1})
  {:pre [(spec/valid? :map/ntnl m) (spec/valid? ::usercm-types m) (spec/valid? :map/ntnl mqparams) (spec/valid? ::userblank-types mqparams)]}
    (let [ rpath (replace-map path-candles m)
           rmqparams (filter-keys mqparams [:limit :start :end :sort]) ]
      (get-request (url-for rpath "" rmqparams))))

  (spec/def ::userconfm-types
    (dict { :Action :string/ne-string }
          { :Object :string/ne-string }
          { :Detail string? }
    ))

  (defn configs [m] ;;(configs {:Action "map" :Object "currency" :Detail "sym"})
  {:pre [(spec/valid? :map/ntnl m) (spec/valid? ::userconfm-types m)]}
    (let [rpath (replace-map path-configs m)]
        (get-request (url-for rpath))))

  (def regexp-str-start-with-t #"^[t]+.*$")

  (defn str-start-with-t? [^String str]
  (every? #(re-matches regexp-str-start-with-t %) (map s/trim (s/split str #","))))

  (spec/def ::userds-types
    (dict ^:opt { :keys :string/ne-string })) ;; (or #(str-start-with-t? %) #(= "ALL" %)))

  (def vec-deriv-type ["deriv"])

  (defn derivetives-status [^String type mqparams] ;;(derivetives-status "deriv" { :keys "tBTCF0:USTF0,tETHF0:USTF0" })
  {:pre [(spec/valid? :string/ne-string type) (in? vec-deriv-type type)
         (spec/valid? :map/ntnl mqparams) (spec/valid? ::userds-types mqparams)
         (or (str-start-with-t? (:keys mqparams)) (= "ALL" (:keys mqparams)))]}
  (let [rmqparams (filter-keys mqparams [:keys])]
  (get-request (url-for path-derivatives-status type rmqparams))))

  (spec/def ::userdsh-types
    (dict { :type (spec/and :string/ne-string #(in? vec-deriv-type %)) }
          { :symbol (spec/and :string/ne-string #(str-start-with-t? %)) } ))

  (spec/def ::userdshblank-types
    (dict  ^:opt { :limit (spec/and :int/limit max-5000?) }
           ^:opt { :start :long/string }
           ^:opt { :end   :long/string }
           ^:opt { :sort  :int/sort }))

  (defn derivetives-status-history [m mqparams] ;;(derivetives-status-history { :type "deriv" :symbol "tBTCF0:USTF0" } { :sort -1 :start "" :end "" :limit 10 })
  {:pre [(spec/valid? :map/ntnl m) (spec/valid? ::userdsh-types m)
         (spec/valid? :map/ntnl mqparams) (spec/valid? ::userdshblank-types mqparams)]}
    (let [ rpath (replace-map path-derivatives-status-history m)
           rmqparams (filter-keys mqparams [:limit :start :end :sort])]
        (get-request (url-for rpath "" rmqparams))))

  (spec/def ::userlic-types
    (dict  ^:opt { :limit (spec/and :int/limit max-500?) }
           ^:opt { :start :long/string }
           ^:opt { :end   :long/string }
           ^:opt { :sort  :int/sort }))

  (defn liquidations [mqparams] ;;(liquidations { :start "157057800000" :end "1573566992000" :limit 10 :sort -1 } )
  {:pre [(spec/valid? :map/ntnl mqparams) (spec/valid? ::userlic-types mqparams)]}
  (let [rmqparams (filter-keys mqparams [:limit :start :end :sort])]
  (get-request (url-for path-liquidations "" rmqparams))))

  (def vec-lb-key ["plu_diff" "plu" "vol" "plr"])
  (spec/def ::lb-key #(in? vec-lb-key %))
  (def vec-lb-time-frame ["3h", "1w", "1M"])
  (spec/def ::lb-time-frame #(in? vec-lb-time-frame %))
  (def vec-lb-section ["hist"])
  (spec/def ::lb-section #(in? vec-lb-section %))

  (spec/def ::userlbm-types
    (dict { :Key (spec/and :string/ne-string ::lb-key) }
          { :Time_Frame (spec/and :string/ne-string ::lb-time-frame) }
          { :Symbol (spec/and :string/ne-string #(str-start-with-t? %)) }
          { :Section (spec/and :string/ne-string ::lb-section)}
    ))

  (defn leaderboards [m mqparams] ;;(leaderboards { :Key "vol" :Time_Frame "3h" :Symbol "tBTCUSD" :Section "hist" } { :sort -1 :start "" :end "" :limit 10 })
  {:pre [(spec/valid? :map/ntnl m) (spec/valid? ::userlbm-types m) (spec/valid? :map/ntnl mqparams) (spec/valid? ::userblank-types mqparams)]}
    (let [ rpath (replace-map path-leaderboards m)
           rmqparams (filter-keys mqparams [:limit :start :end :sort]) ]
      (get-request (url-for rpath "" rmqparams))))

  (defn max-100? [val] (max? val 100))

  (spec/def ::userphq-types
    (dict  ^:opt { :end   :long/string }
           ^:opt { :limit (spec/and :int/limit max-100?) } ))

  (defn pulsehistory [mqparams] ;;(pulsehistory { :limit 2 :end 1593608548140})
  {:pre [(spec/valid? :map/ntnl mqparams) (spec/valid? ::userphq-types mqparams)]}
  (let [rmqparams (filter-keys mqparams [:end :limit])]
  (get-request (url-for path-pulse-history "" rmqparams))))

  (defn pulseprofile [^String nickname] ;;(pulseprofile "Bitfinex")
  {:pre [(spec/valid? :string/ne-string nickname)]}
  (get-request (url-for path-pulse-profile nickname)))

  (spec/def ::userfs-types
    (dict  ^:opt { :limit (spec/and :int/limit max-250?) }
           ^:opt { :start :long/string }
           ^:opt { :end   :long/string } ))

  (def regexp-str-start-with-f #"^[f]+.*$")

  (defn fundingstats [^String symbol mqparams] ;;(fundingstats "fUSD" { :limit "100" :start "" :end ""  :sort "-1"})
  {:pre [(spec/valid? :string/ne-string symbol) (re-matches regexp-str-start-with-f symbol) (spec/valid? :map/ntnl mqparams) (spec/valid? ::userfs-types mqparams)]}
  (let [rmqparams (filter-keys mqparams [:limit :start :end])]
  (get-request (url-for path-funding-stats symbol rmqparams))))

  (spec/def ::userma-types
    (dict  { :symbol (spec/and :string/ne-string #(re-matches regexp-str-start-with %)) }
           { :amount ::amount }
           ^:opt { :period int? }
           ^:opt { :rate_limit :string/ne-string } ))

  (defn market-average-price [mparam] ;;{:symbol "tBTCUSD" :amount "100.123"}
  {:pre [(spec/valid? :map/ntnl mparam) (spec/valid? ::userma-types mparam)]}
    (let [ url (format "%s/%s" (get-baseurl public-host) path-market-average-price)
           params (build-parameters mparam) ]
       (json/read-json (:body (post-request (format "%s?%s" url params) default-timeout)))
   ))

   (spec/def ::userfer-types
     (dict  { :ccy1 :string/ne-string }
            { :ccy2 :string/ne-string } ))

  (defn foreign-exchange-rate [mbody] ;;{ :ccy1 "BTC" :ccy2 "USD" }
  {:pre [(spec/valid? :map/ntnl mbody) (spec/valid? ::userfer-types mbody)]}
    (let [ url (format "%s/%s" (get-baseurl public-host) path-foreign-exchange-rate) ]
       (json/read-json (:body (post-request url (json/write-str mbody) default-timeout)))
   ))

  ;; Wallets
  (defn contains-all-keys? [m ks]
  (empty? (set/difference (set ks) (set (keys m)))))

  (defn post-wallets []
     (post-with-sign path-wallets "" default-timeout))

  (spec/def :gn/id (spec/and integer? #(<= % Long/MAX_VALUE) pos?))

  ;; Orders
  (defn retrive-order [^Long orderid]
   {:pre [(spec/valid? :gn/id orderid)]}
    (post-with-sign path-retrieve-orders (format "{\"id\":[%d]}" orderid) default-timeout))

  (spec/def ::mustbe-seq-of-long
      (spec/coll-of :gn/id :kind seq?))

  (defn retrive-orders
  ([^Long orderid & rest]
  {:pre [(spec/valid? :gn/id orderid) (spec/valid? ::mustbe-seq-of-long rest)]}
     (let [orders (s/join ", " (vec rest))]
       (post-with-sign path-retrieve-orders (format "{\"id\":[%d, %s]}" orderid orders) default-timeout)))
  ([^String symbol] ;;https://api.bitfinex.com/v2/auth/r/orders/:Symbol
  {:pre [(spec/valid? :string/ne-string symbol)]}
     (post-with-sign (format "%s/%s" path-retrieve-orders symbol) "" default-timeout)))

  (spec/def :string/price #(re-matches #"^\d+(?:[.]\d{2})?$" %))
  (spec/def :string/amount #(re-matches #"[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?" %)) ;; regexp for double

  (def vec-ordertype ["LIMIT", "EXCHANGE LIMIT", "MARKET",
                      "EXCHANGE MARKET", "STOP", "EXCHANGE STOP",
                      "STOP LIMIT", "EXCHANGE STOP LIMIT", "TRAILING STOP",
                      "EXCHANGE TRAILING STOP", "FOK", "EXCHANGE FOK", "IOC", "EXCHANGE IOC"])

  (spec/def :string/timeinforce #(re-matches #"^\d{4}-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01]) (2[0-3]|[01]?[0-9]):([0-5]?[0-9]):([0-5]?[0-9])$" %)) ;; YYYY-MM-DD HH:MM:SS  (ie. 2020-01-01 10:45:23)
  (spec/def ::amount (spec/and :string/ne-string :string/amount))
  (spec/def ::price (spec/and :string/ne-string :string/price))
  (spec/def ::tif (spec/and :string/ne-string :string/timeinforce))

  (defn collect-sum [v res]
    (if-let [f (first v)]
      (collect-sum (next v) (into res (map #(+ f %) (next v)) ))
      res))

  (def vec-flags (collect-sum [0 64 512 1024 4096 16384 524288 576 1088 4160] [0]))

  (spec/def ::userso-types
   (dict  { :type   (spec/and :string/ne-string #(in? vec-ordertype %))  }
          { :symbol (spec/and :string/ne-string #(s/starts-with? % "t")) }
          { :amount ::amount } ;; Amount of order (positive for buy, negative for sell)
          { :price  ::price  }
          ^:opt { :gid   :gn/id } ;; client group id
          ^:opt { :cid   :gn/id } ;; client id
          ^:opt { :flags (spec/and int? #(in? vec-flags %)) } ;; 0 -def value. table Hidden	64, Close 512, Reduce Only 1024, Post Only 4096, OCO 16384, No Var Rates 524288
          ^:opt { :lev   (spec/and int? #(spec/int-in-range? 1 101 %)) } ;; 1 .. 100 derivative order, supported by derivative symbol orders only.
          ;;The value should be between 1 and 100 inclusive. The field is optional, if omitted the default leverage value of 10 will be used.
          ^:opt { :price_trailing  ::price } ;; The trailing price for a trailing stop order
          ^:opt { :price_aux_limit ::price } ;; Auxiliary Limit price (for STOP LIMIT)
          ^:opt { :price_oco_stop  ::price } ;; OCO stop price
          ^:opt { :tif   ::tif } ;; Time-In-Force: datetime for automatic order cancellation (ie. 2020-01-01 10:45:23) )
          ^:opt { :meta  (dict {:aff_code :string/ne-string}) })) ;; optional param to pass an affiliate code

  (defn submit-order [morder]
  {:pre [(spec/valid? :map/ntnl morder) (contains-all-keys? morder [:type :symbol :price :amount]) (spec/valid? ::userso-types morder)]}
   (let [ rmorder (filter-keys morder [:gid :cid :type :symbol :price :amount :flags :lev :price_trailing :price_aux_limit :price_oco_stop :tif :meta])
          order (json/write-str rmorder)]
   (post-with-sign path-submit-order order default-timeout)))

  (def regexp-cid-date #"^\d{4}-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])$")

  (spec/def :string/cid-date #(re-matches regexp-cid-date %) )

  (spec/def ::useruo-types
    (dict  { :id     :gn/id }
           { :amount ::amount }
           { :price  ::price }
           ^:opt { :gid   :gn/id }
           ^:opt { :cid   :gn/id }
           ^:opt { :cid_date (spec/and :string/ne-string :string/cid-date) }
           ^:opt { :flags (spec/and int? #(in? vec-flags %)) }
          ;; 0 -def value. table Hidden	64, Close 512, Reduce Only 1024, Post Only 4096, OCO 16384, No Var Rates 524288
           ^:opt { :lev   (spec/and int? #(spec/int-in-range? 1 101 %)) } ;; 1 .. 100 derivative order, supported by derivative symbol orders only.
           ;;The value should be between 1 and 100 inclusive. The field is optional, if omitted the default leverage value of 1 will be used
           ^:opt { :delta ::amount } ;; Change of amount
           ^:opt { :price_trailing  ::price }
           ^:opt { :price_aux_limit ::price }
           ^:opt { :tif ::tif } ;; Time-In-Force: datetime for automatic order cancellation (ie. 2020-01-01 10:45:23)
           ))

  (defn update-order [muorder]
  {:pre [(spec/valid? :map/ntnl muorder) (contains-all-keys? muorder [:id :amount :price]) (spec/valid? ::useruo-types muorder)]}
   (let [ rmuorder (filter-keys muorder [:id :cid :cid_date :gid :amount :price :flags :lev :delta :price_aux_limit :price_trailing :tif])
          uorder (json/write-str rmuorder) ]
     (post-with-sign path-update-order uorder default-timeout)))

  (defn cancel-order
  ([^Long orderid]
  {:pre [(spec/valid? :gn/id orderid)]}
    (post-with-sign path-cancel-order (format "{\"id\":%d}" orderid) default-timeout))
  ([^Long сid ^String cid_date] ;; { cid: 12345, cid_date: "YYYY-MM-DD" } ;; ^[+-]?\d{4,5}-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])$
  {:pre [(spec/valid? :gn/id сid) (re-matches regexp-cid-date cid_date)]}
    (post-with-sign path-cancel-order (format "{\"cid\":%d,\"cid_date\":\"%s\"}" сid cid_date) default-timeout)))

  (def select-values (comp vals select-keys))

  (defn cid_dates? [m]
  (every? #(re-matches regexp-cid-date %) (select-values m (keys m))))

  (defn cids_id? [m]
  (every? (comp integer? #(<= % Long/MAX_VALUE) pos?) (keys m)))

  (defn cancel-orders
  ([] ;; cancel all orders
    (post-with-sign path-cancel-orders "{\"all\" : 1}"  default-timeout))
  ([^Long orderid & rest]
  {:pre [(spec/valid? :gn/id orderid) (spec/valid? ::mustbe-seq-of-long rest)]}
    (let [orders (s/join "," (vec rest))]
    (post-with-sign path-cancel-orders (format "{\"id\":[%d, %s]}" orderid orders) default-timeout)))
  ([mсid] ;; mсid -> { 123 "1999-09-24" 124 "1999-09-25" } { cid: 12345, cid_date: "YYYY-MM-DD" }
  {:pre [(spec/valid? :map/ntnl mсid)]}
  (spec/assert cids_id? mсid)
  (spec/assert cid_dates? mсid)
    (let [ciddata (s/join "," (map (fn [[k v]] (format "[%d,\"%s\"]" k v)) mсid))]
    (post-with-sign path-cancel-orders (format "{\"cid\": [%s]}" ciddata) default-timeout))))

  (defn cancel-orders-by-gid
  ([^Long gid]
  {:pre [(spec/valid? :gn/id gid)]}
    (post-with-sign path-cancel-orders (format "{\"gid\":[%d]}" gid) default-timeout))
  ([^Long gid & rest]
  {:pre [(spec/valid? :gn/id gid) (spec/valid? ::mustbe-seq-of-long rest)]}
    (let [gids (s/join "," (vec rest))]
       (post-with-sign path-cancel-orders (format "{\"gid\":[%d, %s]}" gid gids) default-timeout))))

  (spec/def ::useroh-types
  (dict  ^:opt { :start   :long/string }
         ^:opt { :end     :long/string }
         ^:opt { :limit   :int/limit }
         ^:opt { :id      (spec/coll-of :gn/id :kind vector? :distinct true) } ))

  (defn orders-history
  ([^String symbol mqparams]
  {:pre [(spec/valid? :string/ne-string symbol)
         (spec/valid? :map/ntnl mqparams) (spec/valid? ::useroh-types mqparams)]}
   (let [ path (format path-orders-history symbol)
          qdata (json/write-str mqparams) ]
      (post-with-sign path qdata default-timeout)))
  ([mqparams]
  {:pre [(spec/valid? :map/ntnl mqparams) (spec/valid? ::useroh-types mqparams)]}
   (let [qdata (json/write-str mqparams)]
      (post-with-sign path-orders-history-all qdata default-timeout))))

  (spec/def ::userot-types
    (dict { :Symbol   (spec/and :string/ne-string #(s/starts-with? % "t")) }
          { :OrderId  :gn/id } ))

  (defn order-trades
  [mp] ;; mp -> { :Symbol "tBTCUSD" :OrderId 12345 } https://api.bitfinex.com/v2/auth/r/order/Symbol:OrderId/trades
  {:pre [(spec/valid? :map/ntnl mp) (spec/valid? ::userot-types mp)]}
   (let [ path (replace-map path-order-trades mp) ]
     (post-with-sign path "" default-timeout)))

  (defn ptrades
  ([^String symbol mqparams]
  {:pre [(spec/valid? :string/ne-string symbol) (spec/valid? :map/ntnl mqparams) (spec/valid? ::userblank-types mqparams)]}
   (let [ path (format path-ptrades symbol)
          rmqparams (pfilter-keys mqparams [:start :end :limit :sort])
          qdata (json/write-str rmqparams) ]
     (post-with-sign path qdata default-timeout)))
  ([mqparams]
  {:pre [(spec/valid? :map/ntnl mqparams) (spec/valid? ::userblank-types mqparams)]}
   (let [ rmqparams (pfilter-keys mqparams [:start :end :limit :sort])
          qdata (json/write-str rmqparams)]
     (post-with-sign path-ptrades-all qdata default-timeout))))

  (def vec-categories [5 22 23 25 26 27 28 29 31 51 101 104 105 201 202 204 207 222 224 226 228 241 243 251 254 255 258 262 401 501 905 907 911])

  (spec/def ::userledg-types
    (dict ^:opt { :start    :long/string }
          ^:opt { :end      :long/string }
          ^:opt { :limit    :int/limit }
          ^:opt { :category (spec/and int? #(in? vec-categories %)) } ))

  (defn pledgers
  ([^String currency mqparams]
  {:pre [(spec/valid? :string/ne-string currency)
         (spec/valid? :map/ntnl mqparams) (spec/valid? ::userledg-types mqparams)]}
   (let [ path (format path-pledgers currency)
          rmqparams (pfilter-keys mqparams [:category :start :end :limit])
          qdata (json/write-str rmqparams) ]
     (post-with-sign path qdata default-timeout)))
  ([mqparams]
  {:pre [(spec/valid? :map/ntnl mqparams) (spec/valid? ::userledg-types mqparams)]}
   (let [ rmqparams (pfilter-keys mqparams [:category :start :end :limit])
          qdata (json/write-str rmqparams)]
     (post-with-sign path-pledgers-all qdata default-timeout))))

  (def path-margin-info "auth/r/info/margin/%s")
  (def path-retrieve-position "auth/r/positions")
  (def path-claim-position "auth/w/position/claim")
  (def path-increase-position "auth/w/position/increase")
  (def path-increase-position-info "auth/r/position/increase/info")
  (def path-position-history "auth/r/positions/hist")
  (def path-position-snapshot "auth/r/positions/snap")
  (def path-position-audit "auth/r/positions/audit")
  (def path-deriv-pos-collateral "auth/w/deriv/collateral/set")
  (def path-derivposcol-limits "auth/calc/deriv/collateral/limits")

  (defmacro current-function-name []
  "Returns a string, the name of the current Clojure function"
  `(-> (Throwable.) .getStackTrace first .getClassName unmangle))

  (def vec-minfo-key ["base" "sym_all"])

  ;; Margin
  (defn post-margin-info [^String key] ;; "base" | SYMBOL tBTCUSD | sym_all
  {:pre [ (spec/valid? :string/ne-string key)
          (or (in? vec-minfo-key key) (s/starts-with? key "t")) ]}
    (post-with-sign (format path-margin-info key) (json/write-str {}) default-timeout))

  (defn post-retrieve-position []
      (post-with-sign path-retrieve-position  (json/write-str {})  default-timeout))

  (spec/def ::usercp-types
      (dict { :id    :gn/id }
            ^:opt { :amont ::amount } ))

  (defn post-claim-position [mbody] ;; { id*: 1234 :amount "11" }
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::usercp-types mbody) ]}
     (let [ fmbody (pfilter-keys mbody [:id :amount]) ]
       (post-with-sign path-increase-position (json/write-str fmbody) default-timeout)))

  (spec/def ::userip-types
       (dict { :symbol (spec/and :string/ne-string #(s/starts-with? % "t")) }
             { :amont ::amount } ))

  (defn post-increase-position [mbody] ;; { symbol*: "tBTCUSD" :amount* "120" }
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::userip-types mbody) ]}
     (let [ fmbody (pfilter-keys mbody [:symbol :amount]) ]
       (post-with-sign path-increase-position (json/write-str fmbody) default-timeout)))

  (spec/def ::useripf-types
    (dict { :symbol (spec/and :string/ne-string #(s/starts-with? % "t")) }
          ^:opt { :amont ::amount } ))

  (defn post-increase-position-info [mbody] ;; { symbol*: "tBTCUSD" :amount "120" }
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::useripf-types mbody) ]}
     (let [ fmbody (pfilter-keys mbody [:symbol :amount]) ]
       (post-with-sign path-increase-position-info (json/write-str fmbody) default-timeout)))

  (defn max-50? [val] (max? val 50))

  (spec/def ::userph-types
     (dict ^:opt { :start :gn/id }
           ^:opt { :end   :gn/id }
           ^:opt { :limit (spec/and :int/limit max-50? ) }
  ))

  (defn post-position-history [mbody] ;; { start: 1544555024000 :end 1544555054000 :limit 10 } -> optional
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::userph-types mbody) ]}
     (let [ fmbody (pfilter-keys mbody [:start :end :limit]) ]
       (post-with-sign path-position-history (json/write-str fmbody) default-timeout)))

  (spec/def ::userps-types
     (dict ^:opt { :start    :gn/id }
           ^:opt { :end      :gn/id }
           ^:opt { :limit (spec/and :int/limit max-500? ) }
  ))

  (defn post-position-snapshot [mbody] ;; { start: 1544555024000 :end 1544555054000 :limit 2 } -> optional(Max 500)
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::userps-types mbody) ]}
     (let [ fmbody (pfilter-keys mbody [:start :end :limit]) ]
       (post-with-sign path-position-snapshot (json/write-str fmbody) default-timeout)))

  (spec/def ::userpa-types
    (dict { :id (spec/coll-of :gn/id :kind vector?) }
          ^:opt { :start    :gn/id }
          ^:opt { :end      :gn/id }
          ^:opt { :limit (spec/and :int/limit max-250? ) } ))

  (defn post-position-audit [mbody] ;; { :id* [123 1234 12345] :start 1544555024000 :end 1544555054000 :limit 2 }
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::userpa-types mbody) ]}
     (let [ fmbody (pfilter-keys mbody [:id :start :end :limit]) ]
      (post-with-sign path-position-audit (json/write-str fmbody) default-timeout)))

  (spec/def ::userdpc-types
     (dict { :symbol (spec/and :string/ne-string #(s/starts-with? % "t")) }
           { :collateral ::amount } ))

  (defn post-deriv-pos-collateral [mbody] ;; { :symbol "tBTCF0:USTF0" :collateral 1150.6 } collateral->float
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::userdpc-types mbody) ]}
     (let [ fmbody (pfilter-keys mbody [:symbol :collateral]) ]
        (post-with-sign path-deriv-pos-collateral (json/write-str fmbody) default-timeout)))

  (spec/def ::userdpcl-types
     (dict { :symbol (spec/and :string/ne-string #(s/starts-with? % "t")) } ))

  (defn post-deriv-pos-coll-limits [mbody] ;; { :symbol "tBTCF0:USTF0" }
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::userdpcl-types mbody) ]}
     (let [ fmbody (pfilter-keys mbody [:symbol]) ]
        (post-with-sign path-derivposcol-limits (json/write-str fmbody) default-timeout)))

  (def path-active-funding-offers "auth/r/funding/offers")
  (def path-submit-funding-offer "auth/w/funding/offer/submit")
  (def path-cancel-funding-offer "auth/w/funding/offer/cancel")
  (def path-cancel-all-funding-offers "auth/w/funding/offer/cancel/all")
  (def path-funding-close "auth/w/funding/close")
  (def path-funding-auto-renew "auth/w/funding/auto")
  (def path-keep-funding "auth/w/funding/keep")
  (def path-funding-offers-history "auth/r/funding/offers/%s/hist")
  (def path-funding-offers-history-all "auth/r/funding/offers/hist")
  (def path-funding-loans "auth/r/funding/loans/%s")
  (def path-funding-loans-all "auth/r/funding/loans")
  (def path-funding-loans-history "auth/r/funding/loans/%s/hist")
  (def path-funding-loans-history-all "auth/r/funding/loans/hist")
  (def path-funding-credits "auth/r/funding/credits/%s")
  (def path-funding-credits-all "auth/r/funding/credits")
  (def path-funding-credits-history "auth/r/funding/credits/%s/hist")
  (def path-funding-credits-history-all "auth/r/funding/credits/hist")
  (def path-funding-trades "auth/r/funding/trades/%s/hist")
  (def path-funding-trades-all "auth/r/funding/trades/hist")
  (def path-funding-info "uth/r/info/funding/%s")

  (defn post-active-funding-offers
  ([]
     (post-with-sign path-active-funding-offers "" default-timeout))
  ([^String symbol] ;; symbol fUSD
  {:pre [ (spec/valid? :string/ne-string symbol) (s/starts-with? symbol "f") ]}
     (post-with-sign (format "%s/%s" path-active-funding-offers symbol) "" default-timeout)))

  (def vec-order-type ["LIMIT" "FRRDELTAVAR" "FRRDELTAFIX"])
  (spec/def ::rate (spec/and :string/ne-string :string/amount))
  (defn period? [val] (or (>= val 2) (<= val 120)))

  (spec/def ::usersfo-types
    (dict { :type (spec/and :string/ne-string #(in? vec-order-type %)) }
    ;; { :type* "LIMIT" :symbol* "fUSD" :amount* "123.45" :rate* "0.001" :period* 2 :flags 0 }
    ;;Order Type (LIMIT, FRRDELTAVAR, FRRDELTAFIX)
          { :symbol (spec/and :string/ne-string #(s/starts-with? % "f")) }
          { :amount ::amount }
          { :rate   ::rate }
          { :period period? }
          ^:opt { :flags (spec/and int? #(in? vec-flags %)) } ))
          ;; 0 -def value. table Hidden	64, Close 512, Reduce Only 1024, Post Only 4096, OCO 16384, No Var Rates 524288 ))

  (defn post-submit-funding-offer [mbody]
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::usersfo-types mbody) ]}
    (let [ fmbody (pfilter-keys mbody [:type :symbol :amount :rate :period :flags]) ]
      (post-with-sign path-submit-funding-offer (json/write-str fmbody) default-timeout)))

  (spec/def :offer/id (spec/and int? pos?))

  (defn post-cancel-funding-offer [^long id]
    {:pre [(spec/valid? :offer/id id)]}
    (post-with-sign path-cancel-funding-offer (json/write-str { :id id }) default-timeout))

  (defn post-cancel-all-funding-offers
  ([]
     (post-with-sign path-cancel-all-funding-offers "" default-timeout))
  ([^String currency] ;; currency USD etc
    {:pre [ (spec/valid? :string/ne-string currency) ]}
     (post-with-sign path-cancel-all-funding-offers (json/write-str { :currency currency }) default-timeout)))

  (defn post-path-funding-close [^long id] ;;Offer ID (retrievable via the Funding Loans and Funding Credits endpoints)
    {:pre [(spec/valid? :offer/id id)]}
    (post-with-sign path-funding-close (json/write-str { :id id }) default-timeout))

  (def vec-status [1 0])

  (spec/def ::userfar-types
    (dict { :status #(in? vec-status %) }
          { :currency :string/ne-string }
          ^:opt { :amount ::amount }
          ^:opt { :rate   ::rate }
          ^:opt { :period period? } ))

  (defn post-funding-auto-renew [mbody]
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::userfar-types mbody) ]}
    (let [ fmbody (pfilter-keys mbody [:status :currency :amount :rate :period]) ] ;;status 1 to activate, 0 to deactivate
       (post-with-sign path-funding-auto-renew (json/write-str fmbody) default-timeout)))

  (def vec-funding-type ["credit" "loan"])

  (defn long-str? [str] (parse-long str))

  (defn changes-key-id? [m]
    (every? #(long-str? %) (keys m)))

  (def changes-valid-values [0 1])

  (defn changes-val-valid? [m]
    (every? #(in? changes-valid-values %) (select-values m (keys m))))

  (spec/def ::userkf-types
    (dict { :type #(in? vec-funding-type %) }
          ^:opt { :id (spec/coll-of :gn/id :kind vector? :distinct true) }
          ^:opt { :changes (spec/and map? changes-key-id? changes-val-valid?) } ))

  (defn post-keep-funding [mbody] ;;{"type":"loan","id":[123,124,125] } ;;{"type":"loan","id":[123,124,125],"changes":{"123":1,"124":1,"125":1}}
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::userkf-types mbody) ]}
    (let [ fmbody (pfilter-keys mbody [:type :id :changes]) ]
       (post-with-sign path-keep-funding (json/write-str fmbody) default-timeout)))

  (spec/def ::userafoh-types
     (dict ^:opt { :start    :gn/id }
           ^:opt { :end      :gn/id }
           ^:opt { :limit (spec/and :int/limit max-500? ) } ))

  (defn post-active-funding-offers-history
  ([mbody]
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::userafoh-types mbody) ]}
    (let [ fmbody (pfilter-keys mbody [:start :end :limit]) ]
       (post-with-sign path-funding-offers-history-all (json/write-str fmbody) default-timeout)))
  ([^String symbol mbody] ;; symbol fUSD
  {:pre [ (spec/valid? :string/ne-string) (s/starts-with? symbol "f")
          (spec/valid? :map/ntnl mbody) (spec/valid? ::userafoh-types mbody) ]}
     (let [ fmbody (pfilter-keys mbody [:start :end :limit]) ]
        (post-with-sign (format path-funding-offers-history symbol) (json/write-str fmbody) default-timeout))))

  (defn post-funding-loans
  ([]
     (post-with-sign (format path-funding-loans-all symbol) "" default-timeout))
  ([^String symbol] ;; symbol fUSD
  {:pre [ (spec/valid? :string/ne-string) (s/starts-with? symbol "f") ]}
     (post-with-sign (format path-funding-loans symbol) "" default-timeout)))


  (defn post-funding-loans-history
  ([mbody]
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::userafoh-types mbody) ]}
    (let [ fmbody (pfilter-keys mbody [:start :end :limit]) ]
      (post-with-sign path-funding-loans-history-all (json/write-str fmbody) default-timeout)))
  ([^String symbol mbody] ;; symbol fUSD
  {:pre [ (spec/valid? :string/ne-string symbol) (s/starts-with? symbol "f")
          (spec/valid? :map/ntnl mbody) (spec/valid? ::userafoh-types mbody) ]}
    (let [ fmbody (pfilter-keys mbody [:start :end :limit]) ]
      (post-with-sign (format path-funding-loans-history symbol) (json/write-str fmbody) default-timeout))))

  (defn post-funding-credits
  ([]
      (post-with-sign path-funding-credits-all "" default-timeout))
  ([^String symbol] ;; symbol fUSD
  {:pre [ (spec/valid? :string/ne-string symbol) (s/starts-with? symbol "f") ]}
      (post-with-sign (format path-funding-credits symbol) "" default-timeout)))

  (defn post-funding-credits-history
  ([mbody]
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::userafoh-types mbody) ]}
    (let [ fmbody (pfilter-keys mbody [:start :end :limit]) ]
      (post-with-sign path-funding-credits-history-all (json/write-str fmbody) default-timeout)))
  ([^String symbol mbody] ;; symbol fUSD
  {:pre [ (spec/valid? :string/ne-string symbol) (s/starts-with? symbol "f")
          (spec/valid? :map/ntnl mbody) (spec/valid? ::userafoh-types mbody) ]}
    (let [ fmbody (pfilter-keys mbody [:start :end :limit]) ]
      (post-with-sign (format path-funding-credits-history symbol) (json/write-str fmbody) default-timeout))))

  (defn post-funding-trades
  ([mbody]
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::userafoh-types mbody) ]}
    (let [ fmbody (pfilter-keys mbody [:start :end :limit]) ]
      (post-with-sign path-funding-trades-all (json/write-str fmbody) default-timeout)))
  ([^String symbol mbody] ;; symbol fUSD
  {:pre [ (spec/valid? :string/ne-string symbol) (s/starts-with? symbol "f")
          (spec/valid? :map/ntnl mbody) (spec/valid? ::userafoh-types mbody) ]}
    (let [ fmbody (pfilter-keys mbody [:start :end :limit]) ]
      (post-with-sign (format path-funding-trades symbol) (json/write-str fmbody) default-timeout))))

  (defn post-funding-info [^String symbol] ;; symbol fUSD
  {:pre [ (spec/valid? :string/ne-string symbol) (s/starts-with? symbol "f") ]}
      (post-with-sign (format path-funding-info symbol) "" default-timeout))

  ;; Account actions

  (def path-user-info "auth/r/info/user")
  (def path-summary "auth/r/summary")
  (def path-login-history "auth/r/logins/hist")
  (def path-key-permissions "auth/r/permissions")
  (def path-generate-token "auth/w/token")
  (def path-account-changelog "auth/r/audit/hist")
  (def path-transfer-between-wallets "auth/w/transfer")
  (def path-deposit-address "auth/w/deposit/address")
  (def path-generate-invoice "auth/w/deposit/invoice")
  (def path-withdrawal "auth/w/withdraw")
  (def path-movements "auth/r/movements/%s/hist")
  (def path-movements-all "auth/r/movements/hist")
  (def path-alert-list "auth/r/alerts")
  (def path-alert-set "auth/w/alert/set")
  (def path-alert-delete "auth/w/alert/price:%s:%s/del")
  (def path-balance-orders-offers "auth/calc/order/avail")
  (def path-user-settings-write "auth/w/settings/set")
  (def path-user-settings-read "auth/r/settings")
  (def path-user-settings-delete "auth/w/settings/del")
  (def path-post-pulse-history "auth/r/pulse/hist")
  (def path-pulse-write "auth/w/pulse/add")
  (def path-pulse-delete "auth/w/pulse/del")


  (defn post-user-info []
     (post-with-sign path-user-info "" default-timeout))

  (defn post-summary []
     (post-with-sign path-summary "" default-timeout))

  (spec/def ::userlh-types
     (dict ^:opt { :start    :gn/id }
           ^:opt { :end      :gn/id }
           ^:opt { :limit (spec/and :int/limit max-250? ) } ))

  (defn post-login-history [mbody]
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::userlh-types mbody) ]}
     (let [ fmbody (pfilter-keys mbody [:start :end :limit]) ]
        (post-with-sign path-login-history (json/write-str fmbody) default-timeout)))

  (defn post-key-permissions []
     (post-with-sign path-key-permissions "" default-timeout))

  (def vec-scope ["api"])
  (def vec-caps  ["a" "o" "f" "s" "w" "wd"])
  (def regexp-IPv4 #"^(?:[0-9]{1,3}\.){3}[0-9]{1,3}$")
  (def regexp-IPv6 #"^(?:[0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}$")
  (defn str-ip-address? [str] (or (re-matches regexp-IPv4 str) (re-matches regexp-IPv6 str)))

  (spec/def ::usergt-types
     (dict { :scope (spec/and :string/ne-string #(in? vec-scope %)) }
           ^:opt { :ttl (spec/and int? pos?) }
           ^:opt { :caps (spec/coll-of (spec/and :string/ne-string #(in? vec-caps %)) :kind vector? :distinct true) }
           ^:opt { :writePermission boolean? }
           ^:opt { :_cust_ip #(str-ip-address? %) }
  ))

  (defn post-generate-token [mbody]
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::usergt-types mbody) ]}
     (let [ fmbody (pfilter-keys mbody [:scope :ttl :caps :writePermission :_cust_ip]) ]
       (post-with-sign path-generate-token (json/write-str fmbody) default-timeout)))

  (spec/def ::userac-types
     (dict ^:opt { :start    :gn/id }
           ^:opt { :end      :gn/id }
           ^:opt { :limit (spec/and :int/limit max-500? ) } ))

  (defn post-account-changelog [mbody]
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::userac-types mbody) ]}
     (let [ fmbody (pfilter-keys mbody [:start :end :limit]) ]
       (post-with-sign path-account-changelog (json/write-str fmbody) default-timeout)))

  (def vec-from ["exchange" "margin" "funding" "trading" "deposit"])

  (def regexp-email #"[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?")

  (defn str-email? [^String email]
     (re-matches regexp-email email))

  (spec/def ::usertbw-types
     (dict { :from (spec/and :string/ne-string #(in? vec-from %)) }
           { :to (spec/and :string/ne-string #(in? vec-from %)) }
           { :currency :string/ne-string }
           ^:opt { :currency_to :string/ne-string }
           { :amount ::amount }
           ^:opt { :email_dst (spec/and :string/ne-string str-email?) }
  ))

  (defn post-transfer-between-wallets [mbody]
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::usertbw-types mbody) ]}
     (let [ fmbody (pfilter-keys mbody [:from :to :currency :currency_to :amount :email_dst]) ]
       (post-with-sign path-transfer-between-wallets (json/write-str fmbody) default-timeout)))

  (def vec-op_renew [0 1])

  (spec/def ::userda-types
     (dict { :wallet (spec/and :string/ne-string #(in? vec-from %)) }
           { :method :string/ne-string }
           ^:opt { :op_renew (spec/and int? pos? #(in? vec-op_renew)) }
  ))

  (defn post-deposit-address [mbody]
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::userda-types mbody) ]}
     (let [ fmbody (pfilter-keys mbody [:wallet :method :op_renew]) ]
       (post-with-sign path-deposit-address (json/write-str fmbody) default-timeout)))

  (spec/def ::userw-types
    (dict { :wallet (spec/and :string/ne-string #(in? vec-from %)) }
          { :method :string/ne-string } ;;https://api-pub.bitfinex.com//v2/conf/pub:map:tx:method [[[METHOD,[CURRENCY]]...]] *
          { :amount ::amont }
          { :address :string/ne-string }
          ^:opt { :payment_id :string/ne-string } ;; Specify a tag/memo/etc.
  ))

  (def data [["BITCOIN" "BTC"],["LITECOIN" "LTC"],["ETHEREUM" "ETH"],
             ["ETHEREUMC" "ETC"], ["TETHERUSO" "UST"], ["ZCASH" "ZEC"],
             ["MONERO" "XMR"], ["IOTA" "IOT"], ["RIPPLE" "XRP"],
             ["DASH" "DSH"],["EOS" "EOS"], ["SANTIMENT" "SAN"],
             ["OMISEGO" "OMG"], ["NEO" "NEO"], ["METAVERSE" "ETP"],
             ["QTUM" "QTM"], ["EIDOO" "EDO"], ["DATACOIN" "DAT"],
             ["TETHERUSE" "UST"], ["BGOLD" "BTG"], ["QASH" "QSH"],
             ["GOLEM" "GNT"], ["STATUS" "SNT"], ["TETHEREUE" "EUT"],
             ["BAT" "BAT"],["MNA" "MNA"], ["FUN" "FUN"], ["ZRX" "ZRX"],
             ["TRX" "TRX"], ["SNG" "SNG"], ["REP" "REP"],["NEC" "NEC"],
             ["REQ" "REQ"], ["LRC" "LRC"], ["WAX" "WAX"],["DAI" "DAI"],
             ["BFT" "BFT"],["ODE" "ODE"], ["ANT" "ANT"], ["STJ" "STJ"],
             ["XLM" "XLM"], ["XVG" "XVG"], ["MKR" "MKR"], ["KNC" "KNC"],
             ["LYM" "LYM"], ["UTK" "UTK"], ["VEE" "VEE"], ["ORS" "ORS"],
             ["ZCN" "ZCN"], ["ESS" "ESS"], ["ATD" "ATD"], ["ADD" "ADD"],
             ["MTO" "MTO"], ["IQX" "IQX"], ["ZIL" "ZIL"], ["BNT" "BT"],
             ["XRA" "XRA"], ["VET" "VET"], ["GOT" "GOT"], ["XTZ" "XTZ"],
             ["YGG" "YGG"], ["MLN" "MLN"], ["OMN" "OMN"], ["PNK" "PNK"],
             ["DGB" "DGB"], ["BS" "BSV"], ["ENJ" "ENJ"], ["RBT" "RBT"],
             ["UDC" "UDC"], ["TSD" "TSD"], ["PAX" "PAX"], ["PAS" "PAS"],
             ["VSY" "VSY"], ["BTT" "BTT"], ["CLO" "CLO"], ["GNO" "GNO"],
             ["ATO" "ATO"], ["WBT" "WBT"], ["XCH" "XCH"], ["EUS" "EUS"],
             ["LBT" "LBT"], ["LET" "LET"], ["LES" "LES"], ["GTX" "GTX"],
             ["KAN" "KAN"], ["AMP" "AMP"], ["ALG" "ALG"], ["DUSK" "DUSK"],
             ["UOS" "UOS"], ["TETHERUSX" "UST"], ["TETHERUSL" "UST"],["RRB" "RRB"],
             ["TETHERUSS" "UST"], ["FTT" "FTT"], ["TETHERCHTE" "CNHT"],
             ["CHZ" "CHZ"], ["LNX" "LNX"], ["TETHERXAUTE" "XAUT"],
             ["TETHERUSDTALG" "ST"], ["TETHERUSDTBCH" "UST"],["RINGX" "RINGX"],
             ["BTSE" "BTSE"], ["DOG" "DOG"], ["DOT" "DOT"], ["ADA" "ADA"],
             ["FET" "FET"],["TETHERUSDTOMG" "UST"], ["LINK" "LINK"],
             ["COMP" "COMP"], ["KSM" "KSM"], ["EGLD" "EGLD"], ["UNI" "UNI"],
             ["BAND" "BAND"], ["AVAX" "AVAX"], ["SNX" "SNX"], ["YFI" "YFI"],
             ["BAL" "BAL"], ["PBTCEOS" "PBTCEOS"],["EOSDT" "EOSDT"],
             ["PLTCEOS" "PLTCEOS"], ["PETHEOS" "PETHEOS"], ["FIL" "FIL"],
             ["PBTCETH" "PBTCETH"], ["PEOSETH" "PEOSETH"], ["PLTCETH" "PLTCETH"],
             ["PPNTEOS" "PPNTEOS"], ["PLINKEOS" "PLINKEOS"], ["PMKREOS" "PMKREOS"],
             ["PYFIEOS" "PYFIEOS"], ["JST" "JST"], ["HEZ" "HEZ"], ["BCHABC" "BCHABC"],
             ["BCHN" "BCHN"], ["XDC" "XDC"], ["PLU" "PLU"], ["SUN" "SUN"],
             ["PUNIEOS" "PUNIEOS"], ["PBANDEOS" "PBANDEOS"], ["PBALEOS" "PBALEOS"],
             ["PCOMPEOS" "PCOMPEOS"], ["PSNXEOS" "PSNXEOS"], ["UOP" "UOP"],
             ["POMGEOS" "POMGEOS"], ["PANTEOS" "PANTEOS"], ["PDAIEOS" "PDAIEOS"],
             ["PLRCEOS" "PLRCEOS"], ["B21X" "B21X"], ["PUOSEOS" "PUOSEOS"], ["PBATEOS" "PBATEOS"],
             ["PREPEOS" "PREPEOS"], ["PZRXEOS" "PZRXEOS"],["PPNKEOS" "PPNKEOS"],
             ["SUSHI" "SUSHI"], ["XSN" "XSN"], ["EXRD" "EXRD"], ["AAVE" "AAVE"],
             ["CTK" "CTK"], ["SOL" "SOL"], ["BEST" "BEST"], ["TETHERUSDTSOL" "UST"],
             ["ALBT" "ALBT"], ["CEL" "CEL"], ["SUKU" "SUKU"], ["BMI" "BMI"], ["MOB" "MOB"],
             ["NEAR" "NEAR"], ["BOSON" "BOSON"], ["LUNA" "LUNA"],
             ["TETHERUSDTHEZ" "UST"], ["ICE" "ICE"], ["DOGE" "DOGE"],
             ["OXY" "OXY"],["1INCH" "1INCH"], ["IDX" "IDX"], ["FORTH" "FORTH"],
             ["CHEX" "CHEX"], ["QTF" "QTF"], ["OCEAN" "OCEAN"], ["PLANETS" "PLANETS"],
             ["FTM" "FTM"], ["NEXO" "NEXO"], ["VELO" "VELO"], ["ICP" "ICP"],
             ["FCL" "FCL"], ["TERRAUST" "TERRAUST"], ["MIR" "MIR"], ["GRT" "GRT"], ["WAVES" "WAVES"],
             ["REEF" "REEF"], ["DCR" "DCR"], ["CHSB" "CH SB"], ["XRD" "XRD"], ["EXO" "EXO"],
             ["ROSE" "ROSE"], ["MATIC" "MATIC"], ["AXS" "AXS"], ["HMT" "HMT"],
             ["TETHERUSDTDVF" "UST"], ["TETHERUSDTDVF" "USDTDVF"], ["DORA" "DORA"], ["JASMY" "JASMY"],
             ["ANC" "ANC"], ["WIRE" "USD"], ["WIRE" "GBP"], ["WIRE" "HKD"], ["WIRE" "EUR"],
             ["WIRE" "SGD"], ["WIRE" "TWD"], ["WIRE" "CHF"], ["WIRE" "JPY"], ["WIRE" "CNH"]
  ])

  (defn get-method-by-currency [^String currency]
        (get (first (filter #(some #{currency} %) data)) 0))

  (defn post-withdrawal [mbody]
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::userw-types mbody) ]}
     (let [ fmbody (pfilter-keys mbody [:wallet :method :amount :address :payment_id]) ]
       (post-with-sign path-withdrawal (json/write-str fmbody) default-timeout)))

  (defn max-1000? [val] (max? val 1000))

  (spec/def ::userm-types
    (dict ^:opt { :start    :gn/id }
          ^:opt { :end      :gn/id }
          ^:opt { :limit (spec/and :int/limit max-1000? ) } ))

  (defn post-movements
  ([^String currency mbody]
  {:pre [ (spec/valid? :string/ne-string currency)
          (spec/valid? :map/ntnl mbody) (spec/valid? ::userm-types mbody) ]}
     (let [ fmbody (pfilter-keys mbody [:start :end :limit]) ]
       (post-with-sign (format path-movements currency) (json/write-str fmbody) default-timeout)))
  ([mbody]
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::userm-types mbody) ]}
     (let [ fmbody (pfilter-keys mbody [:start :end :limit]) ]
       (post-with-sign path-movements-all (json/write-str fmbody) default-timeout))))

  (def vec-alert-type ["price"])

  (spec/def ::useral-types
     (dict { :type (spec/and :string/ne-string #(in? vec-alert-type %))} ))

  (defn post-alert-list [mbody]
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::useral-types mbody) ]}
     (let [ fmbody (pfilter-keys mbody [:type]) ]
       (post-with-sign path-alert-list (json/write-str fmbody) default-timeout)))

  (spec/def ::userals-types
     (dict { :type (spec/and :string/ne-string #(in? vec-alert-type %)) }
           { :symbol (spec/and :string/ne-string (s/starts-with? symbol "t")) }
           { :price float? } ))

  (defn post-alert-set [mbody]
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::userals-types mbody) ]}
     (let [ fmbody (pfilter-keys mbody [:type :symbol :price]) ]
       (post-with-sign path-alert-set (json/write-str fmbody) default-timeout)))

  (defn post-alert-delete [^String symbol ^Float price]
  {:pre [ (spec/valid? :string/ne-string symbol) (float? price) ]}
     (post-with-sign (format path-alert-delete symbol price) "" default-timeout))

  (def vec-dir [1 -1]) ;; Direction of the order (1 for by, -1 for sell) (Mandator for EXCHANGE and MARGIN type, not used for FUNDING)
  (def vec-type-ordoff ["EXCHANGE" "MARGIN" "FUNDING"])

  (spec/def ::userboo-types
    (dict { :symbol (spec/and :string/ne-string #(re-matches regexp-str-start-with %)) }
          ^:opt { :dir #(in? vec-dir %) }
          ^:opt { :rate ::pcice }
          { :type #(in? vec-type-ordoff %) }
          ^:opt { :lev ::amount }
  ))

  (defn post-balance-orders-offers [mbody]
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::userboo-types mbody) ]}
     (let [ fmbody (pfilter-keys mbody [:symbol :dir :rate :type :lev]) ]
       (post-with-sign path-balance-orders-offers (json/write-str fmbody) default-timeout)))

  (def regexp-apikey #"^api:[A-Za-z0-9_-]*$")

  (defn settings-key-apikey? [m]
     (every? #(re-matches regexp-apikey %) (keys m)))

  (spec/def ::userusw-types
     (dict { :settings (spec/and map? settings-key-apikey?) } ))

  (defn post-user-settings-write [mbody]
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::userusw-types mbody) ]}
     (let [ fmbody (pfilter-keys mbody [:settings]) ]
       (post-with-sign path-user-settings-write (json/write-str fmbody) default-timeout)))

  (defn api-key? [v]
     (every? #(re-matches regexp-apikey %) v))

  (spec/def ::userusr-types
    (dict { :keys (spec/and vector? #(api-key? %)) } ))

  (defn post-user-settings-read [mbody]
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::userusr-types mbody) ]}
     (let [ fmbody (pfilter-keys mbody [:keys]) ]
       (post-with-sign path-user-settings-read (json/write-str fmbody) default-timeout)))

  (def vec-isPublic [1 0])

  (spec/def ::userpph-types
     (dict { :isPublic #(in? vec-isPublic %) } ))

  (defn post-pulse-history [mbody]
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::userpph-types mbody) ]}
     (let [ fmbody (pfilter-keys mbody [:isPublic]) ]
       (post-with-sign path-post-pulse-history (json/write-str fmbody) default-timeout)))

  (defn isTitle? [^String str]
    (let [lenght (.lenght str)]
    (and (>= lenght 16) (<= lenght 120) )))

  (def vec-isPin [1 0])
  (def regexp-base64 #"^([A-Za-z0-9+/]{4})*([A-Za-z0-9+/]{3}=|[A-Za-z0-9+/]{2}==)?$")
  (defn all-base64? [v]
     (every? #(re-matches regexp-base64 %) v))

  (def vec-disableComments [1 0])  ;; 1 - disable

  (spec/def ::userpw-types
    (dict { :title (spec/and :string/ne-string isTitle?) }
          { :content :string/ne-string  }
          ^:opt { :isPublic #(in? vec-isPublic %) }
          ^:opt { :isPin #(in? vec-isPin %) }
          ^:opt { :attachments all-base64? } ;; image in base64
          ^:opt { :disableComments #(in? vec-disableComments %) }
          ^:opt { :parent :string/ne-string } ))  ;;  Pulse Message ID (PID)

  (defn post-pulse-write [mbody]
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::userpw-types mbody) ]}
     (let [ fmbody (pfilter-keys mbody [:title :content :isPublic :isPin :attachments :disableComments :parent]) ]
       (post-with-sign path-pulse-write (json/write-str fmbody) default-timeout)))

  (spec/def ::userpd-types
     (dict { :pid (spec/and :string/ne-string #(parse-long %)) } ))

  (defn post-pulse-delete [mbody]
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::userpd-types mbody) ]}
   (let [ fmbody (pfilter-keys mbody [:pid]) ]
     (post-with-sign path-pulse-delete (json/write-str fmbody) default-timeout)))

  ;; Merchants

  (def path-submit-invoice "auth/w/ext/pay/invoice/create")
  (def path-invoice-list "auth/r/ext/pay/invoices")

  (def vec-currency ["USD"])
  (def vec-payCurrencies ["BTC" "ETH" "UST-ETH" "UST-TRX" "UST-LBT" "LNX" "LBT"])
  (defn payCurrencies? [v]
     (every? #(in? vec-payCurrencies %) v))

  (defn duration? [val] (and (>= val 300) (<= val 86400)))

  (defn url? [str]
    (try
      (io/as-url str)
    (catch clojure.lang.ExceptionInfo e
      nil)
    (catch Exception e
      nil)))

  (spec/def ::usersi-types
    (dict { :amount (spec/and :string/ne-string isTitle?) }
          { :currency (spec/and :string/ne-string #(in? vec-currency %)) }
          ^:opt { :payCurrencies payCurrencies? }
          ^:opt { :duration (spec/and int? duration?) }
          ^:opt { :orderId :string/ne-string }
          ^:opt { :webhook (spec/and :string/ne-string url?) } ;; url
          ^:opt { :redirectUrl (spec/and :string/ne-string url?) } ;; url
          ^:opt { :customerInfo map? }
          ))

  (defn post-submit-invoice [mbody]
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::usersi-types mbody) ]}
     (let [ fmbody (pfilter-keys mbody [:amount :currency :payCurrencies :duration :orderId :webhook :redirectUrl :customerInfo]) ]
       (post-with-sign path-submit-invoice (json/write-str fmbody) default-timeout)))

  (defn max-100? [val] (max? val 100))

  (spec/def ::useril-types
     (dict { :id  :string/ne-string } ;; Unique invoice identifier
           ^:opt { :start    :gn/id }
           ^:opt { :end      :gn/id }
           ^:opt { :limit (spec/and :int/limit max-100? ) } ))

  (defn post-invoice-list [mbody]
  {:pre [ (spec/valid? :map/ntnl mbody) (spec/valid? ::useril-types mbody) ]}
     (let [ fmbody (pfilter-keys mbody [:id :start :end :limit]) ]
       (post-with-sign path-invoice-list (json/write-str fmbody) default-timeout)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (get-method-by-currency "USD"))
  (println (spec/valid? :map/ntnl { :id 12345 :amount "19.03" }))
  (println (spec/valid? ::usercp-types { :id 12345 :amount "19.03" }))
  (println (spec/valid? ::userkf-types {:type "loan" :id [123,124,125] :changes { "123" 1 "124" 1 "125" 1}}))
  ;;(println (post-claim-position { :id 12345 :amount "19" }))
  (println (cancel-orders))
  (println (spec/valid? ::userds-types { :keys "ALL" }))
  (try
    (println (orders-history { :limit 10 :end (str (inst-ms (java.util.Date.))) }))
    (println (spec/valid? ::userso-types { :gid (inst-ms (java.util.Date.)) :cid (inst-ms (java.util.Date.))  :type "EXCHANGE LIMIT" :symbol "tTESTBTC:TESTUSD" :price "10000" :amount "0.001" :flags 0 }))
    (println (spec/explain ::userso-types { :gid (inst-ms (java.util.Date.)) :cid (inst-ms (java.util.Date.))  :type "EXCHANGE LIMIT" :symbol "tTESTBTC:TESTUSD" :price "10000" :amount "0.001" :flags 0 }))
    (println (submit-order { :gid (inst-ms (java.util.Date.)) :cid (inst-ms (java.util.Date.))  :type "EXCHANGE LIMIT" :symbol "tTESTBTC:TESTUSD" :price "10000" :amount "0.001" :flags 0 }))
  (catch Throwable ex
    (.printStackTrace ex)))


 (try
   (println (orders-history { :limit 10 :id [12345 1234567] }))
 (catch Throwable ex
   (println "Error!")))

  (try
    (println (retrive-orders ""))
  (catch Throwable ex
    (println "Error--")))

  (try
    (println (retrive-orders 12345 1234567 12345678))
  (catch Throwable ex
    (println "Error--**")
    (.printStackTrace ex)))

  (println (submit-order { :type "EXCHANGE LIMIT" :symbol "tTESTBTC:TESTUSD" :price "15000" :amount "0.001" :flags 0 }))

  (println "1")
  (println (spec/valid? :map/ntnl  { :Symbol "tBTCUSD" :Precision "P0"}))
  (println "2")
  (println (spec/valid? ::userbm-types { :Symbol "tBTCUSD" :Precision "P0"}))
  (println "3")
  (println (spec/valid? :map/ntnl { :len 1 }))
  (println "4")
  (println (spec/valid? ::userbq-types { :len 1 }))

  (println "11")
  (println (spec/valid? :map/ntnl  { :Symbol "fBTC" :Precision "P0"}))
  (println "21")
  (println (spec/valid? ::userbm-types { :Symbol "fBTC" :Precision "P0"}))
  (println "31")
  (println (spec/valid? :map/ntnl { :len 1 }))
  (println "41")
  (println (spec/valid? ::userbq-types { :len 25 }))
  (println (book { :Symbol "tBTCUSD" :Precision "P0"} { :len 1 }))

  ;;{ :Symbol "fBTC" :Precision "P0"} { :len 25 }

  (try
    (println (ticker ""))
  (catch Throwable ex
          (println "Error.")))

  (try
    (tickers { :symbols "" })
  (catch Throwable ex
    (println "Error.tks")))
          ;;(throw (Exception. ex))
  ;;(println (tickers-history nil))
  ;;(println (spec/valid? ::userth-types { :symbols "" :limit 10 }))
  ;;(println (tickers-history { :symbols "" :limit 10 }))

  ;;(println (post-active-funding-offers))

  (println (cancel-orders))
  (println (post-cancel-funding-offer -1))
  (try
  (println (post-submit-funding-offer { :type "LIMIT" :symbol "fTESTUSD" :amount "25" :rate "0.1" :period 2 :flags 0 }))
  (catch clojure.lang.ExceptionInfo e
    (prn "caught" e)
    (println (ex-data e)))
  (catch Exception e
    (.printStackTrace e)))



  (try
  (println (post-deriv-pos-coll-limits  { :symbol "tBTCUSD" }))
  (catch clojure.lang.ExceptionInfo e
     (let [{:keys [status body]} (ex-data e)]
        (println status)
        (println body)))
      ;;(.printStackTrace e))
  (catch Exception e
      (.printStackTrace e)))

  (try
  (post-position-audit {})
  ;;  (println (post-deriv-pos-coll-limits  {}))
  (catch clojure.lang.ExceptionInfo e
    (prn "caught" e)
    (println (ex-data e)))
  (catch Exception e
    (.printStackTrace e)))


  (println (post-position-history  { :limit 10 }))
  (println (post-position-snapshot { :limit 10 }))



  (println (post-margin-info "sym_all"))
  (println (post-retrieve-position))

  (try
  (let [ cid (inst-ms (java.util.Date.))
         ;;odata (submit-order { :cid cid :type "EXCHANGE LIMIT" :symbol "tTESTBTC:TESTUSD" :price "15000" :amount "0.001" :flags 0 })
         odata (submit-order { :type "EXCHANGE LIMIT" :symbol "tTESTBTC:TESTUSD" :price "15000" :amount "0.001" :flags 0 })
         ordid (get-in odata [4 0 0])
         orddata (submit-order { :cid cid :type "EXCHANGE LIMIT" :symbol "tTESTBTC:TESTUSD" :price "11000" :amount "0.001" :flags 0 })
         ordidp (get-in orddata [4 0 0])
         ]
  (println odata)
  (println (get-in odata [4 0 0]))
  (println (get-in odata [6]))
  (println (update-order { :id ordid :price "17000" }))
  ;;(println (post-deriv-pos-coll-limits  { :symbol "tTESTBTC:TESTUSD" }))
  (println "Orders History")
  (println (orders-history { :limit 10 :id [12345 1234567] }))
  ;;(println (cancel-order ordid)))
  (println "Retrive Order")
  (println (retrive-order ordid))
  (println (retrive-orders "tTESTBTC:TESTUSD"))
  (println (retrive-orders ordid ordidp))
  (println "Order Trades")
  (println (order-trades { :Symbol "tTESTBTC:TESTUSD" :OrderId ordid } ))
  (println "POST TRADES")
  (println (ptrades "tTESTBTC:TESTUSD" {:mem "" :limit 10 :sort 1}))
  (println (ptrades { :limit 10}))
  (println "Ledgers")
  (println (pledgers {:category 5 :limit 10})) ;; :category 5
  ;;(println (count (retrive-order ordid)))
  (println (cancel-orders)))
  (catch clojure.lang.ExceptionInfo e
      (ex-data e)
      (.printStackTrace e))
  (catch Exception e
      (.printStackTrace e)))

  (println "Market Average Price")
  (println (:body (post-request "https://api-pub.bitfinex.com/v2/calc/trade/avg?symbol=tBTCUSD&amount=100" default-timeout)))
  (println (:body (post-request "https://api.bitfinex.com/v2/calc/fx" (json/write-str { :ccy1 "BTC" :ccy2 "USD" })  default-timeout)))

  (try
  (post-wallets)
  (catch clojure.lang.ExceptionInfo e
      (ex-data e)
      (.printStackTrace e))
  (catch Exception e
      (.printStackTrace e)))

  (println "Retrive Orders")
  (println { 123 "1999-09-24" 124 "1999-09-25" })
  (println (retrive-orders "tBTCUSD"))
  (println (retrive-orders 1 2 3 4))

  (println "Ticker")
  (println (ticker "fBTC"))
  (println (class (ticker "fBTC")))
  (println "Status")
  (println (platform-status))
  (println "Tickers")
  (println (tickers { :symbols "ALL" }))
  (println "TickersHistory")
  (println (tickers-history { :symbols "ALL" :limit "10"  :start nil :end nil}))
  (println "Book")
  (println (book { :Symbol "tBTCUSD" :Precision "P0"} { :len "100" })) ;; len 1 25 100
  (println "Trades")
  (println (trades "tBTCUSD" { :limit "10" }))
  (println "STATS")
  (println (stats {:Key "pos.size" :Size "1m" :Symbol "tBTCUSD" :Side "long" :Section "hist"} { :limit "10" :start "" :end "" :sort "-1" }))
  (println "CANDLES")
  (println (candles {:TimeFrame "1m" :Symbol "tBTCUSD" :Section "hist"} { :limit "" :start "" :end ""  :sort "1"}))
  (println "CONFIGS")
  (println (configs {:Action "map" :Object "currency" :Detail "sym"}))
  (println "DERIV STATUS")
  (println (derivetives-status "deriv" { :keys "tBTCF0:USTF0,tETHF0:USTF0" }))
  (println "DERIV STATUS HISTORY")
  (println (derivetives-status-history { :type "deriv" :symbol "tBTCF0:USTF0" } { :sort "-1" :start "" :end "" :limit "10" }))
  (println "Liquidations")
  (println (liquidations { :start "157057800000" :end "1573566992000" :limit "10" :sort "-1" } ))
  (println "Leaderboards")
  (println (leaderboards { :Key "vol" :Time_Frame "3h" :Symbol "tBTCUSD" :Section "hist" } { :sort "-1" :start "" :end "" :limit "10" }))
  (println "Pulsehistory")
  (println (pulsehistory { :limit "2" :end "1593608548140"}))
  (println "PulseProfile")
  (println (pulseprofile "Bitfinex"))
  (println "FundingStats")
  (println (fundingstats "fUSD" { :limit "100" :start "" :end ""  :sort "-1"}))
  (println "Hello, World!"))
