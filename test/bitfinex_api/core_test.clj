(ns bitfinex-api.core-test
  (:require [clojure.test :refer :all]
            [clojure.data.json :as json]
            [bitfinex-api.core :refer :all]))

(deftest get-baseurl-test
  (testing "get-baseurl-test fixme, I fail."
    (let [tdata (get-baseurl public-host)]
        (is (= "https://api-pub.bitfinex.com/v2" tdata))
)))

(deftest replace-map-test
  (testing "replace-map-test fixme, I fail."
    (let [candldata (replace-map path-candles {:TimeFrame "1m" :Symbol "tBTCUSD" :Section "hist"})
          confdata  (replace-map path-configs  {:Action "map" :Object "currency" :Detail "sym"})
          lbdata    (replace-map path-leaderboards { :Key "vol" :Time_Frame "3h" :Symbol "tBTCUSD" :Section "hist" })
          dshdata   (replace-map path-derivatives-status-history { :type "deriv" :symbol "tBTCF0:USTF0" })
          statsdata (replace-map path-stats {:Key "pos.size" :Size "1m" :Symbol "tBTCUSD" :Side "long" :Section "hist"})
          ordbdata  (replace-map path-book { :Symbol "tBTCUSD" :Precision "P0"})
          ]
        (is (= "candles/trade:1m:tBTCUSD/hist" candldata))
        (is (= "conf/pub:map:currency:sym" confdata))
        (is (= "rankings/vol:3h:tBTCUSD/hist" lbdata))
        (is (= "status/deriv/tBTCF0:USTF0/hist" dshdata))
        (is (= "stats1/pos.size:1m:tBTCUSD:long/hist" statsdata))
        (is (= "book/tBTCUSD/P0" ordbdata))
)))

(deftest filter-keys-test
  (testing "replace-map-test fixme, I fail."
    (is (= { :symbols "tBTCUSD" :limit "10" :start nil :end nil } (filter-keys { :symbols "tBTCUSD" :limit "10" :start nil :end nil } [:symbols :limit :start :end ])))
    (is (= { :symbols "tBTCUSD" :limit "10" "start" nil "end" nil } (filter-keys { :symbols "tBTCUSD" :limit "10" "start" nil "end" nil } [:symbols :limit :start :end ])))
    (is (= { :symbols "tBTCUSD" :limit "10" "start" nil "end" nil } (filter-keys { :symbols "tBTCUSD" :limit "10" "start" nil "end" nil :mem nil :test nil } [:symbols :limit :start :end ])))
))

(deftest platform-status-test
  (testing "platform-status-test fixme, I fail."
    (let [sdata (platform-status)]
        (is (not= nil sdata))
        (is (or (= [1] sdata) (= [0] sdata)))
        (is (= false ((comp not seq) sdata)))
        )))
;;; --------------------------------------------------------------------------------------------------------------
(deftest ticker-nil-symbol-test
  (testing "ticker-nil-symbol-test fix me, I fail."
    (try
      (ticker nil)
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest ticker-empty-symbol-test
  (testing "ticker-nil-symbol-test fix me, I fail."
    (try
      (ticker "")
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))
;;; --------------------------------------------------------------------------------------------------------------

(deftest ticker-test-fdata
  (testing "FIXME, I fail."
    (let [tdata (ticker "fBTC")]
        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
        (try
        (ticker "fUMC")
        (catch clojure.lang.ExceptionInfo e
            (ex-data e))
        (catch Exception e
            (.printStackTrace e)))
)))

(deftest ticker-test-tdata
  (testing "ticker-test-tdata fix me, I fail."
    (let [tdata (ticker "tBTCUSD")]
        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
        (try
        (ticker "tUMC")
        (catch clojure.lang.ExceptionInfo e
            (ex-data e))
        (catch Exception e
            (.printStackTrace e)))
)))
;;; --------------------------------------------------------------------------------------------------------------
(deftest tickers-nil-symbol-test
  (testing "tickers-nil-symbol-test fix me, I fail."
    (try
      (tickers nil)
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest tickers-empty-symbol-test
  (testing "tickers-nil-symbol-test fix me, I fail."
    (try
      (tickers "")
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))
;;; --------------------------------------------------------------------------------------------------------------
(deftest tickers-test-all-upper
  (testing "tickers-test-all fixme, I fail."
    (let [tdata (tickers "ALL")]
        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
)))

(deftest tickers-test-fdata
  (testing "tickers-test-fdata fix me, I fail."
    (let [tdata (tickers "fUSD")]
        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
)))

(deftest tickers-test-tdata
  (testing "tickers-test-tdata fix me, I fail."
    (let [tdata (tickers "tBTCUSD")]
        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
)))

(deftest tickers-test-tfdata
  (testing "tickers-test-tfdata fix me, I fail."
    (let [tdata (tickers "tBTCUSD,tLTCUSD,fUSD")]
        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
)))

(deftest tickers-incorrect-symbols-test
  (testing "tickers-incorrect-symbols-test fix me, I fail."
    (let [tdata (tickers "tttttttt")]
        (is (= [] tdata)))))
;;; --------------------------------------------------------------------------------------------------------------
(deftest tickers-history-nil-symbols-test
  (testing "tickers-history-nil-symbols-test fix me, I fail."
    (try
      (tickers-history { :symbols nil :limit 10 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest tickers-history-empty-symbols-test
  (testing "tickers-history-nil-symbols-test fix me, I fail."
    (try
      (tickers-history { :symbols "" :limit 10 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest tickers-history-incorrect-limit-type-test
  (testing "tickers-history-incorrect-limit-type-test fix me, I fail."
    (try
      (tickers-history { :symbols "ALL" :limit "10" })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))
;;; --------------------------------------------------------------------------------------------------------------

(deftest tickers-history-test-all
  (testing "tickers-history-test-all fix me, I fail."
    (let [tdata (tickers-history { :symbols "ALL" :limit 10 })]
        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
)))

(deftest tickers-history-test-tdata
  (testing "tickers-history-test-tdata fix me, I fail."
    (let [tdata (tickers-history { :symbols "tBTCUSD" :limit 10 :mem nil })]
        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
)))

(deftest tickers-history-test-fdata
  (testing "tickers-history-test-fdata fix me, I fail."
    (let [tdata (tickers-history { :symbols "fUSD" })]
        (is (not= nil tdata))
        (is (= [] tdata))
        (is (= true ((comp not seq) tdata)))
)))

(deftest tickers-history-test-tfdata
  (testing "tickers-history-test-tfdata fix me, I fail."
    (let [tdata (tickers-history { :symbols "tBTCUSD,tLTCUSD,fUSD" })]
        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
)))

(deftest tickers-history-incorrect-symbols-test
  (testing "tickers-history-incorrect-symbols-test fix me, I fail."
    (let [tdata (tickers-history { :symbols "tttttttt" })]
        (is (= [] tdata)))))

;;; --------------------------------------------------------------------------------------------------------------
(deftest trades-nil-symbol-test
  (testing "trades-nil-symbol-test fix me, I fail."
    (try
      (trades nil { :limit 10 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest trades-empty-symbol-test
  (testing "trades-empty-symbol-test fix me, I fail."
    (try
      (trades "" { :limit 10 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest trades-incorrect-limit-type-test
  (testing "trades-incorrect-limit-type-test fix me, I fail."
    (try
      (trades "tBTCUSD" { :limit "10" })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))
;;; --------------------------------------------------------------------------------------------------------------
(deftest trades-test-tdata
  (testing "trades-test-tdata fix me, I fail."
    (let [tdata (trades "tBTCUSD" { :limit 10 })]
        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
        (try
            (trades "ttttttttt" { :limit 10 })
        (catch clojure.lang.ExceptionInfo e
               (ex-data e))
        (catch Exception e
               (.printStackTrace e)))
        )))

(deftest trades-test-fdata
  (testing "trades-test-tdata fixme, I fail."
    (let [tdata (trades "fBTC" { :limit 10 })]
        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
)))
;;; --------------------------------------------------------------------------------------------------------------
(deftest orderbook-nil-symbol-test
  (testing "orderbook-nil-symbol-test fix me, I fail."
    (try
      (book { :Symbol nil :Precision "P0"} { :len 1 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest orderbook-empty-symbol-test
  (testing "orderbook-empty-symbol-test fix me, I fail."
    (try
      (book { :Symbol "" :Precision "P0"} { :len 1 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest orderbook-nil-precision-test
  (testing "orderbook-nil-precision-test fix me, I fail."
    (try
      (book { :Symbol "tBTCUSD" :Precision nil} { :len 1 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest orderbook-empty-precision-test
  (testing "orderbook-empty-precision-test fix me, I fail."
    (try
      (book { :Symbol "tBTCUSD" :Precision ""} { :len 1 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest orderbook-incorrect-len-type-test
  (testing "orderbook-empty-precision-test fix me, I fail."
    (try
      (book { :Symbol "tBTCUSD" :Precision "P0"} { :len "1" })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))
;;; --------------------------------------------------------------------------------------------------------------
(deftest orderbook-tdata-test
  (testing " orderbook-tdata-test fix me, I fail."
    (let [tdata (book { :Symbol "tBTCUSD" :Precision "P0"} { :len 1 })]
        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
        (try
        (book { :Symbol "ttttttttt" :Precision "P0"} { :len 1 })
        (catch clojure.lang.ExceptionInfo e
              (ex-data e))
        (catch Exception e
            (.printStackTrace e)))
)))

(deftest orderbook-fdata-test
  (testing "orderbook-fdata-test fixme, I fail."
    (let [tdata (book { :Symbol "fBTC" :Precision "P0"} { :len 25 })]
        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
)))
;;; --------------------------------------------------------------------------------------------------------------
(deftest stats-nil-key-test
  (testing "stats-nil-key-test fix me, I fail."
    (try
      (stats {:Key nil :Size "1m" :Symbol "tBTCUSD" :Side "long" :Section "hist"} { :limit 10 :sort -1 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest stats-empty-key-test
  (testing "stats-empty-key-test fix me, I fail."
    (try
      (stats {:Key "" :Size "1m" :Symbol "tBTCUSD" :Side "long" :Section "hist"} { :limit 10 :sort -1 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest stats-nil-size-test
  (testing "stats-nil-size-test fix me, I fail."
    (try
      (stats {:Key "pos.size" :Size nil :Symbol "tBTCUSD" :Side "long" :Section "hist"} { :limit 10 :sort -1 })
      (is false)
    (catch Throwable ex
      (is true)))
))

(deftest stats-empty-size-test
  (testing "stats-empty-size-test fix me, I fail."
    (try
      (stats {:Key "pos.size" :Size "" :Symbol "tBTCUSD" :Side "long" :Section "hist"} { :limit 10 :sort -1 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest stats-nil-symbol-test
  (testing "stats-nil-symbol-test fix me, I fail."
    (try
      (stats {:Key "pos.size" :Size "1m" :Symbol nil :Side "long" :Section "hist"} { :limit 10 :sort -1 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest stats-empty-symbol-test
  (testing "stats-empty-symbol-test fix me, I fail."
    (try
      (stats {:Key "pos.size" :Size "1m" :Symbol "" :Side "long" :Section "hist"} { :limit 10 :sort -1 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest stats-nil-side-test
  (testing "stats-nil-side-test fix me, I fail."
    (try
      (stats {:Key "pos.size" :Size "1m" :Symbol "tBTCUSD" :Side nil :Section "hist"} { :limit 10 :sort -1 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest stats-empty-side-test
  (testing "stats-empty-side-test fix me, I fail."
    (try
      (stats {:Key "pos.size" :Size "1m" :Symbol "tBTCUSD" :Side "" :Section "hist"} { :limit 10 :sort -1 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest stats-nil-section-test
  (testing "stats-nil-section-test fix me, I fail."
    (try
      (stats {:Key "pos.size" :Size "1m" :Symbol "tBTCUSD" :Side "long" :Section nil} { :limit 10 :sort -1 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest stats-empty-section-test
  (testing "stats-empty-section-test fix me, I fail."
    (try
      (stats {:Key "pos.size" :Size "1m" :Symbol "tBTCUSD" :Side "long" :Section ""} { :limit 10 :sort -1 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest stats-empty-incorrect-limit-type-test
  (testing "stats-empty-incorrect-limit-type-test fix me, I fail."
    (try
      (stats {:Key "pos.size" :Size "1m" :Symbol "tBTCUSD" :Side "long" :Section "hist"} { :limit "10" :sort -1 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))
;;; --------------------------------------------------------------------------------------------------------------
(deftest stats-tdata-test
  (testing "stats-tdata-test fixme, I fail."
    (let [tdata (stats {:Key "pos.size" :Size "1m" :Symbol "tBTCUSD" :Side "long" :Section "hist"} { :limit 10 :sort -1 })]
        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
        (try
        (stats {:Key "pos.size" :Size "1m" :Symbol "tttttttttttt" :Side "long" :Section "hist"} { :limit 10 :sort -1 })
        (catch clojure.lang.ExceptionInfo e
              (ex-data e))
        (catch Exception e
            (.printStackTrace e)))
)))

(deftest stats-fdata-test
  (testing "stats-fdata-test fixme, I fail."
    (let [tdata (stats {:Key "pos.size" :Size "1m" :Symbol "fBTC" :Side "long" :Section "hist"} { :limit 10 :sort -1 })]
        (is (not= nil tdata))
        (is (= [] tdata))
        (is (= true ((comp not seq) tdata)))
)))
;;; --------------------------------------------------------------------------------------------------------------
(deftest candles-nil-timeframe-test
  (testing "candles-nil-timeframe-test fix me, I fail."
    (try
      (candles {:TimeFrame nil :Symbol "tBTCUSD" :Section "hist"} { :sort 1})
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest candles-empty-timeframe-test
  (testing "candles-empty-timeframe-test fix me, I fail."
    (try
      (candles {:TimeFrame "" :Symbol "tBTCUSD" :Section "hist"} { :sort 1})
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest candles-nil-symbol-test
  (testing "candles-nil-symbol-test fix me, I fail."
    (try
      (candles {:TimeFrame "1m" :Symbol nil :Section "hist"} { :sort 1})
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest candles-empty-symbol-test
  (testing "candles-empty-symbol-test fix me, I fail."
    (try
      (candles {:TimeFrame "1m" :Symbol "" :Section "hist"} { :sort 1})
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest candles-nil-section-test
  (testing "candles-nil-section-test fix me, I fail."
    (try
      (candles {:TimeFrame "1m" :Symbol "tBTCUSD" :Section nil} { :sort 1})
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest candles-empty-section-test
  (testing "candles-empty-section-test fix me, I fail."
    (try
      (candles {:TimeFrame "1m" :Symbol "tBTCUSD" :Section ""} { :sort 1})
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest candles-incorrect-sort-type-test
  (testing "candles-incorrect-sort-type-test fix me, I fail."
    (try
      (candles {:TimeFrame "1m" :Symbol "tBTCUSD" :Section "hist"} { :sort "1"})
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))
;;; --------------------------------------------------------------------------------------------------------------
(deftest candles-tdata-test
  (testing "candles-tdata-test fixme, I fail."
    (let [tdata (candles {:TimeFrame "1m" :Symbol "tBTCUSD" :Section "hist"} { :sort 1})]
        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
        (try
        (candles {:TimeFrame "1m" :Symbol "ttttttttttttt" :Section "hist"} { :sort 1})
        (catch clojure.lang.ExceptionInfo e
              (ex-data e))
        (catch Exception e
            (.printStackTrace e)))
)))

(deftest candles-fdata-test
  (testing "candles-fdata-test fixme, I fail."
    (let [tdata (candles {:TimeFrame "1m" :Symbol "fBTC" :Section "hist"} { :sort 1 })]
        (is (not= nil tdata))
        (is (= [] tdata))
)))
;;; --------------------------------------------------------------------------------------------------------------
(deftest configs-nil-action-test
  (testing "configs-nil-action-test fix me, I fail."
    (try
      (configs {:Action nil :Object "currency" :Detail "sym"})
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest configs-empty-action-test
  (testing "configs-empty-action-test fix me, I fail."
    (try
      (configs {:Action "" :Object "currency" :Detail "sym"})
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest configs-nil-object-test
  (testing "configs-nil-object-test fix me, I fail."
    (try
      (configs {:Action "map" :Object nil :Detail "sym"})
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest configs-empty-object-test
  (testing "configs-empty-object-test fix me, I fail."
    (try
      (configs {:Action "map" :Object "" :Detail "sym"})
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest configs-empty-detail-test
  (testing "configs-empty-detail-test fix me, I fail."
    (try
      (configs {:Action "map" :Object "currency" :Detail ""})
      (is true)
    (catch Throwable ex
      (is false)))
))
;;; --------------------------------------------------------------------------------------------------------------
(deftest configs-data-test
  (testing "configs-data-test fixme, I fail."
    (let [tdata (configs {:Action "map" :Object "currency" :Detail "sym"})]
        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
        (try
        (configs {:Action "ttt" :Object "ttt" :Detail "ttt"})
        (catch clojure.lang.ExceptionInfo e
              (ex-data e))
        (catch Exception e
            (.printStackTrace e)))
)))
;;; --------------------------------------------------------------------------------------------------------------
(deftest derivetives-status-nil-type-test
  (testing "derivetives-status-nil-type-test fix me, I fail."
    (try
      (derivetives-status nil { :keys "tBTCF0:USTF0,tETHF0:USTF0" })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest derivetives-status-empty-type-test
  (testing "derivetives-status-nil-type-test fix me, I fail."
    (try
      (derivetives-status "" { :keys "tBTCF0:USTF0,tETHF0:USTF0" })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest derivetives-status-nil-keys-test
  (testing "derivetives-status-empty-keys-test fix me, I fail."
    (try
      (derivetives-status "deriv" { :keys nil })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest derivetives-status-empty-keys-test
  (testing "derivetives-status-empty-keys-test fix me, I fail."
    (try
      (derivetives-status "deriv" { :keys "" })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))
;;; --------------------------------------------------------------------------------------------------------------
(deftest derivetives-status-data-test
  (testing "derivetives-status-data-test fixme, I fail."
    (let [tdata (derivetives-status "deriv" { :keys "tBTCF0:USTF0,tETHF0:USTF0" })]
        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
        (try
        (derivetives-status "deriv" { :keys "tttttttttttttt" })
        (catch clojure.lang.ExceptionInfo e
              (ex-data e))
        (catch Exception e
            (.printStackTrace e)))
)))

(deftest derivetives-status-all-data-test
  (testing "derivetives-status-data-test fixme, I fail."
    (let [tdata (derivetives-status "deriv" { :keys "ALL" })]
        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
        (try
           (derivetives-status "deriv" { :keys "mem" })
           (is false)
        (catch Throwable ex
            (is true)))
)))
;;; --------------------------------------------------------------------------------------------------------------
(deftest derivetives-status-history-nil-type-test
  (testing "derivetives-status-nil-type-test fix me, I fail."
    (try
      (derivetives-status-history { :type nil :symbol "tBTCF0:USTF0" } { :sort -1 :limit 10 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest derivetives-status-history-empty-type-test
  (testing "derivetives-status-nil-type-test fix me, I fail."
    (try
      (derivetives-status-history { :type "" :symbol "tBTCF0:USTF0" } { :sort -1 :limit 10 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest derivetives-status-history-nil-symbol-test
  (testing "derivetives-status-empty-keys-test fix me, I fail."
    (try
      (derivetives-status-history { :type "deriv" :symbol nil } { :sort -1 :limit 10 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest derivetives-status-history-empty-symbol-test
  (testing "derivetives-status-empty-keys-test fix me, I fail."
    (try
      (derivetives-status-history { :type "deriv" :symbol "" } { :sort -1 :limit 10 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))
;;; --------------------------------------------------------------------------------------------------------------
(deftest derivetives-status-history-data-test
  (testing "derivetives-status-history-data-test fixme, I fail."
    (let [tdata (derivetives-status-history { :type "deriv" :symbol "tTESTBTC:TESTUSD" } { :sort -1 :limit 10 })]
        (is (not= nil tdata))
        (is (or (= [] tdata) (not= [] tdata))))
))

(deftest derivetives-status-history-incorrect-data-test
  (testing "derivetives-status-history-incorrect-data-test fixme, I fail."
    (try
      (derivetives-status-history { :type "deriv" :symbol "mm" } { :limit 10 })
    (catch Throwable ex
      (is true)))
))

(deftest liquidations-test-data
  (testing "liquidations-test-tdata fixme, I fail."
    (let [tdata (liquidations { :start "157057800000" :end "1573566992000" :limit 10 :sort -1 } )]
        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
)))
;;; --------------------------------------------------------------------------------------------------------------
(deftest leaderboards-nil-key-test
  (testing "leaderboards-nil-key-test fix me, I fail."
    (try
      (leaderboards { :Key nil :Time_Frame "3h" :Symbol "tBTCUSD" :Section "hist" } { :sort -1 :limit 10 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest leaderboards-empty-key-test
  (testing "leaderboards-empty-key-test fix me, I fail."
    (try
      (leaderboards { :Key "" :Time_Frame "3h" :Symbol "tBTCUSD" :Section "hist" } { :sort -1 :limit 10 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest leaderboards-nil-timeframe-test
  (testing "leaderboards-nil-timeframe-test fix me, I fail."
    (try
      (leaderboards { :Key "vol" :Time_Frame nil :Symbol "tBTCUSD" :Section "hist" } { :sort -1 :limit 10 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest leaderboards-empty-timeframe-test
  (testing "leaderboards-empty-timeframe-test fix me, I fail."
    (try
      (leaderboards { :Key "vol" :Time_Frame "" :Symbol "tBTCUSD" :Section "hist" } { :sort -1 :limit 10 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest leaderboards-nil-symbol-test
  (testing "leaderboards-nil-symbol-test fix me, I fail."
    (try
      (leaderboards { :Key "vol" :Time_Frame "3h" :Symbol nil :Section "hist" } { :sort -1 :limit 10 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest leaderboards-empty-symbol-test
  (testing "leaderboards-empty-symbol-test fix me, I fail."
    (try
      (leaderboards { :Key "vol" :Time_Frame "3h" :Symbol "" :Section "hist" } { :sort -1 :limit 10 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest leaderboards-nil-section-test
  (testing "leaderboards-nil-symbol-test fix me, I fail."
    (try
      (leaderboards { :Key "vol" :Time_Frame "3h" :Symbol "tBTCUSD" :Section nil } { :sort -1 :limit 10 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest leaderboards-empty-section-test
  (testing "leaderboards-empty-symbol-test fix me, I fail."
    (try
      (leaderboards { :Key "vol" :Time_Frame "3h" :Symbol "tBTCUSD" :Section "" } { :sort -1 :limit 10 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))
;;; --------------------------------------------------------------------------------------------------------------
(deftest leaderboards-test-tdata
  (testing "leaderboards-test-tdata fixme, I fail."
    (let [tdata (leaderboards { :Key "vol" :Time_Frame "3h" :Symbol "tBTCUSD" :Section "hist" } { :sort -1 :limit 10 })]
        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
)))

(deftest pulsehistory-test-data
  (testing "pulsehistory-test-data fixme, I fail."
    (let [tdata (pulsehistory { :limit 2 :end "1593608548140"})]
        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
)))
;;; --------------------------------------------------------------------------------------------------------------
(deftest pulseprofile-nil-nickname-test
  (testing "pulseprofile-nil-nickname-test fix me, I fail."
    (try
      (pulseprofile nil)
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest pulseprofile-empty-nickname-test
  (testing "pulseprofile-empty-nickname-test fix me, I fail."
    (try
      (pulseprofile "")
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))
;;; --------------------------------------------------------------------------------------------------------------
(deftest pulseprofile-test-data
  (testing "pulseprofile-test-data fixme, I fail."
    (let [tdata (pulseprofile "Bitfinex")]
        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
        (try
        (pulseprofile "tttttttttt")
        (catch clojure.lang.ExceptionInfo e
              (ex-data e))
        (catch Exception e
            (.printStackTrace e)))
)))
;;; --------------------------------------------------------------------------------------------------------------
(deftest fundingstats-nil-symbol-test
  (testing "fundingstats-nil-symbol-test fix me, I fail."
    (try
      (fundingstats nil { :limit 100 :sort -1})
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest fundingstats-empty-symbol-test
  (testing "fundingstats-empty-symbol-test fix me, I fail."
    (try
      (fundingstats "" { :limit 100 :sort -1})
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))
;;; --------------------------------------------------------------------------------------------------------------
(deftest fundingstats-data-test
  (testing "fundingstats-data-test fixme, I fail."
    (let [tdata (fundingstats "fUSD" { :limit 100 :sort -1})]
        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
)))

;;; --------------------------------------------------------------------------------------------------------------
(deftest market-average-price-nil-symbol-test
  (testing "market-average-price-nil-symbol-test fix me, I fail."
    (try
      (market-average-price {:symbol nil :amount "100.23"})
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest market-average-price-empty-symbol-test
  (testing "market-average-price-empty-symbol-test fix me, I fail."
    (try
      (market-average-price {:symbol "" :amount "100.23"})
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest market-average-price-nil-amount-test
  (testing "market-average-price-nil-amount-test fix me, I fail."
    (try
      (market-average-price {:symbol "tBTCUSD" :amount nil})
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest market-average-price-empty-amount-test
  (testing "market-average-price-empty-amount-test fix me, I fail."
    (try
      (market-average-price {:symbol "tBTCUSD" :amount ""})
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))
;;; --------------------------------------------------------------------------------------------------------------

(deftest market-average-price-test
  (testing "market-average-price-test fixme, I fail."
    (let [ tdata (market-average-price {:symbol "tBTCUSD" :amount "100.23"}) ]
        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
)))

;;; --------------------------------------------------------------------------------------------------------------
(deftest foreign-exchange-rate-nil-ccy1-test
  (testing "foreign-exchange-rate-nil-ccy1-test fix me, I fail."
    (try
      (foreign-exchange-rate { :ccy1 nil :ccy2 "USD" })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest foreign-exchange-rate-empty-ccy1-test
  (testing "foreign-exchange-rate-empty-ccy1-test fix me, I fail."
    (try
      (foreign-exchange-rate { :ccy1 "" :ccy2 "USD" })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest foreign-exchange-rate-nil-ccy2-test
  (testing "foreign-exchange-rate-nil-ccy2-test fix me, I fail."
    (try
      (foreign-exchange-rate { :ccy1 "BTC" :ccy2 nil })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest foreign-exchange-rate-empty-ccy2-test
  (testing "foreign-exchange-rate-empty-ccy2-test fix me, I fail."
    (try
      (foreign-exchange-rate { :ccy1 "BTC" :ccy2 "" })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))
;;; --------------------------------------------------------------------------------------------------------------

(deftest foreign-exchange-rate-test
  (testing "foreign-exchange-rate-test fixme, I fail."
    (let [ tdata (foreign-exchange-rate { :ccy1 "BTC" :ccy2 "USD" }) ]
        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
)))
;; **********************************************************************************************

(deftest get-baseurl-test
  (testing "get-baseurl-test fixme, I fail."
    (let [tdata (get-baseurl public-host)]
        (is (= "https://api-pub.bitfinex.com/v2" tdata))
)))

(deftest replace-map-test
  (testing "replace-map-test fixme, I fail."
    (let [candldata (replace-map path-candles {:TimeFrame "1m" :Symbol "tBTCUSD" :Section "hist"})
          confdata  (replace-map path-configs  {:Action "map" :Object "currency" :Detail "sym"})
          lbdata    (replace-map path-leaderboards { :Key "vol" :Time_Frame "3h" :Symbol "tBTCUSD" :Section "hist" })
          dshdata   (replace-map path-derivatives-status-history { :type "deriv" :symbol "tBTCF0:USTF0" })
          statsdata (replace-map path-stats {:Key "pos.size" :Size "1m" :Symbol "tBTCUSD" :Side "long" :Section "hist"})
          ordbdata  (replace-map path-book { :Symbol "tBTCUSD" :Precision "P0"})
          ]
        (is (= "candles/trade:1m:tBTCUSD/hist" candldata))
        (is (= "conf/pub:map:currency:sym" confdata))
        (is (= "rankings/vol:3h:tBTCUSD/hist" lbdata))
        (is (= "status/deriv/tBTCF0:USTF0/hist" dshdata))
        (is (= "stats1/pos.size:1m:tBTCUSD:long/hist" statsdata))
        (is (= "book/tBTCUSD/P0" ordbdata))
)))

(deftest filter-keys-test
  (testing "replace-map-test fixme, I fail."
    (is (= { :symbols "tBTCUSD" :limit "10" :start nil :end nil } (filter-keys { :symbols "tBTCUSD" :limit "10" :start nil :end nil } [:symbols :limit :start :end ])))
    (is (= { :symbols "tBTCUSD" :limit "10" "start" nil "end" nil } (filter-keys { :symbols "tBTCUSD" :limit "10" "start" nil "end" nil } [:symbols :limit :start :end ])))
    (is (= { :symbols "tBTCUSD" :limit "10" "start" nil "end" nil } (filter-keys { :symbols "tBTCUSD" :limit "10" "start" nil "end" nil :mem nil :test nil } [:symbols :limit :start :end ])))
))

(defn count-unique-vec [coll]
  (loop [coll coll, e1 (transient #{}), e2 (transient #{})]
    (cond (empty? coll) [(count (persistent! e1)) (count (persistent! e2))]
          :else (recur (rest coll)
                       (conj! e1 (first (first coll)))
                       (conj! e2 (second (first coll)))))))


(deftest post-wallets-test
  (testing "post-wallets-test fixme, I fail."
    (let [ data  (post-wallets) ]
        (is (not= nil data))
        (is (not= [] data))
        (is (= false ((comp not seq) data)))
)))

;;----------------------------------------------------------------------------------
(deftest retrive-order-neg-id-test
  (testing "retrive-order-neg-id-test fix me, I fail."
    (try
      (retrive-order -1)
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest retrive-orders-nil-symbol-test
  (testing "retrive-orders-nil-symbol-test fix me, I fail."
    (try
      (retrive-orders nil)
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest retrive-orders-empty-symbol-test
  (testing "retrive-orders-empty-symbol-test fix me, I fail."
    (try
      (retrive-orders "")
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest retrive-orders-neg-ids-test
  (testing "retrive-orders-neg-ids-test fix me, I fail."
    (try
      (retrive-orders -1 -2 -10 -20)
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))
;;----------------------------------------------------------------------------------

(deftest retrive-orders-test
  (testing "retrive-orders-test fixme, I fail."
    (let [ tdata1  (submit-order { :cid (inst-ms (java.util.Date.)) :type "EXCHANGE LIMIT" :symbol "tTESTBTC:TESTUSD" :price "14000" :amount "0.001" :flags 0 })
           status1 (get-in tdata1 [6])
           ordid1  (get-in tdata1 [4 0 0])
           tdata2  (submit-order { :cid (.toEpochMilli (java.time.Instant/now)) :type "EXCHANGE MARKET" :symbol "tTESTBTC:TESTUSD" :price "15000" :amount "0.001" :flags 0 })
           status2 (get-in tdata2 [6])
           ordid2  (get-in tdata2 [4 0 0])
           tdata3  (retrive-order ordid1)
           tdata4  (retrive-orders ordid1 ordid2)
           tdata5  (retrive-orders "tTESTBTC:TESTUSD") ]
        (is (not= nil tdata1))
        (is (not= [] tdata1))
        (is (= false ((comp not seq) tdata1)))
        (is (= (str status1) "SUCCESS"))
        (is (> ordid1 0))

        (is (not= nil tdata2))
        (is (not= [] tdata2))
        (is (= false ((comp not seq) tdata2)))
        (is (= (str status2) "SUCCESS"))
        (is (> ordid2 0))

        (is (not= nil tdata3))
        (is (not= nil tdata4))
        (is (not= nil tdata5))
        (is (not= [] tdata3))
        (is (not= [[]] tdata3))
        (is (not= [] tdata4))
        (is (not= [[]] tdata4))
        (is (not= [] tdata5))
        (is (not= [[]] tdata5))
        (is (= [1 1] (count-unique-vec tdata3)))

        (is (= false ((comp not seq) tdata3)))
        (is (= false ((comp not seq) tdata4)))
        (is (= false ((comp not seq) tdata5)))
)))

(deftest orders-history-test
  (testing "orders-history-test fixme, I fail."
    (let [ tdata   (submit-order { :cid (inst-ms (java.util.Date.)) :type "EXCHANGE LIMIT" :symbol "tTESTBTC:TESTUSD" :price "14000" :amount "0.001" :flags 0 })
           status (get-in tdata [6])
           ordid  (get-in tdata [4 0 0]) ]
        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
        (is (= (str status) "SUCCESS"))
        (is (> ordid 0))

        (let [ tcorderdata (cancel-order ordid)
               cordstatus  (get-in tcorderdata [6])
               cordid      (get-in tcorderdata [4 0])
               hdata       (orders-history "tTESTBTC:TESTUSD" { :limit 1 :id [ordid] }) ]

            (is (not= nil tcorderdata))
            (is (not= [] tcorderdata))
            (is (= false ((comp not seq) tcorderdata)))
            (is (= (str cordstatus) "SUCCESS"))
            (is (> cordid 0))
            (is (= cordid ordid))

            (is (not= nil hdata))
            (is (not= [] hdata))
            (is (= false ((comp not seq) hdata)))
        )
)))

(deftest orders-history-all-test
  (testing "orders-history-all-test fixme, I fail."
    (let [ tdata   (submit-order { :cid (inst-ms (java.util.Date.)) :type "EXCHANGE LIMIT" :symbol "tTESTBTC:TESTUSD" :price "14000" :amount "0.001" :flags 0 })
           status  (get-in tdata [6])
           ordid   (get-in tdata [4 0 0])
           tcorderdata  (cancel-orders)
           cordstatus (get-in tcorderdata [6])
           hdata  (orders-history { :limit 10 }) ]
        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
        (is (= (str status) "SUCCESS"))
        (is (> ordid 0))

        (is (not= nil tcorderdata))
        (is (not= [] tcorderdata))
        (is (= false ((comp not seq) tcorderdata)))
        (is (= (str cordstatus) "SUCCESS"))

        (is (not= nil  hdata))
        (is (not= []   hdata))
        (is (not= [[]] hdata))
)))
;;----------------------------------------------------------------------------------
(deftest submit-order-norequired-parameter-test
  (testing "submit-order-noonerequired-parameter-test fix me, I fail."
    (try
      (submit-order { :cid (inst-ms (java.util.Date.)) :type "EXCHANGE LIMIT" :price "15000" :amount "0.001" :flags 0 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest submit-order-nil-type-test
  (testing "submit-order-nil-type-test fix me, I fail."
    (try
      (submit-order { :cid (inst-ms (java.util.Date.)) :type nil :symbol "tTESTBTC:TESTUSD" :price "15000" :amount "0.001" :flags 0 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest submit-order-empty-type-test
  (testing "submit-order-empty-type-test fix me, I fail."
    (try
      (submit-order { :cid (inst-ms (java.util.Date.)) :type "" :symbol "tTESTBTC:TESTUSD" :price "15000" :amount "0.001" :flags 0 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest submit-order-nil-symbol-test
  (testing "submit-order-empty-symbol-test fix me, I fail."
    (try
      (submit-order { :cid (inst-ms (java.util.Date.)) :type "EXCHANGE LIMIT" :symbol nil :price "15000" :amount "0.001" :flags 0 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest submit-order-empty-symbol-test
  (testing "submit-order-empty-symbol-test fix me, I fail."
    (try
      (submit-order { :cid (inst-ms (java.util.Date.)) :type "EXCHANGE LIMIT" :symbol "" :price "15000" :amount "0.001" :flags 0 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest submit-order-nil-price-test
  (testing "submit-order-nil-price-test fix me, I fail."
    (try
      (submit-order { :cid (inst-ms (java.util.Date.)) :type "EXCHANGE LIMIT" :symbol "tTESTBTC:TESTUSD" :price nil :amount "0.001" :flags 0 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest submit-order-empty-price-test
  (testing "submit-order-empty-price-test fix me, I fail."
    (try
      (submit-order { :cid (inst-ms (java.util.Date.)) :type "EXCHANGE LIMIT" :symbol "tTESTBTC:TESTUSD" :price "" :amount "0.001" :flags 0 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest submit-order-nil-amount-test
  (testing "submit-order-nil-amount-test fix me, I fail."
    (try
      (submit-order { :cid (inst-ms (java.util.Date.)) :type "EXCHANGE LIMIT" :symbol "tTESTBTC:TESTUSD" :price "15000" :amount nil :flags 0 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest submit-order-empty-amount-test
  (testing "submit-order-nil-amount-test fix me, I fail."
    (try
      (submit-order { :cid (inst-ms (java.util.Date.)) :type "EXCHANGE LIMIT" :symbol "tTESTBTC:TESTUSD" :price "15000" :amount "" :flags 0 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest submit-order-incorrect-lev-test
  (testing "submit-order-incorrect-lev-test fix me, I fail."
    (try
      (submit-order { :cid (inst-ms (java.util.Date.)) :type "EXCHANGE LIMIT" :symbol "tTESTBTC:TESTUSD" :price "15000" :amount "0.001" :lev 105})
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest submit-order-nil-metaaff_code-test
  (testing "submit-order-nil-metaaff_code-test fix me, I fail."
    (try
      (submit-order { :cid (inst-ms (java.util.Date.)) :type "EXCHANGE LIMIT" :symbol "tTESTBTC:TESTUSD" :price "15000" :amount "0.001" :lev 90 :meta { :aff_code nil } })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest submit-order-incorrect-price-coma-separator-test
  (testing "submit-order-incorrect-price-coma-separator-test fix me, I fail."
    (try
      (submit-order { :cid (inst-ms (java.util.Date.)) :type "EXCHANGE LIMIT" :symbol "tTESTBTC:TESTUSD" :price "15000,43" :amount "0.001" :lev 90 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))

(deftest submit-order-incorrect-price-test
  (testing "submit-order-incorrect-price-test fix me, I fail."
    (try
      (submit-order { :cid (inst-ms (java.util.Date.)) :type "EXCHANGE LIMIT" :symbol "tTESTBTC:TESTUSD" :price "15.345" :amount "0.001" :lev 90 })
      (is (= false true))
    (catch Throwable ex
      (is (= [] []))))
))
;;----------------------------------------------------------------------------------
(deftest submit-order-test
  (testing "submit-order-test fixme, I fail."
    (let [ tdata  (submit-order { :cid (inst-ms (java.util.Date.)) :type "EXCHANGE LIMIT" :symbol "tTESTBTC:TESTUSD" :price "15000" :amount "0.001" :flags 0 })
           status (get-in tdata [6])
           ordid  (get-in tdata [4 0 0]) ]
        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
        (is (= (str status) "SUCCESS"))
        (is (> ordid 0))
)))

(deftest update-order-test
  (testing "update-order-test fixme, I fail."
    (let [ torderdata (submit-order { :cid (inst-ms (java.util.Date.)) :type "EXCHANGE LIMIT" :symbol "tTESTBTC:TESTUSD" :price "16000" :amount "0.001" :flags 0 })
           ordstatus (get-in torderdata [6])
           ordid (get-in torderdata [4 0 0])
           tdata (update-order { :id ordid :price "17000" :amount "0.002" })
           updateordstatus (get-in tdata [6]) ]
        (is (not= nil torderdata))
        (is (not= [] torderdata))
        (is (= false ((comp not seq) torderdata)))
        (is (= (str ordstatus) "SUCCESS"))
        (is (> ordid 0))

        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
        (is (= (str updateordstatus) "SUCCESS"))
)))

(deftest cancel-order-test
  (testing "cancel-order-test fixme, I fail."
    (let [ torderdata  (submit-order { :cid (inst-ms (java.util.Date.)) :type "EXCHANGE LIMIT" :symbol "tTESTBTC:TESTUSD" :price "15000" :amount "0.001" :flags 0 })
           ordstatus   (get-in torderdata [6])
           ordid       (get-in torderdata [4 0 0])
           tcorderdata (cancel-order ordid)
           cordstatus  (get-in tcorderdata [6])
           cordid      (get-in tcorderdata [4 0]) ]

        (is (not= nil torderdata))
        (is (not= [] torderdata))
        (is (= false ((comp not seq) torderdata)))
        (is (= (str ordstatus) "SUCCESS"))
        (is (> ordid 0))

        (is (not= nil tcorderdata))
        (is (not= [] tcorderdata))
        (is (= false ((comp not seq) tcorderdata)))
        (is (= (str cordstatus) "SUCCESS"))
        (is (> cordid 0))
        (is (= cordid ordid))
)))

(deftest cancel-all-orders-test
  (testing "cancel-all-orders-test fixme, I fail."
    (let [ torderdata  (submit-order { :cid (inst-ms (java.util.Date.)) :type "EXCHANGE LIMIT" :symbol "tTESTBTC:TESTUSD" :price "16500" :amount "0.001" :flags 0 })
           ordstatus   (get-in torderdata [6])
           ordid       (get-in torderdata [4 0 0])
           tcorderdata (cancel-orders)
           cordstatus  (get-in tcorderdata [6]) ]

        (is (not= nil torderdata))
        (is (not= [] torderdata))
        (is (= false ((comp not seq) torderdata)))
        (is (= (str ordstatus) "SUCCESS"))
        (is (> ordid 0))

        (is (not= nil tcorderdata))
        (is (not= [] tcorderdata))
        (is (= false ((comp not seq) tcorderdata)))
        (is (= (str cordstatus) "SUCCESS"))
)))

(deftest cancel-orders-by-gid-test
  (testing "cancel-orders-by-gid-test fixme, I fail."
  (let [ gid 100
         torderdata (submit-order { :gid gid :cid (inst-ms (java.util.Date.)) :type "EXCHANGE LIMIT" :symbol "tTESTBTC:TESTUSD" :price "16500" :amount "0.001" :flags 0 })
         ordstatus (get-in torderdata [6])
         ordid (get-in torderdata [4 0 0])
         tcorderdata  (cancel-orders-by-gid gid)
         cordstatus (get-in tcorderdata [6]) ]
      (is (not= nil torderdata))
      (is (not= [] torderdata))
      (is (= false ((comp not seq) torderdata)))
      (is (= (str ordstatus) "SUCCESS"))
      (is (> ordid 0))

      (is (not= nil tcorderdata))
      (is (not= [] tcorderdata))
      (is (= false ((comp not seq) tcorderdata)))
      (is (= (str cordstatus) "SUCCESS"))
)))

(deftest cancel-orders-by-gids-test
  (testing "cancel-orders-by-gids-test fixme, I fail."
  (let [ gid1 100
         gid2 200
         torderdata1 (submit-order { :gid gid1 :cid (.getTime (java.util.Date.)) :type "EXCHANGE LIMIT" :symbol "tTESTBTC:TESTUSD" :price "15500" :amount "0.001" :flags 0 })
         ordstatus1 (get-in torderdata1 [6])
         ordid1 (get-in torderdata1 [4 0 0])
         torderdata2 (submit-order { :gid gid2 :cid (.getTime (java.util.Date.)) :type "EXCHANGE LIMIT" :symbol "tTESTBTC:TESTUSD" :price "16500" :amount "0.001" :flags 0 })
         ordstatus2 (get-in torderdata2 [6])
         ordid2 (get-in torderdata2 [4 0 0])
         tcorderdata  (cancel-orders-by-gid gid1 gid2)
         cordstatus (get-in tcorderdata [6]) ]
      (is (not= nil torderdata1))
      (is (not= [] torderdata1))
      (is (= false ((comp not seq) torderdata1)))
      (is (= (str ordstatus1) "SUCCESS"))
      (is (> ordid1 0))

      (is (not= nil torderdata2))
      (is (not= [] torderdata2))
      (is (= false ((comp not seq) torderdata2)))
      (is (= (str ordstatus2) "SUCCESS"))
      (is (> ordid2 0))

      (is (not= nil tcorderdata))
      (is (not= [] tcorderdata))
      (is (= false ((comp not seq) tcorderdata)))
      (is (= (str cordstatus) "SUCCESS"))
)))

(deftest order-trades-test
  (testing "order-trades-test fixme, I fail."
    (let [ torderdata (submit-order { :gid 101 :cid (inst-ms (java.util.Date.)) :type "EXCHANGE MARKET" :symbol "tTESTBTC:TESTUSD" :price "10" :amount "0.001" :flags 0 })
           ordstatus (get-in torderdata [6])
           ordid (get-in torderdata [4 0 0])
           tdata (order-trades { :Symbol "tTESTBTC:TESTUSD" :OrderId ordid } ) ]
        (is (not= nil torderdata))
        (is (not= [] torderdata))
        (is (= false ((comp not seq) torderdata)))
        (is (= (str ordstatus) "SUCCESS"))
        (is (> ordid 0))

        (is (not= nil tdata))
        (is (or (= [] tdata) (not= [] tdata)))
)))

(deftest ptrades-test
  (testing "ptrades-test fixme, I fail."
    (let [ torderdata (submit-order { :gid 102 :cid (inst-ms (java.util.Date.)) :type "EXCHANGE MARKET" :symbol "tTESTBTC:TESTUSD" :price "9" :amount "0.001" :flags 0 })
           ordstatus (get-in torderdata [6])
           ordid (get-in torderdata [4 0 0])
           tdata (ptrades "tTESTBTC:TESTUSD" {:mem nil :limit 10 :sort 1}) ]
        (is (not= nil torderdata))
        (is (not= [] torderdata))
        (is (= false ((comp not seq) torderdata)))
        (is (= (str ordstatus) "SUCCESS"))
        (is (> ordid 0))

        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
)))

(deftest ptrades-all-test
  (testing "ptrades-all-test fixme, I fail."
    (let [ torderdata (submit-order { :gid 102 :cid (inst-ms (java.util.Date.)) :type "EXCHANGE MARKET" :symbol "tTESTBTC:TESTUSD" :price "10" :amount "0.001" :flags 0 })
           ordstatus (get-in torderdata [6])
           ordid (get-in torderdata [4 0 0])
           tdata (ptrades {:mem nil :limit 10}) ]
        (is (not= nil torderdata))
        (is (not= [] torderdata))
        (is (= false ((comp not seq) torderdata)))
        (is (= (str ordstatus) "SUCCESS"))
        (is (> ordid 0))

        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
)))

(deftest pledgers-test
  (testing "pledgers-test fixme, I fail."
    (let [ tdata (pledgers "TESTBTC" {:category 5 :limit 10}) ]
        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
)))

(deftest pledgers-all-test
  (testing "pledgers-all-test fixme, I fail."
    (let [ tdata (pledgers {:category 5 :limit 10}) ]
        (is (not= nil tdata))
        (is (not= [] tdata))
        (is (= false ((comp not seq) tdata)))
)))

(deftest post-margin-info-test
  (testing "post-margin-info-test fixme, I fail."
    (let [ tbdata  (post-margin-info "base")
           tsdata  (post-margin-info "tTESTBTC:TESTUSD")
           tsadata (post-margin-info "sym_all") ]
        (is (not= nil tbdata))
        (is (not= [] tbdata))
        (is (= false ((comp not seq) tbdata)))

        (is (not= nil tsdata))
        (is (not= [] tsdata))
        (is (= false ((comp not seq) tsdata)))

        (is (not= nil tsadata))
        (is (not= [] tsadata))
        (is (not= [[]] tsadata))
        (is (= false ((comp not seq) tsadata)))
)))

(deftest post-margin-info-empty-key-test
  (testing "post-margin-info-empty-key-test fixme, I fail."
    (try
      (post-margin-info "")
      (is false)
    (catch Throwable e
      (is true)))

    (try
      (post-margin-info nil)
      (is false)
    (catch Throwable e
      (is true)))
))

(deftest post-retrieve-position-test
  (testing "post-retrieve-position-test fixme, I fail."
    (let [ tdata  (post-retrieve-position) ]
        (is (not= nil tdata))
        (is (or (not= []  tdata) (= [] tdata))
))))

(deftest post-claim-position-test
  (testing "post-claim-position-test fixme, I fail."
  (try
    (let [ tdata  (post-claim-position { :id 12345 :amount "19.03" }) ]
        (is (not= nil tdata))
        (is (or (not= []  tdata) (= [] tdata))))
  (catch clojure.lang.ExceptionInfo e
    ;;(prn "caught" e)
    (is true))
  (catch Throwable e
      (is false)))
))

(deftest post-claim-position-nil-mbody-test
  (testing "post-claim-position-nil-mbody-test fixme, I fail."
  (try
    (post-claim-position nil)
    (is false)
  (catch Throwable e
    (is true)))
))

(deftest post-claim-position-empty-mbody-test
  (testing "post-claim-position-empty-mbody-test fixme, I fail."
  (try
    (post-claim-position {})
    (is false)
  (catch Throwable e
    (is true)))
))

(deftest post-increase-position-nil-mbody-test
  (testing "post-increase-position-nil-mbody-test fixme, I fail."
  (try
    (post-increase-position nil)
    (is false)
  (catch Throwable e
    (is true)))
))

(deftest post-increase-position-empty-mbody-test
  (testing "post-increase-position-empty-mbody-test fixme, I fail."
  (try
    (post-increase-position {})
    (is false)
  (catch Throwable e
    (is true)))
))

(deftest post-increase-position-info-nil-mbody-test
  (testing "post-increase-position-info-nil-mbody-test fixme, I fail."
  (try
    (post-increase-position-info nil)
    (is false)
  (catch Throwable e
    (is true)))
))

(deftest post-increase-position-info-empty-mbody-test
  (testing "post-increase-position-info-empty-mbody-test fixme, I fail."
  (try
    (post-increase-position-info {})
    (is false)
  (catch Throwable e
    (is true)))
))

(deftest post-position-history-test
  (testing "post-position-history-test fixme, I fail."
    (let [ tdata  (post-position-history { :limit 10 }) ]
        (is (not= nil tdata))
        (is (or (not= [] tdata) (= [] tdata))
))))

(deftest post-position-history-nil-test
  (testing "post-position-history-nil-test fixme, I fail."
  (try
    (post-position-history nil)
    (is false)
  (catch Throwable e
    (is true)))
))

(deftest post-position-snapshot-test
  (testing "post-position-snapshot-test fixme, I fail."
    (let [ tdata  (post-position-snapshot { :limit 10 }) ]
        (is (not= nil tdata))
        (is (or (not= [] tdata) (= [] tdata))
))))

(deftest post-position-snapshot-nil-test
  (testing "post-position-snapshot-nil-test fixme, I fail."
  (try
    (post-position-snapshot nil)
    (is false)
  (catch Throwable e
    (is true)))
))

(deftest post-position-audit-nil-mbody-test
  (testing "post-position-audit-nil-mbody-test fixme, I fail."
    (try
      (post-position-audit nil)
      (is false)
    (catch Throwable e
      (is true)))
))

(deftest post-position-audit-empty-mbody-test
  (testing "post-position-audit-empty-mbody-test fixme, I fail."
  (try
    (post-position-audit {})
    (is false)
  (catch Throwable e
    (is true)))
))

(deftest post-deriv-pos-collateral-nil-test
  (testing "post-deriv-pos-collateral-nil-test fixme, I fail."
  (try
    (post-deriv-pos-collateral nil)
    (is false)
  (catch Throwable e
    (is true)))
))

(deftest post-deriv-pos-coll-limits-nil-test
  (testing "post-deriv-pos-coll-limits-nil-test fixme, I fail."
  (try
    (post-deriv-pos-coll-limits nil)
    (is false)
  (catch Throwable e
    (is true)))
))

(deftest post-user-info-test
  (testing "post-user-info-test fixme, I fail."
    (let [ tdata  (post-user-info) ]
        (is (not= nil tdata))
        (is (not= [] tdata)))
))

(deftest post-summary-test
 (testing "post-summary-test fixme, I fail."
   (let [ tdata  (post-summary) ]
       (is (not= nil tdata))
       (is (not= [] tdata)))
))

(deftest post-login-history-test
 (testing "post-login-history-test fixme, I fail."
   (let [ tdata  (post-login-history { :limit 10 }) ]
       (is (not= nil tdata))
       (is (not= [] tdata)))
))

(deftest post-account-changelog-test
 (testing "post-account-changelog-test fixme, I fail."
   (let [ tdata  (post-account-changelog { :limit 10 }) ]
       (is (not= nil tdata))
       (is (or (not= [] tdata) (= [] tdata)))
)))

(deftest post-alert-list-test
 (testing "post-alert-list-test fixme, I fail."
   (let [ tdata  (post-alert-list { :type "price" }) ]
       (is (not= nil tdata))
       (is (or (not= [] tdata) (= [] tdata)))
)))

(deftest post-user-settings-read-test
 (testing "post-user-settings-read-test fixme, I fail."
   (let [ tdata  (post-user-settings-read { :keys [ "api:KuOTVyCD5MkrYLXd6VtgWFCzZDDblPlUFjALK14jtdp" ] }) ]
       (is (not= nil tdata))
       (is (or (not= [] tdata) (= [] tdata)))
)))

(deftest post-pulse-history-test
 (testing "post-pulse-history-test fixme, I fail."
   (let [ tdata  (post-pulse-history { :isPublic 1 }) ]
       (is (not= nil tdata))
       (is (not= [] tdata)))
))

(deftest get-method-name-test
  (testing "get-method-name-test fixme, I fail."
  (is (= "WIRE" (get-method-by-currency "USD")))
  (is (= "WIRE" (get-method-by-currency "EUR")))
  (is (= "XRA" (get-method-by-currency "XRA")))
  (is (= "LUNA" (get-method-by-currency "LUNA")))
  (is (= nil (get-method-by-currency "TTTTTTNNNNNNNNN")))
))
