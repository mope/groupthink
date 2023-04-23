(use-modules (srfi srfi-64)
             (main))

(test-begin "market-price")

(test-assert "test-market-price-50-50"
  (let ([ market (make-market 50 50 100) ])
    (= 0.5 (market-price market))))

(test-assert "test-market-price-75"
  (let ([ market (make-market 25 75 100) ])
    (= 0.3775406687981454 (market-price market))))

(test-assert "test-market-price-25-75"
  (let ([ market (make-market 25 75 100) ])
    (= 0.3775406687981454 (market-price market))))

(test-assert "test-market-price-75-25"
  (let ([ market (make-market 75 25 100) ])
    (= 0.6224593312018546 (market-price market))))

(test-end "market-price")

(test-begin "trade-price")

(test-assert "test-trade-price-buy"
  (let ([ market (make-market 0 0 100) ]
        [ trade (make-trade 'outcome-1 'buy 10) ])
    (= 5.124947951362557 (trade-price market trade))))

(test-assert "test-trade-price-sell"
  (let ([ market (make-market 10 0 100) ]
        [ trade (make-trade 'outcome-1 'sell 10) ])
    (= -5.124947951362557 (trade-price market trade))))

(test-end "trade-price")

(test-begin "buy?")

(let* ([ user (make-user 100) ]
       [ market (make-market 50 50 100) ]
       [ recommendation (buy? user 0.6 market) ])
  (test-assert "test-buy?-direction"
    (eq? 'buy (cdr( hash-get-handle recommendation 'direction))))
  (test-assert "test-buy?-outcome"
    (eq? 'outcome-1 (cdr( hash-get-handle recommendation 'outcome))))
  (test-assert "test-buy?-quantity"
    (= 19.999999999999996 (cdr( hash-get-handle recommendation 'quantity)))))

(test-end "buy?")

(test-begin "kelly")

(test-assert "kelly-60-10"
  (= 0.19999999999999996 (kelly 0.6 0.5)))

(test-end "kelly")

(test-begin "brier")

(test-assert "brier"
  (= 0.4886610762705407 (brier '(0.61612786 0.00234133 0.50031398 0.04890813 0.34880641 0.18529963
  0.63597477 0.08505006 0.96671728 0.03728153) '(0 1 0 0 0 0 0 1 0 1))))

(test-end "brier")
