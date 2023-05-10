(use-modules (rnrs))

(define-module (market)
  #: export (make-market
             make-trade
             make-user
             trade-cost
             trade-price
             market-price
             kelly
             buy?
             brier))

(define (make-market outcome-1-quantity outcome-2-quantity liquidity)
  ;; A market is a prediction about an event
  (let ((market (make-hash-table)))
    (hash-set! market 'outcome-1-quantity outcome-1-quantity)
    (hash-set! market 'outcome-2-quantity outcome-2-quantity)
    (hash-set! market 'liquidity liquidity)
    market))

(define (make-trade outcome direction quantity)
  ;; A trade is a bet on an outcome
  (let ((trade (make-hash-table)))
    (hash-set! trade 'outcome outcome)
    (hash-set! trade 'direction direction)
    (hash-set! trade 'quantity quantity)
    trade))

(define (make-user bankroll)
  ;; A user is a person who is making trades
  (let ((user (make-hash-table)))
    (hash-set! user 'bankroll bankroll)
    (hash-set! user 'predictions (make-hash-table))
    user))

(define (trade-cost liquidity quantity quantity-other)
  ;; The amount of money traders have collectively spent so far
  (* liquidity (log (+ (exp (/ quantity liquidity)) (exp (/ quantity-other liquidity))))))

(define (trade-price prediction trade)
  ;; Given your the trade you want to make (how much you want to move the market by)
  ;; This is how much it will cost you
  (let*-values ([ (liquidity) (cdr (hash-get-handle prediction 'liquidity)) ]
                [ (direction) (cdr (hash-get-handle trade 'direction)) ]
                [ (trade-quantity) (cdr (hash-get-handle trade 'quantity)) ]
                [ (old-quantity old-quantity-other)
                               (if (eq? (cdr (hash-get-handle trade 'outcome)) 'outcome-1)
                                   (values (cdr (hash-get-handle prediction 'outcome-1-quantity)) (cdr (hash-get-handle prediction 'outcome-2-quantity)))
                                   (values (cdr (hash-get-handle prediction 'outcome-2-quantity)) (cdr (hash-get-handle prediction 'outcome-1-quantity)))) ]
                [ (new-quantity) ((if (eq? direction 'buy) + -) old-quantity trade-quantity) ])
    (- (trade-cost liquidity new-quantity old-quantity-other)
       (trade-cost liquidity old-quantity old-quantity-other))))

(define (market-price market)
  ;; The current market price is the probability of the event
  (let* ([ outcome-1-quantity (cdr (hash-get-handle market 'outcome-1-quantity)) ]
         [ outcome-2-quantity (cdr (hash-get-handle market 'outcome-2-quantity)) ]
         [ liquidity (cdr (hash-get-handle market 'liquidity)) ]
         [ outcome-1-cost (exp (/ outcome-1-quantity liquidity)) ]
         [ outcome-2-cost (exp (/ outcome-2-quantity liquidity)) ])
    (/ outcome-1-cost (+ outcome-1-cost outcome-2-cost))))

(define (kelly p price)
  ;; calculate trade using kelly criterion (returns a fraction of the bankroll to bet)
  ;; p is probability of winning
  ;; b is ((1 - price) / price) (market probability/how much you win)
  ;; q is (1 - p) (probability of losing)
  ;; ((b * p) - q) / b
  (let ([ b (/ (- 1 price) price) ]
        [ q (- 1 p) ])
    (/ (- (* b p) q) b)))

(define (buy? user estimate market)
  ;; If probability is greater than price, then we're buying outcome 1
  ;; If probability is less than price, then we're buying outcome 2
  ;; Make a kelly bet based on the probability and price
  (let* ([price (market-price market)]
         [ bet (kelly estimate price) ])
    (cond
     ((= estimate price) #f)
     ((> estimate price) (make-trade 'outcome-1 'buy (* bet (cdr (hash-get-handle user 'bankroll)))))
     ((< estimate price) (make-trade 'outcome-2 'buy (* bet (cdr (hash-get-handle user 'bankroll))))))))

(define (brier estimates outcomes)
  ;; estimates between 0 and 1, outcomes always 0 or 1
  ;; This is also known as "mean squared error"
  ;; lower is better
  (let ([ n (length estimates) ])
    (/ (apply + (map (lambda (estimate outcome) (expt (- estimate outcome) 2)) estimates outcomes)) n)))
