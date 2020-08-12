#lang racket
(require dyoo-while-loop)
(define month 0) ;the current month
(define accountTypes '()) ;list of account types. Each account type is a list of appropriately ordered parameters.
(define loanTypes '()) ;list of loan types. Each loan type is a list of appropriately ordered parameters.
(define customers '()) ;list of customers, each customer is a list of 3 elements. The 1st element is his id. The 2nd element is
;the list of his accounts (apparently it can only be one) and the 3rd element is the list of his loans. Each account (apparently
;THE account) has the elements in the order of account type, current balance, interest rate, minimum balance of the year (for annual
;interest), period passed from contract, credit, span counter (months that the balance has not been decreased by the customer), last loan
;in months, last loan amount, blocked money, credit counter, new interest rate. Each loan has the elements in the order of loan type,
;amount left, months passed, and due date.
(define confirmedLoans '()) ;the monthly requests for loans
(define out "")
(define extractNumbers ;gets a string and returns a list of numbers inside
  (lambda (str)
    (begin
      (define l 0)
      (define numbers '())
      (define temp "")
      (while (< l (string-length str))
             (begin
               (if (char-numeric? (string-ref str l))
                   ;then
                   (begin
                     (set! temp (string-append temp (make-string 1 (string-ref str l))))
                     (set! l (add1 l))
                     )
                   ;else
                   (begin
                     (if (equal? temp "")
                         ;then
                         (set! l (add1 l))
                         ;else
                         (begin
                           (set! numbers (append numbers (cons (string->number temp) '())))
                           (set! l (add1 l))
                           (set! temp "")
                           )
                         )
                     
                     )
                   )
               )
             )
      (if (char-numeric? (string-ref str (- l 1)))
          ;then
          (begin
            (set! numbers (append numbers (cons (string->number temp) '())))
            (append numbers '())
            )
          ;else
          (append numbers '())
          )
      )
    )
  )
(define createAccount ;gets the customer id (cusId), the account type (accId), and starting money (amount) and returns 1 if possible
  ;and -1 if not
  (lambda (cusId accId amount)
    (let ((i 0) (tempAccount '()) (tempCustomer '()))
      ;finds which account is this
      (while (and (< i (length accountTypes)) (not (equal? accId (first (list-ref accountTypes i)))))
             (begin
               (set! i (add1 i))
               )
             )
      ;the minimum amount you need to create an account of this type
      (if (and (< i (length accountTypes)) (>= amount (+ (third (list-ref accountTypes i)) (fourth (list-ref accountTypes i)))))
          ;then
          (begin
            (set! tempCustomer (cons cusId '()))
            (set! tempAccount (cons accId '()))
            (set! tempAccount (append tempAccount (cons (- amount (third (list-ref accountTypes i))) '()))) ;balance
            (set! tempAccount (append tempAccount (cons (eighth (list-ref accountTypes i)) '()))) ;interest rate
            (set! tempAccount (append tempAccount (cons (- amount (third (list-ref accountTypes i))) '()))) ;minimum balance of the year
            (set! tempAccount (append tempAccount (cons '0 '()))) ;period passed
            (set! tempAccount (append tempAccount (cons '0 '()))) ;credit
            (set! tempAccount (append tempAccount (cons '0 '()))) ;counter
            (set! tempAccount (append tempAccount (cons (- amount (third (list-ref accountTypes i))) '()))) ;intial balance
            (set! tempAccount (append tempAccount (cons '-1 '()))) ;last loan month
            (set! tempAccount (append tempAccount (cons '-1 '()))) ;last loan amount
            (set! tempAccount (append tempAccount (cons '0 '()))) ;blocked money
            (set! tempAccount (append tempAccount (cons '0 '()))) ;credit counter
            (set! tempAccount (append tempAccount (cons (eighth (list-ref accountTypes i)) '()))) ;new interest rate for yearly accounts
            (set! tempCustomer (append tempCustomer (cons tempAccount '()) '(())))
            ;(display tempCustomer)
            (set! customers (append customers (cons tempCustomer '())))
            '1
            )
          ;else
          '-1
          )
      )
    )
  )
(define addToAccount ;gets the customer ID and the money amount and adds this amount to his account. Returns 1 if possible and -1 if not
  (lambda (cusId amount)
    (begin
       (let ((i 0) (tempAccount '()) (tempCustomer '()))
         ;finds which customer is this
         (while (and (< i (length customers)) (not (equal? cusId (first (list-ref customers i)))))
                (begin
                  (set! i (add1 i))
                  )
                )
         (if (< i (length customers))
             ;then update the balance
             (begin
               (set! tempAccount (list-set (second (list-ref customers i)) 1 (+ (second (second (list-ref customers i))) amount)))
               (set! tempCustomer (list-set (list-ref customers i) 1 tempAccount))
               (set! customers (list-set customers i tempCustomer))
               '1
               )
              ;else
              '-1
              )
         )
       )
    )
  )
(define renewal ;renew the contract of the customer and returns 1 if possible and -1 if not
  (lambda (cusId)
    (let ((i 0) (tempAccount '()) (tempCustomer '()) (tempType 0) (j 0))
      ;finds which customer is this
      (while (and (< i (length customers)) (not (equal? cusId (first (list-ref customers i)))))
             (begin
               (set! i (add1 i))
               )
             )
      ;finds which account is this
      (while (and (< j (length accountTypes)) (not (equal? (first (second (list-ref customers i))) (first (list-ref accountTypes j)))))
             (begin
               (set! j (add1 j))
               )
             )
      (if (and (< i (length customers)) (equal? (fifth (second (list-ref customers i))) (sixth (list-ref accountTypes j))))
          ;then
          (begin
            (set! tempAccount (list-set (second (list-ref customers i)) 4 0))
            (set! tempCustomer (list-set (list-ref customers i) 1 tempAccount))
            (set! customers (list-set customers i tempCustomer))
            '1
            )
          ;else
          '-1
          )
      )
    )
  )
(define cheque ;gets customer Id and the money amount and returns 1 if possible, 0 if not and -1 if forbidden
  (lambda (cusId amount)
         (let ((i 0) (tempAccount '()) (tempCustomer '()) (tempType 0) (j 0))
           ;finds which customer is this
           (while (and (< i (length customers)) (not (equal? cusId (first (list-ref customers i)))))
             (begin
               (set! i (add1 i))
               )
             )
           (if (< i (length customers))
               ;then customer exists
               (begin
                 ;finds which account is this
                 (set! tempType (first (second (list-ref customers i))))
                 (while (and (< j (length accountTypes)) (not (equal? tempType (first (list-ref accountTypes j)))))
                        (begin
                          (set! j (add1 j))
                          )
                        )
                 (if (list-ref (list-ref accountTypes j) 12)
                     ;then cheque
                     (begin
                       (if (>= (- (second (second (list-ref customers i))) amount) ;balance - amount
                               (+ (list-ref (second (list-ref customers i)) 10) (fourth (list-ref accountTypes j)))) ;>= blc + min deposit
                           ;then possible
                           (begin
                             (set! tempAccount (list-set (second (list-ref customers i)) 1 (- (second (second (list-ref customers i))) amount)))
                             (set! tempAccount (list-set tempAccount 6 0)) ;resets span counter
                             (set! tempCustomer (list-set (list-ref customers i) 1 tempAccount))
                             (set! customers (list-set customers i tempCustomer))
                             '1
                             )
                           ;else not enough balance
                           '0
                           )
                       )
                     ;else illegal
                     (begin
                       (set! tempAccount (list-set (second (list-ref customers i)) 5 (- (sixth (second (list-ref customers i)))(quotient (ninth (list-ref accountTypes j)) 2))))
                       (set! tempCustomer (list-set (list-ref customers i) 1 tempAccount))
                       (set! customers (list-set customers i tempCustomer))
                       '-1
                       )
                     )
                 )
               ;else
               '0
               )
           )
         )
  )
(define card ;gets customer Id and the money amount and returns 1 if possible, 0 if not and -1 if forbidden
  (lambda (cusId amount)
    (let ((i 0) (tempAccount '()) (tempCustomer '()) (tempType 0) (j 0))
      ;finds which customer is this
      (while (and (< i (length customers)) (not (equal? cusId (first (list-ref customers i)))))
             (begin
               (set! i (add1 i))
               )
             )
      (if (< i (length customers))
          ;then customer exists
          (begin
            ;finds which account is this
            (set! tempType (first (second (list-ref customers i))))
            (while (and (< j (length accountTypes)) (not (equal? tempType (first (list-ref accountTypes j)))))
                   (begin
                     (set! j (add1 j))
                     )
                   )
            (if (< (fifth (second (list-ref customers i))) (sixth (list-ref accountTypes j)))
                ;then period left?
                (if (list-ref (list-ref accountTypes j) 13)
                    ;then has card
                    (if (>= (- (second (second (list-ref customers i))) amount) ;balance - amount
                            (+ (list-ref (second (list-ref customers i)) 10) (fourth (list-ref accountTypes j)))) ;>= blc + min deposit
                        ;then possible
                        (begin
                             (set! tempAccount (list-set (second (list-ref customers i)) 1 (- (second (second (list-ref customers i))) amount)))
                             (set! tempAccount (list-set tempAccount 6 0)) ;resets span counter
                             (set! tempCustomer (list-set (list-ref customers i) 1 tempAccount))
                             (set! customers (list-set customers i tempCustomer))
                             '1
                             )
                        ;else not enough balance
                        '0
                        )
                    ;else illegal: decrease credit
                    (begin
                       (set! tempAccount (list-set (second (list-ref customers i)) 5 (- (sixth (second (list-ref customers i)))(quotient (ninth (list-ref accountTypes j)) 2))))
                       (set! tempCustomer (list-set (list-ref customers i) 1 tempAccount))
                       (set! customers (list-set customers i tempCustomer))
                       '-1
                       )
                    )
                ;else no contract
                (if (ninth (list-ref accountTypes j))
                    ;then has card: no need to be greater than minimum deposit
                    (if (>= (second (second (list-ref customers i))) amount)
                        ;then possible
                        (begin
                             (set! tempAccount (list-set (second (list-ref customers i)) 1 (- (second (second (list-ref customers i))) amount)))
                             (set! tempAccount (list-set tempAccount 6 -1)) ;resets span counter
                             (set! tempCustomer (list-set (list-ref customers i) 1 tempAccount))
                             (set! customers (list-set customers i tempCustomer))
                             '1
                             )
                        ;else not enough balance
                        '0
                        )
                    ;else no card: no need to decrease the credit
                    '0
                    )
                )
            )
          ;else
          '0
          )
      )
    )
  )
(define closeAccount ;gets the customer ID and returns 1 if possible and -1 if not
  (lambda (cusId)
    (let ((i 0))
      ;finds which customer is this
      (while (and (< i (length customers)) (not (equal? cusId (first (list-ref customers i)))))
             (begin
               (set! i (add1 i))
               )
             )
      (if (< i (length customers))
          ;then the customer exists: delete
          (begin
            (set! customers (append (take customers i) (drop customers (+ i 1))))
            '1
            )
          ;else
          '-1
          )
      )
    )
  )
(define checkLoan ;checks the loan for the customer and returns 1 if possible and -1 if not
  (lambda (cusId loanId)
    (let ((i 0) (j 0) (k 0) (tempAccount '()) (tempCustomer '()) (tempRequest '()))
      ;finds which customer is this
      (while (and (< i (length customers)) (not (equal? cusId (first (list-ref customers i)))))
             (begin
               (set! i (add1 i))
               )
             )
      ;finds which account is this
      (while (and (< j (length customers)) (not (equal? (first (second (list-ref customers i))) (first (list-ref accountTypes j)))))
             (begin
               (set! j (add1 j))
               )
             )
      ;finds which loan is this
      (while (and (< k (length loanTypes)) (not (equal? loanId (first (list-ref loanTypes k)))))
             (begin
               (set! k (add1 k))
               )
             )
      ;check conditions
      (if (and (>= (sixth (second (list-ref customers i))) (seventh (list-ref loanTypes k))) ;enough credit
               (>= (- (second (second (list-ref customers i))) (list-ref (second (list-ref customers i)) 10)) ;balance - bolcked money
                   (+ (third (list-ref loanTypes k)) (fourth (list-ref accountTypes j)))) ; >= blocking money + min deposit; enough balance
               (or (equal? -1 (tenth (second (list-ref customers i)))) ;no loans before
                   (>= (- month (ninth (second (list-ref customers i)))) (sixth (list-ref loanTypes k))) ;enough period since last loan
                   )
               )
          ;then
          (begin
            (set! tempRequest (cons cusId (cons loanId tempRequest)))
            (set! confirmedLoans (append confirmedLoans (cons tempRequest '())))
            '1
            )
          ;else
          '-1
          )
      )
    )
  )
(define addLoan ;adds the loan for the customer and returns 1 if possible and -1 if not
  (lambda (cusId loanId)
    (let ((i 0) (j 0) (k 0) (tempAccount '()) (tempCustomer '()) (tempLoan '()) (l 0))
      ;finds which customer is this
      (while (and (< i (length customers)) (not (equal? cusId (first (list-ref customers i)))))
             (begin
               (set! i (add1 i))
               )
             )
      ;finds which account is this
      (while (and (< j (length customers)) (not (equal? (first (second (list-ref customers i))) (first (list-ref accountTypes j)))))
             (begin
               (set! j (add1 j))
               )
             )
      ;finds which loan is this
      (while (and (< k (length loanTypes)) (not (equal? loanId (first (list-ref loanTypes k)))))
             (begin
               (set! k (add1 k))
               )
             )
      ;then adds loan
      (begin
        (set! tempAccount (list-set (second (list-ref customers i)) 5 (- (sixth (second (list-ref customers i))) (seventh (list-ref loanTypes k))))) ;decreases credit
        (set! tempAccount (list-set tempAccount 1 (+ (second tempAccount) (second (list-ref loanTypes k))))) ;adds this amount to balance
        (set! tempAccount (list-set tempAccount 10 (+ (list-ref tempAccount 10) (third (list-ref loanTypes k))))) ;updates blocked money
        (set! tempAccount (list-set tempAccount 8 month)) ;sets last loan to this month
        (set! tempAccount (list-set tempAccount 9 (second (list-ref loanTypes k)))) ;sets last loan amount to this amount
        (set! tempLoan (cons loanId (cons (floor (/ (* (second (list-ref loanTypes k)) (+ 100 (fifth (list-ref loanTypes k)))) 100))
                       (cons 0 (cons (+ month (fourth (list-ref loanTypes k)))
                       (cons #f '())))))) ;(loanType amountLeft monthsPassed due withdrawn)
        (set! tempCustomer (list-set (list-ref customers i) 1 tempAccount))
        ;finds where to put the loan in order to sort them by their due dates
        (while (and (< l (length (third tempCustomer))) 
                    (> (fourth tempLoan) (fourth (list-ref (third tempCustomer) l)))
                    )
               (set! l (add1 l))
               )
        (set! tempCustomer (list-set tempCustomer 2 (append (take (third tempCustomer) l) (cons tempLoan (drop (third tempCustomer) l)))))
        (set! customers (list-set customers i tempCustomer))
        )
      )
    )
  )
(define loanPayed ;deletes the first loan of the customer and returns 1 if possible and -1 if not
  (lambda (cusId)
    (let ((i 0) (j 0) (k 0) (tempAccount '()) (tempCustomer '()) (tempLoan '()))
      ;finds which customer is this
      (while (and (< i (length customers)) (not (equal? cusId (first (list-ref customers i)))))
             (begin
               (set! i (add1 i))
               )
             )
      ;finds which loan is the first one
      (while (and (< k (length loanTypes)) (not (equal? (first (first (third (list-ref customers i)))) (first (list-ref loanTypes k)))))
             (begin
               (set! k (add1 k))
               )
             )
      ;update blocked money
      (set! tempAccount (list-set (second (list-ref customers i)) 10 (- (list-ref (second (list-ref customers i)) 10) (third (list-ref loanTypes k)))))      
      (set! tempCustomer (list-set (list-ref customers i) 1 tempAccount))
      (set! tempCustomer (list-set tempCustomer 2 (rest (third tempCustomer)))) ;update loans of the customer
      (set! customers (list-set customers i tempCustomer))
      )
    )
  )
(define payDebt ;gets customer ID and amount and reduces it from debts and returns 1 if possible and -1 if not
  (lambda (cusId amount)
    (let ((i 0) (j 0) (k 0) (tempAccount '()) (tempCustomer '()) (tempLoan '()))
      ;finds which customer is this
      (while (and (< i (length customers)) (not (equal? cusId (first (list-ref customers i)))))
             (begin
               (set! i (add1 i))
               )
             )
      ;finds which account is this
      (while (and (< j (length customers)) (not (equal? (first (second (list-ref customers i))) (first (list-ref accountTypes j)))))
             (begin
               (set! j (add1 j))
               )
             )
      (if (< i (length customers))
          ;then customer exists
          (if (and (not (null? (third (list-ref customers i)))) ;has at least one loan
                   (>= (- (second (second (list-ref customers i))) amount) ;balance - amount
                      (+ (list-ref (second (list-ref customers i)) 10) (fourth (list-ref accountTypes j))) ;>= blc + min deposit
                      )
                   )
              ;then it is possible to pay debt
              (begin
                (set! tempAccount (list-set (second (list-ref customers i)) 1 (- (second (second (list-ref customers i))) amount))) ;bal -= amount
                ;update Customer
                (set! tempCustomer (list-set (list-ref customers i) 1 tempAccount))
                (set! customers (list-set customers i tempCustomer))
                (while (and (< k (length (third (list-ref customers i)))) ;until at least one loan exists
                            (> amount 0) ;there is still some amount left
                            )
                       (begin
                         (if (>= amount (second (list-ref (third (list-ref customers i)) k)))
                             ;then loan can be payed
                             (begin
                               (set! amount (- amount (second (list-ref (third (list-ref customers i)) k)))) ;update amount
                               (loanPayed (first (list-ref customers i))) ;loan payed
                               (set! tempCustomer (list-ref customers i))
                               )
                             ;else just reduces the amount from the first loan
                             (begin
                               (set! tempLoan (list-set (first (third (list-ref customers i))) 1 (- (second (first (third (list-ref customers i)))) amount)))
                               (set! tempCustomer (list-set (list-ref customers i) 2 (cons tempLoan (rest (third (list-ref customers i))))))
                               (set! amount 0)
                               )
                             )
                         )
                       )
                (set! tempAccount (list-set (second (list-ref customers i)) 1 (+ (second (second (list-ref customers i))) amount))) ;bal += amount
                (set! tempCustomer (list-set tempCustomer 1 tempAccount))
                (set! customers (list-set customers i tempCustomer))
                )
              ;else
              '-1
              )
          ;else
          '-1
          )
      )
    )
  )
(define withdrawLoan ;gets customer ID and reduces the loan amount from balance and returns 1 if possible and -1 if not
  (lambda (cusId)
    (let ((i 0) (j 0) (k 0) (tempAccount '()) (tempCustomer '()) (tempLoan '()) (l 0))
      ;finds which customer is this
      (while (and (< i (length customers)) (not (equal? cusId (first (list-ref customers i)))))
             (begin
               (set! i (add1 i))
               )
             )
      ;finds which account is this
      (while (and (< j (length customers)) (not (equal? (first (second (list-ref customers i))) (first (list-ref accountTypes j)))))
             (begin
               (set! j (add1 j))
               )
             )
      (if (and (< i (length customers)) ;customer exists
               (> (length (third (list-ref customers i))) 0) ;has at least one loan
               )
          ;then
          (begin
            (while (and (< k (length (third (list-ref customers i)))) (fifth (list-ref (third (list-ref customers i)) k)))
                   (set! k (add1 k))
            )
            (if (< k (length (third (list-ref customers i))))
                ;then it is possible withdraw loan
                (begin
                  ;finds which loan is this
                  (while (and (< l (length customers)) (not (equal? (first (list-ref (third (list-ref customers i)) k)) (first (list-ref loanTypes l)))))
                         (begin
                           (set! l (add1 l))
                           )
                         )
                  (if (>= (- (second (second (list-ref customers i))) (second (list-ref loanTypes l))) ;balance - loan amount
                          (+ (list-ref (second (list-ref customers i)) 10) (fourth (list-ref accountTypes j))) ;>= blc + min deposit
                          )
                      ;then withdraws the loan
                      (begin
                        (set! tempAccount (list-set (second (list-ref customers i)) 1
                              (- (second (second (list-ref customers i))) (second (list-ref loanTypes l))))) ;updates balance
                        (set! tempLoan (list-set (list-ref (third (list-ref customers i)) k) 4 #t))
                        (set! tempCustomer (list-set (list-ref customers i) 2
                              (append (take (third (list-ref customers i)) k) (cons tempLoan (drop (third (list-ref customers i)) (+ k 1))))))
                        (set! tempCustomer (list-set tempCustomer 1 tempAccount))
                        (set! customers (list-set customers i tempCustomer))
                        )
                      ;else
                      '-1
                      )
                  )
                ;else
                '-1
                )
            )
          ;else
          '-1
          )
      )
    )
  )
(define timeGoesBy ;update the state of the bank at each month
 (lambda ()
  (let ((i 0) (tempAccount '()) (tempCustomer '()) (tempType 0) (j 0) (k 0) (l 0) (tempLoan '()) (loans '()))
    ;month update
    (set! month (add1 month))    
    ;Account updates
    (while (< i (length customers))
           (begin
             ;finds which account type does this customer have
             (while (and (< j (length accountTypes)) (not (equal? (first (second (list-ref customers i))) (first (list-ref accountTypes j)))))
                    (begin
                      (set! j (add1 j))
                      )
                    )
             ;update period passed
             (set! tempAccount (list-set (second (list-ref customers i)) 4 (+ 1 (fifth (second (list-ref customers i))))))
             ;update span counter
             (set! tempAccount (list-set tempAccount 6 (+ 1 (seventh tempAccount))))
             ;update credit counter
             (set! tempAccount (list-set tempAccount 11 (+ 1 (list-ref tempAccount 11))))
             ;update yearly credit
             (if (equal? 0 (remainder (list-ref tempAccount 11) 12))
                 ;then
                 (set! tempAccount (list-set tempAccount 5 (+ (sixth tempAccount) (ninth (list-ref accountTypes j)))))
                 ;else
                 '0
                 )
             ;includes interest
             (if (not (second (list-ref accountTypes j)))
                 ;then
                 (begin
                   ;checks if contract is still valid
                   (if (<= (fifth tempAccount) (sixth (list-ref accountTypes j)))
                       ;then
                       (begin
                         ;checks if variability is reached
                         (if (and (list-ref (list-ref accountTypes j) 9) (equal? (seventh tempAccount) (list-ref (list-ref accountTypes j) 10)))
                             ;then
                             (begin
                               ;adds to credit
                               (set! tempAccount (list-set tempAccount 5 (+ (sixth tempAccount) (ninth (list-ref accountTypes j)))))
                               ;adds to interest
                               (if (fifth (list-ref accountTypes j))
                                   ;then monthly
                                   (set! tempAccount (list-set tempAccount 2 (+ (third tempAccount) (list-ref (list-ref accountTypes j) 11))))
                                   ;else yearly
                                   (set! tempAccount (list-set tempAccount 12 (+ (third tempAccount) (list-ref (list-ref accountTypes j) 11))))
                                   )                               
                               ;resets the span counter
                               (set! tempAccount (list-set tempAccount 6 0))
                               )
                             ;else
                             '0
                             )
                         ;update balance
                         (if (fifth (list-ref accountTypes j))
                             ;then monthly
                             (set! tempAccount (list-set tempAccount 1 (+ (second tempAccount) (floor (* (second tempAccount) (/ (/ (third tempAccount) 12) 100))))))
                             ;else yearly
                             (if (equal? 0 (remainder (fifth tempAccount) 12))
                                 ;then at the end of the year
                                 (begin
                                   ;adds to the balance
                                   (set! tempAccount (list-set tempAccount 1 (+ (second tempAccount) (floor (* (fourth tempAccount) (/ (third tempAccount) 100))))))
                                   ;update minimun balance
                                   (set! tempAccount (list-set tempAccount 3 (second tempAccount)))
                                   ;update interest rate
                                   (set! tempAccount (list-set tempAccount 2 (list-ref tempAccount 12)))
                                   )
                                 ;else during the year
                                 (begin
                                   ;update minimum balance
                                   (set! tempAccount (list-set tempAccount 3 (min (second tempAccount) (fourth tempAccount))))
                                   )
                                 )
                             )
                         )
                       ;else
                       '0
                       )
                   )
                 ;else
                 '0
                 )
             ;update Customer
             (set! tempCustomer (list-set (list-ref customers i) 1 tempAccount))
             (set! customers (list-set customers i tempCustomer))
             ;Loan updates
             (while (< k (length (third (list-ref customers i))))
                    ;update months passed
                    (set! tempLoan (list-set (list-ref (third (list-ref customers i)) k) 2
                                   (+ 1 (third (list-ref (third (list-ref customers i)) k))))) ;monthsPassed++
                    ;update Customer
                    (set! loans (list-set (third (list-ref customers i)) k tempLoan))
                    (set! tempCustomer (list-set (list-ref customers i) 2 loans))
                    (set! customers (list-set customers i tempCustomer))
                    ;initialize l
                    (set! l 0)
                    ;finds which loan type is this
                    (while (and (< l (length loanTypes)) (not (equal? (first tempLoan) (first (list-ref loanTypes l)))))
                           (set! l (add1 l))
                           )
                    ;yearly interest
                    (if (equal? 0 (remainder (third tempLoan) 12)) ;monthPassed % 12 == 0
                        ;then adds to the loan amount left
                        (begin
                          (set! tempLoan (list-set tempLoan 1 (+ (second tempLoan) ;amount += 
                              (floor (* (second (list-ref loanTypes l)) (/ (fifth (list-ref loanTypes l)) 100))))));loanAmount*interestRate
                          ;updates Customer
                          (set! loans (list-set (third (list-ref customers i)) k tempLoan))
                          (set! tempCustomer (list-set (list-ref customers i) 2 loans))
                          (set! customers (list-set customers i tempCustomer))
                          )
                        ;else
                        '0
                        )
                    ;reaches the pay due (if it had been payed before, it would be deleted, so it is still not payed completely)
                    (if (equal? month (fourth tempLoan)) ;month == due
                        ;then
                        (begin
                          ;the bank gets the blocked Money
                          (set! tempAccount (list-set (second (list-ref customers i)) 10 (- (list-ref (second (list-ref customers i)) 10) ;blockedMoney -=
                                                                        (third (list-ref loanTypes l))))) ; Blocking Money of the Loan
                          ;sets the credit to zero
                          (set! tempAccount (list-set tempAccount 5 0))
                          ;resets the credit counter
                          (set! tempAccount (list-set tempAccount 11 0))
                          ;deletes the loan
                          (set! loans (rest (third (list-ref customers i))))
                          (set! tempCustomer (list-set (list-ref customers i) 2 loans))
                          (set! tempCustomer (list-set tempCustomer 1 tempAccount))
                          (set! customers (list-set customers i tempCustomer))
                          (set! k (sub1 k))
                          )
                        ;else
                        '0
                        )
                    ;iteration of k; next loan
                    (set! k (add1 k))
                    )
             ;iteration of i; next customer
             (set! i (add1 i))
             (set! j 0)
             (set! k 0)
             )
           )
    ;gives confirmed loans
    (while (< l (length confirmedLoans))
           (addLoan (first (list-ref confirmedLoans l)) (second (list-ref confirmedLoans l)))
           (set! l (add1 l))
           )
    (set! confirmedLoans '())
    )
   )
  )
(define (main textFile)
  ;intialize
  (set! out "")
  (set! accountTypes '())
  (set! loanTypes '())
  (set! customers '())
  (set! month 0)
  (set! confirmedLoans '())
  ;convert text file to a list of strings
  (define lines (file->lines textFile))
  ;split setup and commands section
  (define i 0)
  (define j 0)
  (define setup-list '())
  (define commands-list '())
  (while (< i (length lines))
         (if (string=? (list-ref lines i) "setup") ;if setup section
             ;then
             (begin
               (set! i (add1 i))
               (while (and (< i (length lines)) (not (string=? "commands" (list-ref lines i))))
                      ;save setup info
                      (define thisLine (list-ref lines i))
                      (if (non-empty-string? thisLine) ;deletes the empty lines
                          ;then
                          (begin
                            (set! setup-list (append setup-list (cons (list-ref lines i) '())))
                            (set! i (add1 i))
                            )
                          ;else
                          (set! i (add1 i))
                          )
                      )
               )
             ;else 
             (if (string=? (list-ref lines i) "commands") ;if commands section
                 ;then
                 (begin
                   (set! i (add1 i))
                   (while (and (< i (length lines)) (not (string=? "setup" (list-ref lines i))))
                          ;save commands info
                          (define thisLine (list-ref lines i))
                          (if (non-empty-string? thisLine) ;deletes the empty lines
                              ;then
                              (begin
                                (set! commands-list (append commands-list (cons (list-ref lines i) '())))
                                (set! i (add1 i))
                                )
                              ;else
                              (set! i (add1 i))
                              )
                          )
                   )
                 ;else ;just have to iterate
                 (set! i (add1 i))
                 )
             )
         )
  ;(display setup-list)
  ;analayze the setup info; create the ACCOUNT TYPES & LOAN TYPES
  (set! i 0)
  (define tempAccount '())
  (define tempLoan '())
  (define num '()) ;the output list of numbers of extractNumbers function
  (while (< i (length setup-list))
         (if (string-contains? (list-ref setup-list i) "Account type"); if Account
             ;then
             (begin
               ;account-number?
               (set! num (extractNumbers (list-ref setup-list i)))
               (set! tempAccount num)
               (set! i (add1 i))
               ;current-account?
               (if (string-contains? (list-ref setup-list i) "true")
                   ;then
                   (set! tempAccount (append tempAccount (cons #t '())))
                   ;else
                   (set! tempAccount (append tempAccount (cons #f '())))
                   )
               (set! i (add1 i))
               ;bank-fee?
               (set! num (extractNumbers (list-ref setup-list i)))
               (set! tempAccount (append tempAccount num))
               (set! i (add1 i))
               ;minimum-deposit?
               (set! num (extractNumbers (list-ref setup-list i)))
               (set! tempAccount (append tempAccount num))
               (set! i (add1 i))
               ;monthly?
               (if (string-contains? (list-ref setup-list i) "true")
                   ;then
                   (set! tempAccount (append tempAccount (cons #t '())))
                   ;else
                   (set! tempAccount (append tempAccount (cons #f '())))
                   )
               (set! i (add1 i))
               ;period?
               (set! num (extractNumbers (list-ref setup-list i)))
               (set! tempAccount (append tempAccount num))
               (set! i (add1 i))
               ;renewable?
               (if (string-contains? (list-ref setup-list i) "true")
                   ;then
                   (set! tempAccount (append tempAccount (cons #t '())))
                   ;else
                   (set! tempAccount (append tempAccount (cons #f '())))
                   )
               (set! i (add1 i))
               ;interest-rate?
               (set! num (extractNumbers (list-ref setup-list i)))
               (set! tempAccount (append tempAccount num))
               (set! i (add1 i))
               ;credit?
               (set! num (extractNumbers (list-ref setup-list i)))
               (set! tempAccount (append tempAccount num))
               (set! i (add1 i))
               ;variability?
               (if (string-contains? (list-ref setup-list i) "true")
                   ;then
                   (set! tempAccount (append tempAccount (cons #t '())))
                   ;else
                   (set! tempAccount (append tempAccount (cons #f '())))
                   )
               (set! i (add1 i))
               ;span-for-increase?
               (set! num (extractNumbers (list-ref setup-list i)))
               (set! tempAccount (append tempAccount num))
               (set! i (add1 i))
               ;increase-rate?
               (set! num (extractNumbers (list-ref setup-list i)))
               (set! tempAccount (append tempAccount num))
               (set! i (add1 i))
               ;has-cheque?
               (if (string-contains? (list-ref setup-list i) "true")
                   ;then
                   (set! tempAccount (append tempAccount (cons #t '())))
                   ;else
                   (set! tempAccount (append tempAccount (cons #f '())))
                   )
               (set! i (add1 i))
               ;has-card?
               (if (string-contains? (list-ref setup-list i) "true")
                   ;then
                   (set! tempAccount (append tempAccount (cons #t '())))
                   ;else
                   (set! tempAccount (append tempAccount (cons #f '())))
                   )
               (set! i (add1 i))
               ;transfer-fee?
               (set! num (extractNumbers (list-ref setup-list i)))
               (set! tempAccount (append tempAccount num))
               (set! i (add1 i))
               ;add this account type to accounts
               (set! accountTypes (append accountTypes (cons tempAccount '())))
               )
             ;else LOAN
             (begin
               ;loan number?
               (set! num (extractNumbers (list-ref setup-list i)))
               (set! tempLoan num)
               (set! i (add1 i))
               ;loan-amount?
               (set! num (extractNumbers (list-ref setup-list i)))
               (set! tempLoan (append tempLoan num))
               (set! i (add1 i))
               ;blocking-money?
               (set! num (extractNumbers (list-ref setup-list i)))
               (set! tempLoan (append tempLoan num))
               (set! i (add1 i))
               ;return-span?
               (set! num (extractNumbers (list-ref setup-list i)))
               (set! tempLoan (append tempLoan num))
               (set! i (add1 i))
               ;interest?
               (set! num (extractNumbers (list-ref setup-list i)))
               (set! tempLoan (append tempLoan num))
               (set! i (add1 i))
               ;last-loan?
               (set! num (extractNumbers (list-ref setup-list i)))
               (set! tempLoan (append tempLoan num))
               (set! i (add1 i))
               ;minimum-credit?
               (set! num (extractNumbers (list-ref setup-list i)))
               (set! tempLoan (append tempLoan num))
               (set! i (add1 i))
               ;add this loan type to loans
               (set! loanTypes (append loanTypes (cons tempLoan '())))
               )
             )
         )
  ;(display accountTypes)
  ;(display "\n******************************************************************************\n")
  ;(display loanTypes)
  ;analyze the commands info
  (set! i 0)
  (while (< i (length commands-list))
         (display month)
         (display "\t")
         (cond
           ;Time goes by
           [(string-contains? (list-ref commands-list i) "Time goes by")
            (begin
              (display "Time goes by ")
              (timeGoesBy)
              (display customers)
              (display "\n")
              (set! i (add1 i))
              )
            ]
           ;create an account
           [(string-contains? (list-ref commands-list i) "create an account")
            (begin
              (display "Create account ")
              (set! num (extractNumbers (list-ref commands-list i)))
              (createAccount (first num) (second num) (fourth num))
              (display customers)
              (display "\n")
              (set! i (add1 i))
              )
            ]
           ;add to account
           [(string-contains? (list-ref commands-list i) "add")
            (begin
              (display "Add to the acoount ")
              (set! num (extractNumbers (list-ref commands-list i)))
              (addToAccount (first num) (second num))
              (display customers)
              (display "\n")
              (set! i (add1 i))
              )
            ]
           ;request renewal
           [(string-contains? (list-ref commands-list i) "requests renewal")
            (begin
              (display "Renewal ")
              (set! num (extractNumbers (list-ref commands-list i)))
              (renewal (first num))
              (display customers)
              (display "\n")
              (set! i (add1 i))
              )
            ]
           ;cheque
           [(string-contains? (list-ref commands-list i) "cheque")
            (begin
              (display "Cheque ")
              (set! num (extractNumbers (list-ref commands-list i)))
              (cheque (first num) (second num))
              (display customers)
              (display "\n")
              (set! i (add1 i))
              )
            ]
           ;spend via card
           [(string-contains? (list-ref commands-list i) "spend")
            (begin
              (display "Spend via card ")
              (set! num (extractNumbers (list-ref commands-list i)))
              (card (first num) (second num))
              (display customers)
              (display "\n")
              (set! i (add1 i))
              )
            ]
           ;transfer
           [(string-contains? (list-ref commands-list i) "transfer")
            (begin
              (display "Transfer ")
              (set! num (extractNumbers (list-ref commands-list i)))
              (card (first num) (second num))
              (display customers)
              (display "\n")
              (set! i (add1 i))
              )
            ]
           ;close the account
           [(string-contains? (list-ref commands-list i) "close")
            (begin
              (display "Close the account")
              (set! num (extractNumbers (list-ref commands-list i)))
              (closeAccount (first num))
              (display customers)
              (display "\n")
              (set! i (add1 i))
              )
            ]
           ;requests a loan
           [(string-contains? (list-ref commands-list i) "requests a loan")
            (begin
              (display "Loan request ")
              (set! num (extractNumbers (list-ref commands-list i)))
              (checkLoan (first num) (second num))
              (display customers)
              (display " ")
              (display confirmedLoans)
              (display "\n")
              (set! i (add1 i))
              )
            ]
           ;pay the debt
           [(string-contains? (list-ref commands-list i) "pay")
            (begin
              (display "Pay debt ")
              (set! num (extractNumbers (list-ref commands-list i)))
              (payDebt (first num) (second num))
              (display customers)
              (display "\n")
              (set! i (add1 i))
              )
            ]
           ;withdraws the loan
           [(string-contains? (list-ref commands-list i) "withdraws the loan")
            (begin
              (display "Withdraw loan ")
              (set! num (extractNumbers (list-ref commands-list i)))
              (withdrawLoan (first num))
              (display customers)
              (display "\n")
              (set! i (add1 i))
              )
            ]
           ;withdraws from his account
           [(string-contains? (list-ref commands-list i) "withdraws")
            (begin
              (display "Withdraw from the account ")
              (set! num (extractNumbers (list-ref commands-list i)))
              (card (first num) (second num))
              (display customers)
              (display "\n")
              (set! i (add1 i))
              )
            ]
           )
         )
  ;saves all needed information
  (set! out "month ")
  (set! out (string-append out (number->string month)))
  (set! out (string-append out "\n"))
  (set! i 0)
  (while (< i (length customers));customer exists
         (set! j 0)
         ;finds which account type does this customer have
         (while (and (< j (length accountTypes)) (not (equal? (first (second (list-ref customers i))) (first (list-ref accountTypes j)))))
                (begin
                  (set! j (add1 j))
                  )
                )
         (set! out (string-append out "Customer "))
         (set! out (string-append out (number->string (first (list-ref customers i))))) ;customer ID
         (set! out (string-append out "\t"))
         (set! out (string-append out (number->string (eighth (second (list-ref customers i)))))) ;initial balance
         (set! out (string-append out "\t"))
         (set! out (string-append out (number->string (second (second (list-ref customers i)))))) ;current balance
         (set! out (string-append out "\t"))
         (if (not (second (list-ref accountTypes j)));includes interest
             ;then
             (set! out (string-append out (number->string (+ month (- (sixth (list-ref accountTypes j)) (fifth (second (list-ref customers i)))))))) ;contract due
             ;else
             (set! out (string-append out (number->string 0))) ;no contract
             )
         (set! out (string-append out "\t"))
         (set! out (string-append out (number->string (sixth (second (list-ref customers i)))))) ;credit
         (set! out (string-append out "\t"))
         (if (not (second (list-ref accountTypes j)));includes interest
             ;then
             (set! out (string-append out (number->string (third (second (list-ref customers i)))))) ;current interest rate
             ;else
             (set! out (string-append out (number->string 0))) ;no contract
             )
         (set! out (string-append out "\t"))
         (if (> (ninth (second (list-ref customers i))) -1) ;had at least one loan
             ;then
             (begin
               (set! out (string-append out (number->string (tenth (second (list-ref customers i)))))) ;last loan amount
               (set! out (string-append out "\t"))
               (set! out (string-append out (number->string (ninth (second (list-ref customers i)))))) ;last loan in month
               )
             ;else
             (begin
               (set! out (string-append out (number->string 0))) ;no loans
               (set! out (string-append out "\t"))
               (set! out (string-append out (number->string 0))) ;no loans
               )
             )
         (if (not (null? (third (list-ref customers i))))
             ;then
             (begin
               (set! out (string-append out "\t"))
               (set! out (string-append out (number->string (list-ref (second (list-ref customers i)) 10)))) ;blocked money
               (set! out (string-append out "\t"))
               (set! out (string-append out (number->string (fourth (first (third (list-ref customers i))))))) ;loan due
               (set! out (string-append out "\t"))
               (set! out (string-append out (number->string (second (first (third (list-ref customers i))))))) ;loan amount left
               (set! out (string-append out "\n"))
               )
             ;else
             (begin
               (set! out (string-append out "\t"))
               (set! out (string-append out (number->string 0))) ;no loans
               (set! out (string-append out "\t"))
               (set! out (string-append out (number->string 0))) ;no loans
               (set! out (string-append out "\t"))
               (set! out (string-append out (number->string 0))) ;no loans
               (set! out (string-append out "\t"))
               )
             )
         ;iteration of i; next customer
         (set! i (add1 i))
         )
  ;writes to the file
  (if (file-exists? "output.txt")
      ;then already exists
      (delete-file "output.txt")
      ;else
      '0
      )
  (display-to-file out "output.txt")
  )