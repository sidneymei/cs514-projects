(defglobal ?*score* = 0)

(deftemplate question
    (slot key)
    (slot value))

(deftemplate answer
    (slot key)
    (slot value))

(deffacts questions
    (question (key age) (value "How old are you?"))
    (question (key income) (value "What's your annual income?"))
    (question (key emp-status) (value "Do you currently work? (y/n)"))
    (question (key emp-duration) (value "How many years have you been employed?"))
    (question (key defaults) (value "Do you have any defaults on your accounts? If so, how many?"))
    (question (key payments-late) (value "Have you accumulated any late payments? If so, how many?"))
    (question (key inquiries) (value "How many hard credit inquiries are on your record?"))
    (question (key accounts-active) (value "How many active accounts do you currently have?"))
    (question (key accounts-age) (value "On average, how many years have your accounts been open?"))
    (question (key utilization) (value "What percentage of your credit are you currently using? (0-100)"))
    (question (key debt) (value "How much debt do you currently have?"))
    (question (key mortgage) (value "What's the mortgage amount on your home? (Enter 0 if you don't have one.)"))
    (question (key loans) (value "How many active loans do you currently have?"))
    (question (key bankruptcies) (value "Have you ever filed for bankruptcy? (y/n)"))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module start

(defmodule start)

(defrule hello-world
    =>
    (printout t "What's your name? ")
    (bind ?n (read))
    (printout t crlf "Hello, " ?n "." crlf crlf)
    (printout t "Welcome to the Credit Scoring System." crlf crlf)
    (printout t "Based on your responses, we will determine your credit score, which will fall between 300 and 850." crlf)
    (printout t "For each question, please provide a number as your answer unless specified otherwise." crlf crlf)
    (printout t "Let's begin." crlf crlf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module ask

(defmodule ask)

(defrule ask::key
    (declare (auto-focus TRUE))
    (question (value ?value) (key ?key))
    (not (answer (key ?key))) ; Make sure we haven't asked this question yet
    ?ask <- (MAIN::ask ?key)
    =>
    (printout t ?value " ")
    (bind ?answer (read))
    (assert (answer (value ?answer) (key ?key)))
    (retract ?ask))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module query

(defmodule query)

(defrule ask-basics
    (declare (salience 100)) ; Prioritize the basics
    =>
    (printout t "..." crlf crlf)
    (assert (ask emp-status))
    (assert (ask age)))

; Follow-up based on employment status
(defrule ask-employment-related
    (declare (salience 95))
    (answer (key emp-status) (value y))
    =>
    (assert (ask emp-duration))
    (assert (ask income)))

(defrule no-employment
    (declare (salience 90))
    (answer (key emp-status) (value n))
    =>
    (assert (answer (key emp-duration) (value 0)))
    (assert (answer (key income) (value 0))))

(defrule ask-accounts
    (declare (salience 80))
    =>
    (assert (ask accounts-active)))

; Follow-up based on accounts active
(defrule ask-accounts-related
    (declare (salience 75))
    (answer (key accounts-active) (value ?n &:(> ?n 0)))
    =>
    (assert (ask payments-late))
    (assert (ask inquiries))
    (assert (ask utilization))
    (assert (ask accounts-age)))

(defrule no-accounts-active
    (declare (salience 70))
    (answer (key accounts-active) (value 0))
    =>
    (assert (answer (key payments-late) (value 0)))
    (assert (answer (key accounts-age) (value 0)))
    (assert (answer (key utilization) (value 0)))
    (assert (answer (key inquiries) (value 0))))

(defrule ask-defaults
    (declare (salience 65))
    (answer (key payments-late) (value ?n &:(> ?n 0)))
    =>
    (assert (ask defaults)))

(defrule no-defaults
    (declare (salience 60))
    (not (answer (key defaults)))
    =>
    (assert (answer (key defaults) (value 0))))

(defrule ask-others
    (declare (salience 50))
    =>
    (assert (ask bankruptcies))
    (assert (ask debt))
    (assert (ask mortgage)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module score

(defmodule score)

(defrule very-high-credit-utilization
    (answer (key utilization) (value ?p &:(> ?p 90)))
    =>
    (bind ?*score* (- ?*score* 25)))

(defrule high-credit-utilization
    (answer (key utilization) (value ?p &:(> ?p 80)))
    =>
    (bind ?*score* (- ?*score* 15)))

(defrule low-credit-utilization-with-no-defaults
    (answer (key utilization) (value ?p &:(< ?p 30)))
    (answer (key defaults) (value 0))
    =>
    (bind ?*score* (+ ?*score* 15)))

(defrule income-instability-check
    (answer (key emp-duration) (value ?y &:(< ?y 2)))
    (answer (key income) (value ?i))
    (answer (key debt) (value ?d &:(> ?d ?i)))
    =>
    (bind ?*score* (- ?*score* 10)))

(defrule mortgage-overcommitment
    (answer (key emp-duration) (value ?y &:(< ?y 2)))
    (answer (key income) (value ?i))
    (answer (key mortgage) (value ?m &:(> ?m ?i)))
    =>
    (bind ?*score* (- ?*score* 5)))

(defrule defaults-inquiries
    (or (answer (key defaults) (value ?n &:(> ?n 0)))
        (and (answer (key inquiries) (value ?i &:(> ?i 0)))
             (answer (key accounts-active) (value ?n &:(> ?n ?i)))))
    =>
    (bind ?*score* (- ?*score* 15)))

(defrule bankruptcies-and-debt
    (answer (key bankruptcies) (value y))
    (answer (key debt) (value ?d  &:(> ?d 0)))
    =>
    (bind ?*score* (- ?*score* 30)))

(defrule young-applicant-moderate-inquiries
    (answer (key age) (value ?a &:(< ?a 25)))
    (answer (key inquiries) (value ?n &:(> ?n 3) &:(<= ?n 5)))
    =>
    (bind ?penalty (* (/ (- 25 ?a) 25) 3))
    (bind ?*score* (- ?*score* ?penalty)))

(defrule young-applicant-many-inquiries
    (answer (key age) (value ?a &:(< ?a 25)))
    (answer (key inquiries) (value ?n &:(> ?n 5)))
    =>
    (bind ?penalty (* (/ (- 25 ?a) 25) 7))
    (bind ?*score* (- ?*score* ?penalty)))

(defrule elder-applicant-good-credit
    (answer (key age) (value ?a &:(> ?a 50)))
    (answer (key defaults) (value 0))
    (answer (key payments-late) (value ?l &:(< ?l 2)))
    =>
    (bind ?reward (* (/ (- ?a 50) 50) 15))
    (bind ?*score* (+ ?*score* ?reward)))

(defrule long-term-employment-good-credit
    (answer (key emp-duration) (value ?y &:(> ?y 9)))
    (answer (key defaults) (value 0))
    (answer (key utilization) (value ?p &:(< ?p 30)))
    =>
    (bind ?reward (* (/ ?y 10) 20))
    (bind ?*score* (+ ?*score* ?reward)))

(defrule potential-over-leverage
    (answer (key income) (value ?i))
    (answer (key debt) (value ?d &:(> ?d ?i)))
    =>
    (bind ?*score* (- ?*score* 20)))


(defrule short-term-employment-high-mortgage
    (answer (key income) (value ?i))
    (answer (key emp-duration) (value ?y &:(< ?y 2)))
    (answer (key mortgage) (value ?m &:(> ?m ?i)))
    =>
    (bind ?*score* (- ?*score* 15)))

(defrule mature-applicant-zero-defaults
    (answer (key age) (value ?a &:(> ?a 35)))
    (answer (key defaults) (value 0))
    =>
    (bind ?*score* (- ?*score* 15)))

(defrule high-number-of-accounts-low-utilization
    (answer (key accounts-active) (value ?n &:(> ?n 10)))
    (answer (key utilization) (value ?p &:(< ?p 30)))
    =>
    (bind ?*score* (+ ?*score* 12)))

(defrule debt-and-late-payments
    (answer (key income) (value ?i))
    (answer (key debt) (value ?d &:(> ?d ?i)))
    (answer (key payments-late) (value ?l &:(> ?l 2)))
    =>
    (bind ?*score* (- ?*score* 10)))

(defrule new-accounts-high-inquiries
    (answer (key accounts-active) (value ?n &:(< ?n 5)))
    (answer (key inquiries) (value ?i &:(> ?i 5)))
    =>
    (bind ?*score* (- ?*score* 20)))

(defrule high-utilization-high-debt
    (answer (key income) (value ?i))
    (answer (key utilization) (value ?p &:(> ?p 80)))
    (answer (key debt) (value ?d &:(> ?d (* 2 ?i))))
    =>
    (bind ?*score* (- ?*score* 20)))

(defrule senior-age-low-debt
    (answer (key income) (value ?i))
    (answer (key age) (value ?a &:(> ?a 65)))
    (answer (key debt) (value ?d &:(< ?d (* 10 ?i))))
    =>
    (bind ?*score* (+ ?*score* 10)))

(defrule bankruptcies-and-many-accounts
    (answer (key bankruptcies) (value y))
    (answer (key accounts-active) (value ?n &:(> ?n 10)))
    =>
    (bind ?*score* (- ?*score* 20)))

(defrule debt-or-late-payments
    (answer (key income) (value ?i))
    (or (answer (key debt) (value ?d &:(> ?d ?i)))
        (answer (key payments-late) (value ?l &:(> ?l 2))))
    =>
    (bind ?*score* (- ?*score* 20)))

(defrule moderate-utilization-good-history
    (answer (key utilization) (value ?p &:(> ?p 30) &:(< ?p 50)))
    (answer (key defaults) (value 0))
    =>
    (bind ?*score* (+ ?*score* 7)))

(defrule favorable-debt-to-income-ratio
    (answer (key income) (value ?i &:(> ?i 0)))
    (answer (key debt) (value ?d))
    (test (<= (/ ?d ?i) 0.3))
    =>
    (bind ?*score* (+ ?*score* 15)))

(defrule few-late-payments
    (answer (key payments-late) (value ?l &:(> ?l 0) &:(<= ?l 3)))
    =>
    (bind ?*score* (- ?*score* 5)))

(defrule many-late-payments
    (answer (key payments-late) (value ?l &:(> ?l 3)))
    =>
    (bind ?*score* (- ?*score* 20)))

(defrule single-default
    (answer (key defaults) (value 1))
    =>
    (bind ?*score* (- ?*score* 10)))

(defrule multiple-defaults
    (answer (key defaults) (value ?d &:(> ?d 1)))
    =>
    (bind ?penalty (* ?d 8))
    (bind ?*score* (- ?*score* ?penalty)))

(defrule few-inquiries
    (answer (key inquiries) (value ?i &:(> ?i 0) &:(<= ?i 3)))
    =>
    (bind ?*score* (- ?*score* 5)))

(defrule many-inquiries
    (answer (key inquiries) (value ?i &:(> ?i 3)))
    =>
    (bind ?penalty (* ?i 4))
    (bind ?*score* (- ?*score* ?penalty)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module result

(defmodule result)

(deffunction normalize (?score)
    ; Normalize the score to be between 300 and 850
    (return (+ (* 550 (/ (+ ?score 298) 407)) 300)))

(defrule print-score
    =>
    (bind ?s (normalize ?*score*))
    (printout t crlf "Your credit score is: " (round $?s) crlf crlf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run the program

(reset)
(watch facts)
(focus start query score result)
(run)
