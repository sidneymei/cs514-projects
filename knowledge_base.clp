; Templates
(deftemplate question
    (slot key)
    (slot value))

(deftemplate answer
    (slot key)
    (slot value))

; Facts
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