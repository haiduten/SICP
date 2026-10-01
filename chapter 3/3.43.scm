#lang sicp
;if there are run sequentially, the account balances will be 10, 20, 30 in some order because
; we are just swapping two balances at each time

;if we do serialize the transactions on the individual accounts:
; imagine a1 = 10, a2 = 20, a3 = 30. We exchange a1 and a2 and exchange a2 and a3
; a1 becomes 20. Then we start the exchange of a2 and a3. a2 becomes 30 and a3 becomes 20.
; then we finish the depositot in a2 to get 20. so now a1 = 20, a2 = 20, a3 = 20. The sum is still 60

; if we do not serialize the transaction on the indivudal accounts: 
; imagine a1 = 10, a2 = 20, a3 = 30. We exchange a1 and a2 and exchange a2 and a3
; we start the exchange of a1 and a2 but we stop before we make a2 equal to 10.
; then we finish the excahnge of a2 and a3, making a3 equal to 20.
; then we complete the assignemnt of a2 to 10. So we get a1 = 20, a2 = 10, a3 = 20. this does nto sum up to 60

