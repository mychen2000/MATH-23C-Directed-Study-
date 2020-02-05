hwq1 <- expand.grid(c(TRUE, FALSE), c(TRUE, FALSE), c(TRUE, FALSE), c(TRUE, FALSE)); hwq1
hwq1 <- data.frame(hwq1[, c(4:1)]); hwq1
colnames(hwq1) <- c('p', 'q', 'r', 's'); hwq1
attach(hwq1)
hwq1$Part_A <- (!(p&q))|(r|s); hwq1 #produce a fourth column that answers Part (a)

#Part (b): find conjunctive normal form of of the given expression with the truth table
#Based on the table (!p | !q | r | s) is the conjunctive normal form,
#because each term of the expression should represent each row that results in FALSE (in this case, only row 4)

#Part (c): to prove the expression in Part (b)
hwq1$Part_B <- (!p | !q | r | s); hwq1 #this macthes answers from Part_A column