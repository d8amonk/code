# entry salary
entry <- 60

# entry separtion cost % of salary
entry_sep <- .3

# exec salary
hi <- 200

# exec separation cost % of salary
hi_sep <- 4

# overall leave rate (implies 1 - leave = 0.84)
leave <- .16

# number of employees
firm <- 100
entry_prop <- .9
hi_prop
hi_prop <- .1
entry_emps <- entry_prop * firm
hi_emps  <- hi_prop * firm
entry_emps
hi_emps
hi_leave  <- hi_emps*leave
entry_leave <- entry_emps*leave
hi_leave
entry_leave
hi_one <- hi_sep*hi
entry_one <- entry_sep*entry
hi_one
entry_one
hi_cost <- hi_one*hi_leave
entry_cost <- entry_one*entry_leave
total_cost  <- hi_cost + entry_cost
total_cost
# without retention cost minus with retention cost
2886 -  1539.2
# equals total cost savings ==
1346.8

