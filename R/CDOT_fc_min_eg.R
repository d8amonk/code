#############
### NOTES ###
#############

chooser <- function(n) {
  for (i in n)
  x <<- matrix(n,1)
  a <- choose(n,i)
  x <- 
  }

ID <- c("A", "B", "B", "c", "A", "B", "c", "c", "A", "B")
Date = seq(as.Date("2000/07/01"), 
           as.Date("2000/07/10"), 
           "days")
Amt <- rnorm(10, 10, 3)

E <- data.frame(Date = Date, 
                ID = ID, 
                Amt = Amt)
E <- data.frame(Date = Date, ID = ID, Amt = Amt)

E.e <- E %>%
  group_by(ID) %>% 
  summarise(contract_len = as.numeric(difftime(last(Date), first(Date), unit="days")),
            first_pay = first(Date),
            last_pay =last(Date),
            #flightpath = list(d=Date, p=Amt),
            num_payments = n(),
            payment = sum(Amt))


E.e <- E %>%
  group_by(ID) %>% 
  summarise(contract_len = as.numeric(difftime(last(Date), 
                                               first(Date), 
                                               unit="days")),
            first_pay = first(Date),
            last_pay = last(Date),
            num_payments = n(),
            payment = sum(Amt)
            )

E.e$fp <- E %>%
  group_by(ID) %>% 
  mutate(E.e, fp = list(Date))
  #mutate(E.e, c_range = list(seq(first_pay,last_pay)))

View(E.e)

# x <- E %>%
#   group_by(ID) %>%
#     summarise(c_Start = as.POSIXct(min(Date)),
#               c_End = as.POSIXct(min(Date)),
#               c_Len = as.numeric(difftime(last(Date), first(Date), unit="days")),
#               c_Months = c_Len/(365/12), #30.41667
#               c_PayCount = n(),
#               c_Tot = sum(Amt))
x <- data.frame(E.e)

flight_path <- E %>% 
  group_by(ID) %>% 
  list(Date = Date, 
       Amt = Amt)

View(xx)
