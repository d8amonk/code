p <- ggplot(a, aes(GM)) + geom_density(color = 'blue', lwd = 3) + 
  geom_vline(xintercept = mean(a$GM), color = 'red', lwd = 2) + 
  geom_vline(xintercept = 28635, color = 'orange', lwd = 2) + 
  geom_vline(xintercept = mean(a$GM)-sd(a$GM), color = 'black', lwd = 2) +
  geom_vline(xintercept = mean(a$GM)+sd(a$GM), color = 'black', lwd = 2) + 
  geom_text(x = 34000, y = 0.000005, label = "Mean = $31,693", color = 'red') + 
  geom_text(x = 25000, y = 0.000005, label = "11/10 = $28,635", color = 'orange') + 
  geom_text(x = 20000, y = 0.000005, label = "Mean - 1SD", color = 'black') + 
  geom_text(x = 42000, y = 0.000005, label = "Mean + 1SD", color = 'black') +
  title("GM QTD")