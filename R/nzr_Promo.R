# fred_playground.admin.Promotional_data

# nzR sterf ---------------------------------------------------------------

library(nzr)
nzConnect("drew", "Kiopses1", "192.168.100.5", "fred_playground", force=TRUE)
promo <- nz.data.frame("Promotional_data")
nzShowTables()
promo.df <- as.data.frame(promo)

summary(promo.df)

library(dplyr)
library(data.table)

promo <- promo.df %>%
  # arrange(desc(promo.df$EVENT_DATE), desc(promo.df$SKU))
  group_by(SKU) %>% 
  mutate(p.diff1 = promo$PRICE - lag(promo$PRICE, 1))
  
# promo.df$SKU <- as.factor(promo.df$SKU)



# remember
nzDisconnect()
