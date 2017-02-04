require(ggplot2)
data(diamonds)
dsmall = diamonds[sample(nrow(diamonds), 10000),]

(qplot(carat, price, data=dsmall, size = carat, color = clarity, alpha = I(.15))
 + scale_color_fivethirtyeight()
 + theme_fivethirtyeight()
 + ggtitle("Does Size Matter (to Diamond Buyers)?")
 + theme(legend.key.size = unit(2, "cm", panel),
         legend.text = element_text(size = 16, face = "bold"))
 )