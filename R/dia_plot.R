dia_plot <- ggplot(diamonds, aes(x = carat, y = price, col = clarity))

# Expand dia_plot by adding geom_point() with alpha set to 0.2
dia_plot <- dia_plot + geom_point(alpha = 0.2)


# Plot dia_plot with additional geom_smooth() with se set to FALSE
dia_plot + geom_smooth(se = FALSE)


# Copy the command from above and add aes() with the correct mapping to geom_smooth()
dia_plot + geom_smooth(aes(col = clarity), se = FALSE)
dia_plot + geom_smooth(aes(col = clarity), se = FALSE, lwd = 1.5)
dia_plot + geom_smooth(aes(col = clarity), se = FALSE, lwd = 1.5) + ggtitle("Diamond Prices")
