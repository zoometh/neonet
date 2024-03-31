# Create a basic plot
plot(x = 1:10, y = (1:10)^2, type = "b", 
     main = "Example Plot with Vertical Line", 
     xlab = "X-axis", ylab = "Y-axis")

# Add a vertical line at x = 5
abline(v = 5, col = "red", lwd = 2, lty = 2)
