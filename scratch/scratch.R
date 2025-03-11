# Initial parameters
ABCs <- rnorm(n = 5, mean = 100000, sd = 33000)
ABCs[ABCs<0]<-0
OYcap <- 200000
r <- OYcap/sum(ABCs)
w <- c(1,0,0,0,0)

# Function to predict rescaled ABCs based on the multiplier
ABCPred <- function(mult) { 
  r_scaled <- r^(1/(w*mult))  
  return(ABCs * r_scaled)
}

# Objective function to minimize: squared difference between cap and sum of rescaled ABCs
fn <- function(mult) {
  ABC1 <- ABCPred(mult)
  obj <- (OYcap - sum(ABC1))^2
  return(obj)
}

# Modified bisection method for optimization
bisection_min <- function(a, b, fn, tol = 1e-5, max_iter = 1000, trace = FALSE) {
  iter <- 0
  
  # Store the values for visualization
  iterations <- data.frame(
    iteration = numeric(),
    a = numeric(),
    b = numeric(),
    mid = numeric(),
    fn_mid = numeric(),
    deriv_mid = numeric()
  )
  
  # Initial evaluation to ensure we have a valid starting interval
  fa <- fn(a)
  fb <- fn(b)
  
  # Ensure we have enough initial data for visualization
  iterations <- rbind(iterations, data.frame(
    iteration = 0,
    a = a,
    b = b,
    mid = a,
    fn_mid = fa,
    deriv_mid = NA
  ))
  
  iterations <- rbind(iterations, data.frame(
    iteration = 0.5,
    a = a,
    b = b,
    mid = b,
    fn_mid = fb,
    deriv_mid = NA
  ))
  
  while ((b - a) > tol && iter < max_iter) {
    c <- (a + b) / 2  # Calculate midpoint
    fc <- fn(c)       # Function value at midpoint
    h <- max(tol * 10, (b - a) / 100)  # Step size for numerical derivative
    
    # Numerical derivative at midpoint using central difference approximation
    deriv_c <- (fn(c + h) - fn(c - h)) / (2 * h)
    
    # Store values for visualization
    iterations <- rbind(iterations, data.frame(
      iteration = iter + 1,
      a = a,
      b = b,
      mid = c,
      fn_mid = fc,
      deriv_mid = deriv_c
    ))
    
    # Print trace information if requested
    if (trace) {
      cat(sprintf("Iteration %d: a=%.6f, b=%.6f, mid=%.6f, fn(mid)=%.6f, deriv(mid)=%.6f\n", 
                  iter + 1, a, b, c, fc, deriv_c))
    }
    
    if (abs(deriv_c) < tol) {
      # Found a stationary point (derivative close to zero)
      if (trace) {
        cat(sprintf("Converged after %d iterations. mid=%.6f, fn(mid)=%.6f, deriv(mid)=%.6f\n", 
                    iter + 1, c, fc, deriv_c))
      }
      return(list(minimum = c, iterations = iterations))
    } else if (deriv_c > 0) {
      # Function is increasing, move left (minimum is to the left)
      b <- c
    } else {
      # Function is decreasing, move right (minimum is to the right)
      a <- c
    }
    
    iter <- iter + 1
  }
  
  # Calculate final midpoint and function value
  c_final <- (a + b) / 2
  fc_final <- fn(c_final)
  
  # Print final information if reached max iterations or tolerance
  if (trace) {
    cat(sprintf("Reached max iterations or tolerance. mid=%.6f, fn(mid)=%.6f\n", 
                c_final, fc_final))
  }
  
  return(list(minimum = c_final, iterations = iterations))
}

# Use the bisection method to find the optimal mult
# Assume the function is unimodal in the interval [0, 10]
result <- bisection_min(0, 5, fn, tol=1e-7, trace = TRUE)
optimal_mult <- result$minimum
iterations <- result$iterations

# Calculate r and the rescaled ABCs using the optimal mult
r_optimal <- r^(1/(w*optimal_mult))
ABC_optimal <- ABCs * r_optimal

# Print results
cat("\n--- Results ---\n")
print(paste("Optimal mult =", optimal_mult))
print(paste("Rescaled r =", paste(round(r_optimal, 6), collapse=", ")))
print(paste("Rescaled ABCs =", paste(round(ABC_optimal, 2), collapse=", ")))
print(paste("Sum of rescaled ABCs =", sum(ABC_optimal)))
print(paste("Target OYcap =", OYcap))
print(paste("Squared difference =", fn(optimal_mult)))

# Compare with optimize() function
cat("\n--- Comparison with optimize() ---\n")
xx <- optimize(f=fn, interval=c(-1, 5))
mult_optimize <- xx$minimum
r_optimize <- r^(1/(w*mult_optimize))
ABC_optimize <- ABCs * r_optimize
print(paste("optimize() mult =", mult_optimize))
print(paste("optimize() sum =", sum(ABC_optimize)))
print(paste("optimize() squared difference =", fn(mult_optimize)))

# Visualizations
mult_seq <- seq(from=0, to=5, by=0.1)
capdiff <- sapply(mult_seq, fn)

# Focus on where the minimum actually is - get the range
min_idx <- which.min(capdiff)
min_x <- mult_seq[min_idx]
plot_range <- c(max(0, min_x - 1), min(10, min_x + 1))

# Create a zoomed in sequence for better visualization of the minimum region
mult_seq_zoom <- seq(from=plot_range[1], to=plot_range[2], length.out=101)
capdiff_zoom <- sapply(mult_seq_zoom, fn)

# Reset graphics parameters to avoid problems
par(mfrow=c(2, 2), mar=c(5, 5, 4, 2) + 0.1, mgp=c(3, 1, 0), las=1)

# Plot 1: The objective function (full range)
# Show a wider ylim range to ensure all points are visible
plot(mult_seq, capdiff, type="l", xlab="mult", ylab="Squared Difference", 
     main="Objective Function", log="y", ylim=c(min(capdiff)*0.1, max(capdiff)*10))
points(optimal_mult, fn(optimal_mult), col="red", pch=19, cex=1.5)
points(mult_optimize, fn(mult_optimize), col="blue", pch=17, cex=1.5)
abline(v=optimal_mult, col="red", lty=2)
abline(v=mult_optimize, col="blue", lty=3)
# Move legend to bottom-right as requested
legend("bottomright", 
       legend=c("Function", "Bisection Result", "optimize() Result"),
       col=c("black", "red", "blue"), lty=c(1, 2, 3), pch=c(NA, 19, 17),
       bg="white", cex=0.8, bty="n", pt.cex=1.2, seg.len=2)

# Plot 2: Zoomed in view of the objective function near the minimum
plot(mult_seq_zoom, capdiff_zoom, type="l", xlab="mult", ylab="Squared Difference", 
     main="Objective Function (Zoomed)", log="y")
points(optimal_mult, fn(optimal_mult), col="red", pch=19, cex=1.5)
points(mult_optimize, fn(mult_optimize), col="blue", pch=17, cex=1.5)
abline(v=optimal_mult, col="red", lty=2)
abline(v=mult_optimize, col="blue", lty=3)
grid()
# No legend needed here as it's the same as plot 1

# Plot 3: The convergence of the objective function value across iterations
if (nrow(iterations) > 0) {
  plot(iterations$iteration, iterations$fn_mid, type="o", 
       xlab="Iteration", ylab="Objective Function", 
       main="Convergence of Bisection Method", log="y")
  grid()
} else {
  plot(0, 0, type="n", xlab="Iteration", ylab="Objective Function",
       main="Convergence (No Data)")
  text(0, 0, "No iteration data available")
}

# Plot 4: The narrowing of the search interval across iterations
if (nrow(iterations) > 0) {
  # Get iterations with valid a and b values
  valid_iter <- !is.na(iterations$a) & !is.na(iterations$b)
  iter_vals <- iterations[valid_iter,]
  
  if (nrow(iter_vals) > 0) {
    ylim <- range(iter_vals$a, iter_vals$b)
    # Add some padding to ylim
    ylim_pad <- c(ylim[1] - 0.05*(ylim[2]-ylim[1]), ylim[2] + 0.05*(ylim[2]-ylim[1]))
    
    plot(iter_vals$iteration, iter_vals$a, type="l", ylim=ylim_pad,
         xlab="Iteration", ylab="Interval Bounds", main="Search Interval Narrowing")
    lines(iter_vals$iteration, iter_vals$b, col="red")
    
    # Add the midpoints
    points(iter_vals$iteration, iter_vals$mid, col="blue", pch=19)
    
    # Fill the interval area
    x_coords <- c(iter_vals$iteration, rev(iter_vals$iteration))
    y_coords <- c(iter_vals$a, rev(iter_vals$b))
    polygon(x_coords, y_coords, col=rgb(0,0,1,0.2), border=NA)
    
    # Re-add the lines and points on top
    lines(iter_vals$iteration, iter_vals$a, lwd=2)
    lines(iter_vals$iteration, iter_vals$b, col="red", lwd=2)
    points(iter_vals$iteration, iter_vals$mid, col="blue", pch=19)
    
    # Improved legend with less white space
    legend("topright", 
           legend=c("Lower Bound", "Upper Bound", "Midpoint"),
           col=c("black", "red", "blue"), lty=c(1, 1, NA), pch=c(NA, NA, 19),
           bg="white", cex=0.8, inset=c(0.02, 0.02), bty="n", 
           pt.cex=1.2, seg.len=2, x.intersp=0.5, y.intersp=0.8)
    grid()
  } else {
    plot(0, 0, type="n", xlab="Iteration", ylab="Interval Bounds",
         main="Search Interval (No Valid Data)")
    text(0, 0, "No valid interval data available")
  }
} else {
  plot(0, 0, type="n", xlab="Iteration", ylab="Interval Bounds",
       main="Search Interval (No Data)")
  text(0, 0, "No interval data available")
}