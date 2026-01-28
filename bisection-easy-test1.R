library(ggplot2)
library(animint2)

# ----------------------------
# Function and Bisection Setup
# ----------------------------
f <- function(x) x^2 - 2

a <- 0
b <- 2
max_iter <- 5

# Store iterations
results <- data.frame(iteration = integer(),
                      a = numeric(),
                      b = numeric(),
                      m = numeric(),
                      f_a = numeric(),
                      f_b = numeric(),
                      f_m = numeric())

for (i in 1:max_iter) {
  m <- (a + b)/2
  results <- rbind(results,
                   data.frame(iteration = i,
                              a = a,
                              b = b,
                              m = m,
                              f_a = f(a),
                              f_b = f(b),
                              f_m = f(m)))
  if (f(a) * f(m) < 0) {
    b <- m
  } else {
    a <- m
  }
}

# Create cumulative midpoints for history trace
history <- do.call(rbind,
                   lapply(1:nrow(results), function(i){
                     data.frame(iteration = results$iteration[i],
                                m = results$m[1:i],
                                f_m = results$f_m[1:i])
                   }))

# Function curve (static)
curve_df <- data.frame(x = seq(0, 2, length.out = 200))
curve_df$y <- f(curve_df$x)

# ----------------------------
# Build the Plot
# ----------------------------
p <- ggplot() +
  # Function curve
  geom_line(data = curve_df, aes(x = x, y = y), color = "blue", size = 1) +
  
  # Shaded interval [a, b] (dynamic)
  geom_rect(
    data = results,
    aes(xmin = a, xmax = b, ymin = min(curve_df$y)-0.5, ymax = max(curve_df$y)+0.5),
    fill = "orange", alpha = 0.2,
    showSelected = "iteration"
  ) +
  
  # History trace for midpoints (fainter red)
  geom_point(
    data = history,
    aes(x = m, y = f_m),
    color = "red",
    alpha = 0.4,
    size = 2,
    showSelected = "iteration"
  ) +
  
  # Vertical line for current midpoint
  geom_segment(
    data = results,
    aes(x = m, xend = m, y = min(curve_df$y)-0.5, yend = f_m),
    color = "red",
    size = 1.2,
    showSelected = "iteration"
  ) +
  
  # Points at a, b, and current m
  geom_point(
    data = results,
    aes(x = a, y = f_a),
    color = "darkgreen",
    size = 3,
    showSelected = "iteration"
  ) +
  geom_point(
    data = results,
    aes(x = b, y = f_b),
    color = "darkgreen",
    size = 3,
    showSelected = "iteration"
  ) +
  geom_point(
    data = results,
    aes(x = m, y = f_m),
    color = "red",
    size = 4,
    showSelected = "iteration"
  ) +
  
  # Labels for a, b, m
  geom_text(
    data = results,
    aes(x = a, y = f_a - 0.3, label = paste0("a=", round(a,2), "\n", "f(a)=", round(f_a,2))),
    color = "darkgreen",
    showSelected = "iteration",
    hjust = 1
  ) +
  geom_text(
    data = results,
    aes(x = b, y = f_b - 0.3, label = paste0("b=", round(b,2), "\n", "f(b)=", round(f_b,2))),
    color = "darkgreen",
    showSelected = "iteration",
    hjust = 0
  ) +
  geom_text(
    data = results,
    aes(x = m, y = f_m + 0.1, label = paste0("m=", round(m,2), "\n", "f(m)=", round(f_m,2))),
    color = "red",
    showSelected = "iteration",
    vjust = 0
  ) +
  
  ggtitle("Bisection Method Steps with History Trace") +
  xlab("x") + ylab("f(x)") +
  theme_minimal(base_size = 14)

# ----------------------------
# Animint Object
# ----------------------------
viz <- list(
  bisection = p,
  time = list(variable = "iteration", ms = 2000) # slower so we can watch the interval shrink
)

# ----------------------------
# Compile to Local Folder
# ----------------------------
animint2dir(viz, out.dir = "bisection_easy_final_with_history", open.browser = TRUE)