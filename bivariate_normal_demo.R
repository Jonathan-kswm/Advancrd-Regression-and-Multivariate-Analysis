# Bivariate Normal Distribution Demo

# === Explanation ===
# This script demonstrates a 2-dimensional normal distribution (bivariate normal)
# and visualises:
#   1. its probability density function (PDF) as a 3-D surface (base R persp),
#   2. a 2-D contour plot of the PDF with the samples overlaid,
#   3. simulated sample points coloured by density,
#   4. simulated sample points coloured by Mahalanobis distance.
# The Mahalanobis distance is a scale-invariant metric that measures how many
# standard deviations a point is from the mean, accounting for covariance.

# Load required libraries, install if missing
required <- c("MASS", "mvtnorm", "ggplot2")
for (pkg in required) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}
suppressPackageStartupMessages({
  library(MASS)
  library(mvtnorm)
  library(ggplot2)
})

# === Parameters (edit these) ===
mean_vec  <- c(0, 0)
sd_x      <- 1
sd_y      <- 1
rho       <- 0.5      # correlation, must be in (-1, 1)
n_samples <- 500
grid_res  <- 60
n_sd_grid <- 3.5      # grid extends this many SDs either side of the mean
seed      <- 123
persp_theta <- 30     # 3-D surface azimuth (rotate the view)
persp_phi   <- 25     # 3-D surface elevation

stopifnot(abs(rho) < 1, sd_x > 0, sd_y > 0)
cov_mat <- matrix(c(sd_x^2,           rho * sd_x * sd_y,
                    rho * sd_x * sd_y, sd_y^2), nrow = 2)

# Generate samples
set.seed(seed)
samples <- mvrnorm(n = n_samples, mu = mean_vec, Sigma = cov_mat)
colnames(samples) <- c("X", "Y")
samples_df <- as.data.frame(samples)

# Density for each sample
samples_df$density <- dmvnorm(samples, mean = mean_vec, sigma = cov_mat)

# Mahalanobis distance (mahalanobis() returns the SQUARED distance, so take sqrt)
samples_df$mahalanobis <- sqrt(mahalanobis(samples, center = mean_vec, cov = cov_mat))

# Grid for density surface, based on the theoretical distribution
x_seq <- seq(mean_vec[1] - n_sd_grid * sd_x, mean_vec[1] + n_sd_grid * sd_x, length.out = grid_res)
y_seq <- seq(mean_vec[2] - n_sd_grid * sd_y, mean_vec[2] + n_sd_grid * sd_y, length.out = grid_res)
grid <- expand.grid(X = x_seq, Y = y_seq)   # X varies fastest
grid$Z <- dmvnorm(as.matrix(grid), mean = mean_vec, sigma = cov_mat)

# persp() needs z as a matrix: rows = x, columns = y
z_mat <- matrix(grid$Z, nrow = length(x_seq), ncol = length(y_seq))

# --- Plot 1: 3-D density surface (base R) ---
# Colour each facet by the average density of its four corners
nr <- nrow(z_mat); nc <- ncol(z_mat)
z_facet <- (z_mat[-1, -1] + z_mat[-1, -nc] + z_mat[-nr, -1] + z_mat[-nr, -nc]) / 4
pal <- hcl.colors(100, "viridis")
facet_col <- pal[cut(z_facet, breaks = 100, labels = FALSE)]

plot_surface <- function() {
  persp(x_seq, y_seq, z_mat,
        theta = persp_theta, phi = persp_phi, expand = 0.6,
        col = facet_col, border = NA, shade = 0.3,
        xlab = "X", ylab = "Y", zlab = "Density",
        ticktype = "detailed",
        main = "Bivariate Normal Density Surface")
}

# --- Plot 2: 2-D contour plot of the PDF with samples overlaid ---
contour_plot <- ggplot() +
  geom_contour_filled(data = grid, aes(X, Y, z = Z), alpha = 0.6) +
  geom_point(data = samples_df, aes(X, Y), size = 0.8, alpha = 0.5) +
  coord_equal() +
  labs(title = "Bivariate Normal: Density Contours and Samples",
       fill = "Density") +
  theme_minimal()

# --- Plot 3: samples coloured by density ---
scatter_density <- ggplot(samples_df, aes(X, Y, colour = density)) +
  geom_point(size = 1.5) +
  scale_colour_viridis_c(name = "Density") +
  coord_equal() +
  labs(title = "Sample Points Coloured by Density") +
  theme_minimal()

# --- Plot 4: samples coloured by Mahalanobis distance ---
scatter_mahalanobis <- ggplot(samples_df, aes(X, Y, colour = mahalanobis)) +
  geom_point(size = 1.5) +
  scale_colour_viridis_c(name = "Mahalanobis\nDistance", option = "inferno") +
  coord_equal() +
  labs(title = "Sample Points Coloured by Mahalanobis Distance") +
  theme_minimal()

# Show plots (each appears in turn in the plot window / Rplots.pdf under Rscript)
plot_surface()
print(contour_plot)
print(scatter_density)
print(scatter_mahalanobis)

cat("\nAll plots displayed. Under Rscript they are saved to Rplots.pdf in the working directory.\n")
