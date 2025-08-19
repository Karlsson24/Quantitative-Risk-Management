# ---- Libraries ---- #
library(ggplot2)
library(data.table)
library(dplyr)
library(actuar)
library(stats)
library(patchwork) # Load the patchwork library

# ---- Parameters ---- #
mu_U <- 2.00; sigma_U <- 0.40
mu_V <- 2.20; sigma_V <- 0.50

alpha_X <- 1.60; m_X <- 5.5; s_X <- 1
alpha_Y <- 1.40; m_Y <- 5.4; s_Y <- 1

alpha_conf <- 0.99
alpha_VaR <- 0.995 #check

n <- 100000
set.seed(7)

# ---- Random Variable Generation Using Packages ---- #

# Generate lognormal random variables using qlnorm()
U <- qlnorm(runif(n), meanlog = mu_U, sdlog = sigma_U)
V <- qlnorm(runif(n), meanlog = mu_V, sdlog = sigma_V)
W <- U + V

# Generate Frechet random variables using qfrechet() from actuar package
X <- qfrechet(runif(n), shape = alpha_X, scale = s_X, loc = m_X)
Y <- qfrechet(runif(n), shape = alpha_Y, scale = s_Y, loc = m_Y)
Z <- X + Y

# Portfolio
P <- W + Z

# ---- Capital Allocation Functions ---- #
capital_allocation_ES <- function(component, portfolio, alpha) {
  VaR_P <- quantile(portfolio, alpha)
  ES <- mean(component[portfolio >= VaR_P])
  return(ES- mean(component))
}

capital_allocation_VaR <- function(component, portfolio, alpha, tol = 0.001) {
  VaR_P <- quantile(portfolio, alpha)
  lower <- quantile(portfolio, alpha - tol)
  upper <- quantile(portfolio, alpha + tol)
  idx <- which(portfolio >= lower & portfolio <= upper)
  ce <- if (length(idx) > 0) mean(component[idx]) else 0
  return(ce - mean(component))
}

# ---- Capital Allocation ---- #
components <- list(U, V, X, Y, W, Z, P)
component_names <- c("U", "V", "X", "Y", "W", "Z", "Portfolio")

CA_ES <- sapply(components, function(comp) capital_allocation_ES(comp, P, alpha_conf))
CA_VaR <- sapply(components, function(comp) capital_allocation_VaR(comp, P, alpha_VaR))

CA_ES_pct <- round(100 * CA_ES / CA_ES[7], 2)
CA_VaR_pct <- round(100 * CA_VaR / CA_VaR[7], 2)

# ---- Final DataFrame ---- #
df_results <- data.frame(
  Component = component_names,
  CA_ES = round(CA_ES, 4),
  CA_ES_pct = CA_ES_pct,
  CA_VaR = round(CA_VaR, 4),
  CA_VaR_pct = CA_VaR_pct
)

# ---- Diversification Benefits ---- #
ES_U <- mean(U[U >= quantile(U, alpha_conf)])
ES_V <- mean(V[V >= quantile(V, alpha_conf)])
ES_X <- mean(X[X >= quantile(X, alpha_conf)])
ES_Y <- mean(Y[Y >= quantile(Y, alpha_conf)])
ES_W <- mean(W[W >= quantile(W, alpha_conf)])
ES_Z <- mean(Z[Z >= quantile(Z, alpha_conf)])
ES_P <- mean(P[P >= quantile(P, alpha_conf)])

VaR_U <- quantile(U, alpha_VaR)
VaR_V <- quantile(V, alpha_VaR)
VaR_X <- quantile(X, alpha_VaR)
VaR_Y <- quantile(Y, alpha_VaR)
VaR_W <- quantile(W, alpha_VaR)
VaR_Z <- quantile(Z, alpha_VaR)
VaR_P <- quantile(P, alpha_VaR)

avg_loss_U <- mean(U)
avg_loss_V <- mean(V)
avg_loss_X <- mean(X)
avg_loss_Y <- mean(Y)
avg_loss_W <- mean(W)
avg_loss_Z <- mean(Z)
avg_loss_P <- mean(P)


div_benefit_ES <- 1 - (ES_P / (ES_U + ES_V + ES_X + ES_Y))
div_benefit_VaR <- 1 - (VaR_P / (VaR_U + VaR_V + VaR_X + VaR_Y))

# ---- Diversification benefit ---- #

# Average Loss
avg_loss_U <- mean(U)
avg_loss_V <- mean(V)
avg_loss_X <- mean(X)
avg_loss_Y <- mean(Y)
avg_loss_W <- mean(W)
avg_loss_Z <- mean(Z)
avg_loss_P <- mean(P)


ES_U <- mean(U[U >= quantile(U, alpha_conf)])
ES_V <- mean(V[V >= quantile(V, alpha_conf)])
ES_X <- mean(X[X >= quantile(X, alpha_conf)])
ES_Y <- mean(Y[Y >= quantile(Y, alpha_conf)])
ES_W <- mean(W[W >= quantile(W, alpha_conf)])
ES_Z <- mean(Z[Z >= quantile(Z, alpha_conf)])
ES_P <- mean(P[P >= quantile(P, alpha_conf)])

VaR_U <- quantile(U, alpha_VaR)
VaR_V <- quantile(V, alpha_VaR)
VaR_X <- quantile(X, alpha_VaR)
VaR_Y <- quantile(Y, alpha_VaR)
VaR_W <- quantile(W, alpha_VaR)
VaR_Z <- quantile(Z, alpha_VaR)
VaR_P <- quantile(P, alpha_VaR)


# CS ES 
Capital_Allocation_standalone_U <- mean(U[U >= quantile(U, alpha_conf)]) - avg_loss_U
Capital_Allocation_standalone_V <- mean(V[V >= quantile(V, alpha_conf)]) - avg_loss_V
Capital_Allocation_standalone_X <- mean(X[X >= quantile(X, alpha_conf)]) - avg_loss_X
Capital_Allocation_standalone_Y <- mean(Y[Y >= quantile(Y, alpha_conf)]) - avg_loss_Y
Capital_Allocation_standalone_Z <- mean(Z[Z >= quantile(Z, alpha_conf)]) - avg_loss_Z
Capital_Allocation_standalone_W <- mean(W[W >= quantile(W, alpha_conf)]) - avg_loss_W
Capital_Allocation_standalone_P <- mean(P[P >= quantile(P, alpha_conf)]) - avg_loss_P

# CS VaR
C_A_standalone_U <- quantile(U, alpha_VaR) - avg_loss_U
C_A_standalone_V <- quantile(V, alpha_VaR) - avg_loss_V
C_A_standalone_X <- quantile(X, alpha_VaR) - avg_loss_X
C_A_standalone_Y <- quantile(Y, alpha_VaR) - avg_loss_Y
C_A_standalone_Z <- quantile(Z, alpha_VaR) - avg_loss_Z
C_A_standalone_W <- quantile(W, alpha_VaR) - avg_loss_W
C_A_standalone_P <- quantile(P, alpha_VaR) - avg_loss_P


# Compute diversification benefitSEs
div_b_ES_P <- 1 - (CA_ES[7]/(Capital_Allocation_standalone_U+Capital_Allocation_standalone_V+Capital_Allocation_standalone_X+Capital_Allocation_standalone_Y))
div_b_ES_U <- 1 - (CA_ES[1]/Capital_Allocation_standalone_U)
div_b_ES_V <- 1 - (CA_ES[2]/Capital_Allocation_standalone_V)
div_b_ES_X <- 1 - (CA_ES[3]/Capital_Allocation_standalone_X)
div_b_ES_Y <- 1 - (CA_ES[4]/Capital_Allocation_standalone_Y)
div_b_ES_W <- 1 - (CA_ES[5]/(Capital_Allocation_standalone_V+Capital_Allocation_standalone_U))
div_b_ES_Z <- 1 - (CA_ES[6]/(Capital_Allocation_standalone_Y+Capital_Allocation_standalone_X))


div_b_Var_P <- 1 - (CA_VaR[7]/(C_A_standalone_U + C_A_standalone_V + C_A_standalone_X + C_A_standalone_Y))
div_b_Var_U <- 1 - (CA_VaR[1] / C_A_standalone_U)
div_b_Var_V <- 1 - (CA_VaR[2] / C_A_standalone_V)
div_b_Var_X <- 1 - (CA_VaR[3] / C_A_standalone_X)
div_b_Var_Y <- 1 - (CA_VaR[4] / C_A_standalone_Y)
div_b_Var_W <- 1 - (CA_VaR[5] / (C_A_standalone_U + C_A_standalone_V))
div_b_Var_Z <- 1 - (CA_VaR[6] / (C_A_standalone_X + C_A_standalone_Y))

# Slå ihop och skriv ut
div_b_Var <- c(P = div_b_Var_P,
               U = div_b_Var_U,
               V = div_b_Var_V,
               X = div_b_Var_X,
               Y = div_b_Var_Y,
               W = div_b_Var_W,
               Z = div_b_Var_Z)
print(div_b_Var)


# ---- Beräkning av CS VaR (Standalone) ----
C_A_standalone <- c(
  U = quantile(U, alpha_VaR) - mean(U),
  V = quantile(V, alpha_VaR) - mean(V),
  X = quantile(X, alpha_VaR) - mean(X),
  Y = quantile(Y, alpha_VaR) - mean(Y),
  Z = quantile(Z, alpha_VaR) - mean(Z),
  W = quantile(W, alpha_VaR) - mean(W),
  Portfolio = quantile(P, alpha_VaR) - mean(P)
)

# ---- Utskrift med full precision ----
options(digits = 10)
for (i in names(C_A_standalone)) {
  cat(i, ": ", C_A_standalone[i], "\n")
}


# ---- Beräkning av VAR ----
C_A_standalone <- c(
  U = quantile(U, alpha_VaR),
  V = quantile(V, alpha_VaR),
  X = quantile(X, alpha_VaR),
  Y = quantile(Y, alpha_VaR),
  Z = quantile(Z, alpha_VaR),
  W = quantile(W, alpha_VaR),
  Portfolio = quantile(P, alpha_VaR)
)

# ---- Utskrift med full precision ----
options(digits = 10)
for (i in names(C_A_standalone)) {
  cat(i, ": ", C_A_standalone[i], "\n")
}

# ---- Output ---- #
cat("\n--- Capital Allocation Table ---\n")
print(df_results %>% select(Component, CA_ES, CA_ES_pct, CA_VaR, CA_VaR_pct))

cat("\n--- Diversification Benefits ---\n")
cat("Diversification Benefit (VaR):", round(div_b_Var_P, 5), "\n")



# ---- Plots ---- #
library(ggplot2)
library(patchwork) # For combining plots

presentation_blue <- "#1f77b4" # Use the same blue color

# Scatterplot: U vs V
rank_U <- rank(U) / (length(U) + 1)
rank_V <- rank(V) / (length(V) + 1)
scatter_uv <- ggplot(data.frame(rank_U, rank_V), aes(x = rank_U, y = rank_V)) +
  geom_point(alpha = 0.5, size = 1.2, color = presentation_blue) +
  labs(title = "Copula (U vs V)", x = "Rank(U)", y = "Rank(V)") +
  coord_fixed(ratio = 1) + # Make square (ratio = 1 for ranks)
  theme_minimal(base_size = 12)

# Scatterplot: X vs Y
rank_X <- rank(X) / (length(X) + 1)
rank_Y <- rank(Y) / (length(Y) + 1)
scatter_xy <- ggplot(data.frame(rank_X, rank_Y), aes(x = rank_X, y = rank_Y)) +
  geom_point(alpha = 0.5, size = 1.2, color = presentation_blue) +
  labs(title = "Copula (X vs Y)", x = "Rank(X)", y = "Rank(Y)") +
  coord_fixed(ratio = 1) + # Make square (ratio = 1 for ranks)
  theme_minimal(base_size = 12)

# Scatterplot: W vs Z
rank_W <- rank(W) / (length(W) + 1)
rank_Z <- rank(Z) / (length(Z) + 1)
scatter_wz <- ggplot(data.frame(rank_W, rank_Z), aes(x = rank_W, y = rank_Z)) +
  geom_point(alpha = 0.5, size = 1.2, color = presentation_blue) +
  labs(title = "Copula (W vs Z)", x = "Rank(W)", y = "Rank(Z)") +
  coord_fixed(ratio = 1) + # Make square (ratio = 1 for ranks)
  theme_minimal(base_size = 12)

# Combine the plots (adjust layout as needed)
combined_plot <- (scatter_uv + scatter_xy + scatter_wz) +
  plot_layout(nrow = 1) +
  plot_annotation(title = "Copula Dependencies")

# Print the combined plot
print(combined_plot)

