library(copula)
library(dplyr)
library(evd)
library(ggplot2)
library(patchwork) # For combining plots

# ---- Functions ---- #

capital_allocation_ES <- function(component, portfolio, alpha) {
  VaR_portfolio <- quantile(portfolio, alpha)
  conditional_ES <- mean(component[portfolio >= VaR_portfolio])
  return(conditional_ES-mean(component))
}

capital_allocation_VaR <- function(component, portfolio, alpha) {
  VaR_portfolio <- quantile(portfolio, alpha)
  tol <- 0.001
  lower <- quantile(portfolio, alpha - tol)
  upper <- quantile(portfolio, alpha + tol)
  idx <- which(portfolio >= lower & portfolio <= upper)
  ce <- if (length(idx) > 0) mean(component[idx]) else 0 #Calculates the conditional expectation by taking the mean of the component value a the indeificated inices
  return(ce - mean(component)) #check this part
}

set.seed(7) #check!!
n <- 100000
alpha_conf <- 0.99 #99.5% VaR??
alpha_conf_VaR <- 0.995

# Log-normal parameters
mu_u <- 2.00; sigma_u <- 0.40
mu_v <- 2.20; sigma_v <- 0.50

# Fréchet parameters
alpha_x <- 1.60; loc_x <- 5.5; scale_x <- 1
alpha_y <- 1.40; loc_y <- 5.4; scale_y <- 1

# ---- 1. Lognormal sampling via Clayton copula (U vs V) ---- #
theta_uv <- 2
cop_uv <- claytonCopula(param = theta_uv, dim = 2)
uv_uv <-1- rCopula(n, cop_uv)

U <- qlnorm(uv_uv[, 1], meanlog = mu_u, sdlog = sigma_u)
V <- qlnorm(uv_uv[, 2], meanlog = mu_v, sdlog = sigma_v)

# ---- 2. Fréchet sampling via Clayton copula (X vs Y) ---- #
theta_xy <- 1
cop_xy <- claytonCopula(param = theta_xy, dim = 2)
uv_xy <-1 - rCopula(n, cop_xy)

X <- qfrechet(uv_xy[, 1], shape = alpha_x, scale = scale_x, loc = loc_x)
Y <- qfrechet(uv_xy[, 2], shape = alpha_y, scale = scale_y, loc = loc_y)

# ---- 3. Survival Clayton copula, W = U + V och Z = X + Y ---- #
# Sort margins
U_sorted <- sort(U)
V_sorted <- sort(V)
X_sorted <- sort(X)
Y_sorted <- sort(Y)

# Generate depedence between W and Z via copula
theta_zw <- 0.5
cop_zw <- claytonCopula(param = theta_zw, dim = 2)
zw_surv <- 1 - rCopula(n, cop_zw)

# Koppla tillbaka rankat beroende
U_cop <- U_sorted[rank(zw_surv[, 1])]
V_cop <- V_sorted[rank(zw_surv[, 1])]
W <- U_cop + V_cop

X_cop <- X_sorted[rank(zw_surv[, 2])]
Y_cop <- Y_sorted[rank(zw_surv[, 2])]
Z <- X_cop + Y_cop


# Portfolio
P <- W + Z

cat("Spearman-korrelation U och V:", cor(U, V, method = "spearman"), "\n") #correct method?? Rank-Correlation kendall
cat("Spearman-korrelation X och Y:", cor(X, Y, method = "spearman"), "\n")
cat("Spearman-korrelation W och Z:", cor(W, Z, method = "spearman"), "\n") #or pearson

cat("Pearson-korrelation U och V:", cor(U, V, method = "pearson"), "\n") #correct method?? Rank-Correlation kendall
cat("Pearson-korrelation X och Y:", cor(X, Y, method = "pearson"), "\n")
cat("Pearson-korrelation W och Z:", cor(W, Z, method = "pearson"), "\n") #or pearson

cat("Kendall's tau (U, V):", corKendall(cbind(U, V))[1, 2], "\n")
cat("Kendall's tau (X, Y):", corKendall(cbind(X, Y))[1, 2], "\n")
cat("Kendall's tau (W, Z):", corKendall(cbind(W, Z))[1, 2], "\n")

# ---- Capital Allocation ----
components <- list(U_cop, V_cop, X_cop, Y_cop, W, Z, P)
component_names <- c("U", "V", "X", "Y", "W", "Z", "Portfolio")

CA_ES <- sapply(components, function(comp) capital_allocation_ES(comp, P, alpha_conf))
CA_VaR <- sapply(components, function(comp) capital_allocation_VaR(comp, P, alpha_conf_VaR))
CA_ES_pct <- round(100 * CA_ES / CA_ES[7], 2)
CA_VaR_pct <- round(100 * CA_VaR / CA_VaR[7], 2)

df_results <- data.frame(
  Component = component_names,
  CA_ES = round(CA_ES, 4),
  CA_ES_pct = CA_ES_pct,
  CA_VaR = round(CA_VaR, 4),
  CA_VaR_pct = CA_VaR_pct
)

# ---- Result ---- #
cat("\n--- Kapitalallokering med korrekt copulaberoende (inkl. W och Z) ---\n")
print(df_results %>% select(Component, CA_ES, CA_ES_pct, CA_VaR, CA_VaR_pct))


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

VaR_U <- quantile(U, alpha_conf_VaR)
VaR_V <- quantile(V, alpha_conf_VaR)
VaR_X <- quantile(X, alpha_conf_VaR)
VaR_Y <- quantile(Y, alpha_conf_VaR)
VaR_W <- quantile(W, alpha_conf_VaR)
VaR_Z <- quantile(Z, alpha_conf_VaR)
VaR_P <- quantile(P, alpha_conf_VaR)

# CS ES 
Capital_Allocation_standalone_U <- mean(U[U >= quantile(U, alpha_conf)]) - avg_loss_U
Capital_Allocation_standalone_V <- mean(V[V >= quantile(V, alpha_conf)]) - avg_loss_V
Capital_Allocation_standalone_X <- mean(X[X >= quantile(X, alpha_conf)]) - avg_loss_X
Capital_Allocation_standalone_Y <- mean(Y[Y >= quantile(Y, alpha_conf)]) - avg_loss_Y
Capital_Allocation_standalone_Z <- mean(Z[Z >= quantile(Z, alpha_conf)]) - avg_loss_Z
Capital_Allocation_standalone_W <- mean(W[W >= quantile(W, alpha_conf)]) - avg_loss_W
Capital_Allocation_standalone_P <- mean(P[P >= quantile(P, alpha_conf)]) - avg_loss_P

# CS VaR
C_A_standalone_U <- quantile(U, alpha_conf_VaR) - avg_loss_U
C_A_standalone_V <- quantile(V, alpha_conf_VaR) - avg_loss_V
C_A_standalone_X <- quantile(X, alpha_conf_VaR) - avg_loss_X
C_A_standalone_Y <- quantile(Y, alpha_conf_VaR) - avg_loss_Y
C_A_standalone_Z <- quantile(Z, alpha_conf_VaR) - avg_loss_Z
C_A_standalone_W <- quantile(W, alpha_conf_VaR) - avg_loss_W
C_A_standalone_P <- quantile(P, alpha_conf_VaR) - avg_loss_P


# Compute diversification benefitSEs
div_b_ES_P <- 1 - (CA_ES[7]/(Capital_Allocation_standalone_U+Capital_Allocation_standalone_V+Capital_Allocation_standalone_X+Capital_Allocation_standalone_Y))
div_b_ES_U <- 1 - (CA_ES[1]/Capital_Allocation_standalone_U)
div_b_ES_V <- 1 - (CA_ES[2]/Capital_Allocation_standalone_V)
div_b_ES_X <- 1 - (CA_ES[3]/Capital_Allocation_standalone_X)
div_b_ES_Y <- 1 - (CA_ES[4]/Capital_Allocation_standalone_Y)
div_b_ES_W <- 1 - (CA_ES[5]/(Capital_Allocation_standalone_U+Capital_Allocation_standalone_V))
div_b_ES_Z <- 1 - (CA_ES[6]/(Capital_Allocation_standalone_Y+Capital_Allocation_standalone_X))


div_b_Var_P <- 1 - ( CA_VaR[7]/ (C_A_standalone_V + C_A_standalone_U + C_A_standalone_Y + C_A_standalone_X))
div_b_Var_U <- 1 - (CA_VaR[1]/C_A_standalone_U)
div_b_Var_V <- 1 - (CA_VaR[2]/C_A_standalone_V)
div_b_Var_X <- 1 - (CA_VaR[3]/C_A_standalone_X)
div_b_Var_Y <- 1 - (CA_VaR[4]/C_A_standalone_Y)
div_b_Var_W <- 1 - (CA_VaR[5]/(C_A_standalone_V+C_A_standalone_U))
div_b_Var_Z <- 1 - (CA_VaR[6]/(C_A_standalone_X+C_A_standalone_Y))
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
  U = quantile(U, alpha_conf_VaR) - mean(U),
  V = quantile(V, alpha_conf_VaR) - mean(V),
  X = quantile(X, alpha_conf_VaR) - mean(X),
  Y = quantile(Y, alpha_conf_VaR) - mean(Y),
  Z = quantile(Z, alpha_conf_VaR) - mean(Z),
  W = quantile(W, alpha_conf_VaR) - mean(W),
  Portfolio = quantile(P, alpha_conf_VaR) - mean(P)
)

# ---- Utskrift med full precision ----
options(digits = 10)
for (i in names(C_A_standalone)) {
  cat(i, ": ", C_A_standalone[i], "\n")
}


# ---- Beräkning av VAR ----
C_A_standalone <- c(
  U = quantile(U, alpha_conf_VaR),
  V = quantile(V, alpha_conf_VaR),
  X = quantile(X, alpha_conf_VaR),
  Y = quantile(Y, alpha_conf_VaR),
  Z = quantile(Z, alpha_conf_VaR),
  W = quantile(W, alpha_conf_VaR),
  Portfolio = quantile(P, alpha_conf_VaR)
)

# ---- Utskrift med full precision ----
options(digits = 10)
for (i in names(C_A_standalone)) {
  cat(i, ": ", C_A_standalone[i], "\n")
}



