data{
  int nTot;
  int nratings;
  int counts[nratings*2];
}
parameters{
 real meta_d_rS1;
 real meta_d_rS2;
 real d1;
 real c1;
 real cS1;
 real cS2;
 
}
transformed parameters{
  # Type 1 counts
    int N;
  int S;
  int H;
  int M;
  int FA;
  int CR;
  real meta_d;
  real S1mu,S2mu;
  N <- sum(counts[1:nratings*2])
    S <- sum(counts[(nratings*2)+1:nratings*4])
    H <- sum(counts[(nratings*3)+1:nratings*4])
    M <- sum(counts[(nratings*2)+1:nratings*3])
    FA <- sum(counts[nratings+1:nratings*2])
    CR <- sum(counts[1:nratings])
}
model{
  # Type 1 priors
  c1 ~ normal(0, 2)
  d1 ~ normal(0, 0.5)
  ## TYPE 1 SDT BINOMIAL MODEL
  h <- normal_cdf(d1/2-c1,0,1)
  f <- normal_cdf(-d1/2-c1,0,1)
  H ~ binomial(h,S)
  FA ~ binomial(f,N)
  # Type 2 priors
 meta_d ~ dnorm(d1,0.5)
 ## TYPE 2 SDT MODEL (META-D)
# Multinomial likelihood for response counts ordered as c(nR_S1,nR_S2)
counts[1:nratings] ~ multinomial(prT[1:nratings],CR)
counts[nratings+1:nratings*2] ~ multinomial(prT[nratings+1:nratings*2],FA)
counts[(nratings*2)+1:nratings*3] ~ multinomial(prT[(nratings*2)+1:nratings*3],M)
counts[(nratings*3)+1:nratings*4] ~ multinomial(prT[(nratings*3)+1:nratings*4],H)
# Means of SDT distributions
S2mu <- meta_d/2
S1mu <- -meta_d/2
C_area_rS1 <- normal_cdf(c1 - S1mu,0,1)
I_area_rS1 <- normal_cdf(c1 - S2mu,0,1)
C_area_rS2 <- 1-normal_cdf(c1 - S2mu,0,1)
I_area_rS2 <- 1-normal_cdf(c1 - S1mu,0,1)
# Get nC_rS1 probs
pr[1] <- normal_cdf(cS1[1] - S1mu)/C_area_rS1  
for (k in 1:nratings-2) {                
  pr[k+1] <- (normal_cdf(cS1[k+1] - S1mu)-normal_cdf(cS1[k] - S1mu))/C_area_rS1
}
pr[nratings] <- (normal_cdf(c1 - S1mu)-normal_cdf(cS1[nratings-1] - S1mu))/C_area_rS1   

# Get nI_rS2 probs
pr[nratings+1] <- ((1-normal_cdf(c1 - S1mu))-(1-normal_cdf(cS2[1] - S1mu)))/I_area_rS2 
for (k in 1:nratings-2) {                
  pr[nratings+1+k] <- ((1-normal_cdf(cS2[k] - S1mu))-(1-normal_cdf(cS2[k+1] - S1mu)))/I_area_rS2
}
pr[nratings*2] <- (1-normal_cdf(cS2[nratings-1] - S1mu))/I_area_rS2

# Get nI_rS1 probs
pr[(nratings*2)+1] <- normal_cdf(cS1[1] - S2mu)/I_area_rS1
for (k in 1:nratings-2) {
  pr[(nratings*2)+1+k] <- (normal_cdf(cS1[k+1] - S2mu)-normal_cdf(cS1[k] - S2mu))/I_area_rS1 
}
pr[nratings*3] <- (normal_cdf(c1 - S2mu)-normal_cdf(cS1[nratings-1] - S2mu))/I_area_rS1  

# Get nC_rS2 probs
pr[(nratings*3)+1] <- ((1-normal_cdf(c1 - S2mu))-(1-normal_cdf(cS2[1] - S2mu)))/C_area_rS2 
for (k in 1:nratings-2) {                
  pr[(nratings*3)+1+k] <- ((1-normal_cdf(cS2[k] - S2mu))-(1-normal_cdf(cS2[k+1] - S2mu)))/C_area_rS2
}
pr[nratings*4] <- (1-normal_cdf(cS2[nratings-1] - S2mu))/C_area_rS2

# Avoid underflow of probabilities
// for (i in 1:nratings*4) {
//   prT[i] <- ifelse(pr[i] < Tol, Tol, pr[i])
// }

# Specify ordered prior on criteria (bounded above and below by Type 1 c1) 
for (j in 1:nratings-1) {
  cS1_raw[j] ~ dnorm(0,2) I(,c1-Tol)
  cS2_raw[j] ~ dnorm(0,2) I(c1+Tol,)
}
cS1[1:nratings-1] <- sort(cS1_raw)
cS2[1:nratings-1] <- sort(cS2_raw)



  
}