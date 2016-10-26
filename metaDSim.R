#metaDSim

metaDSim<- function (d, metad, c, c1, c2, Ntrials) {
  # sim = metad_sim(d, metad, c, c1, c2, Ntrials)
  #
  # INPUTS
  # d - type 1 dprime
  # metad - type 2 sensitivity in units of type 1 dprime
  #
  # c - type 1 criterion
  # c1 - type 2 criteria for S1 response
  # c2 - type 2 criteria for S2 response
  # Ntrials - number of trials to simulate, assumes equal S/N
  #
  # OUTPUT
  #
  # sim - structure containing nR_S1 and nR_S2 response counts
  #
  
  
  nRatings<-length(c1)+1
  # Calc type 1 response counts
  H <- round((1-pnorm(c,d/2))*(Ntrials/2))
  FA <- round((1-pnorm(c,-d/2))*(Ntrials/2))
  CR <- round(pnorm(c,-d/2)*(Ntrials/2))
  M = round(pnorm(c,d/2)*(Ntrials/2))
  
  # Calc type 2 probabilities
  S1mu <- -metad/2
  S2mu <- metad/2
  
  # Normalising constants
  C_area_rS1 <- pnorm(c,S1mu)
  I_area_rS1 <- pnorm(c,S2mu)
  C_area_rS2 <- 1-pnorm(c,S2mu)
  I_area_rS2 <- 1-pnorm(c,S1mu)
  
  t2c1x <- c(-Inf,c1,c,c2,Inf)
  prC_rS1<-NULL
  prI_rS1<-NULL
  prC_rS2<-NULL
  prI_rS2<-NULL
  
  pnorm(Inf,S1mu)
  
  
  for (i in 1:nRatings){
    prC_rS1[i] <- (pnorm(t2c1x[i+1],S1mu) - pnorm(t2c1x[i],S1mu) ) / C_area_rS1
    prI_rS1[i] <- ( pnorm(t2c1x[i+1],S2mu) - pnorm(t2c1x[i],S2mu) ) / I_area_rS1
    prC_rS2[i] <- ((1-pnorm(t2c1x[nRatings+i],S2mu)) - (1-pnorm(t2c1x[nRatings+i+1],S2mu))) / C_area_rS2;
    prI_rS2[i] <- ((1-pnorm(t2c1x[nRatings+i],S1mu)) - (1-pnorm(t2c1x[nRatings+i+1],S1mu)) ) / I_area_rS2;
  }
  # 
  #  Ensure vectors sum to 1 to avoid problems with mnrnd
  prC_rS1 <- prC_rS1/sum(prC_rS1);
  prI_rS1 <- prI_rS1/sum(prI_rS1);
  prC_rS2 <- prC_rS2/sum(prC_rS2);
  prI_rS2 <- prI_rS2/sum(prI_rS2);
  # 
  #  Sample 4 response classes from multinomial distirbution (normalised
  #                                                        within each response class)
  nC_rS1 <- rmultinom(1,CR,prC_rS1)
  nI_rS1 <- rmultinom(1,M,prI_rS1);
  nC_rS2 <- rmultinom(1,H,prC_rS2);
  nI_rS2 <- rmultinom(1,FA,prI_rS2)
  # 
  # Add to data vectors
  sim.nR_S1 <- c(nC_rS1,nI_rS2)
  sim.nR_S2 <- c(nI_rS1,nC_rS2)
  return(list(sim.nR_S1,sim.nR_S2))
}