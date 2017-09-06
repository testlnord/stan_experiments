# my attempt to simmulate unrestricted LCA

# let's generate some data
# we have two classes: c1 and c2, and 50 people from each class
# people from these classes answers yes/no question. For simlicity let's take two questions: q1, q2

# c1 answers
q1.c1 = rbinom(50, 1, 0.3)
q2.c1 = rbinom(50, 1, 0.6)

# c2 answers
q1.c2 = rbinom(50, 1, 0.8)
q2.c2 = rbinom(50, 1, 0.2)

# here ^ people from c1 tend to choose `yes` as second answer 

stan_data <- list(
  N = 100,
  Q = 2,
  C = 2,
  X = t(cbind(rbind(q1.c1, q1.c2), rbind(q2.c1, q2.c2))),
  class_priors_probs = c(0.5, 0.5)
)
# Create the stan model object using Stan's syntax
stanmodelcode = "
data {                      // Data block
int<lower=1> N;           // Number of people
int<lower=2> C;           // Number of classes
int<lower=2> Q;           // Number of questions
vector<lower=0, upper=1>[Q] X[N];           // Answers 
vector<lower=0>[Q] class_priors_probs; 
}


transformed data {          
vector<lower=0, upper=1>[Q] invX[N];           // Inverted Answers 
for (n in 1:N){
 invX[n] =  1 - X[n];
}

} 


parameters {                // Parameters block
  vector[Q] pi[C];           // Question log probabilities for classes
  vector[C] classes_weights_pars;      // Vector of class proportions parameters

}
transformed parameters{
  vector[Q] log_pi[C];           // Question log probabilities for classes
  
  log_pi = log(pi);
  
}

model {                     // Model block
  //prior
  vector[C] classes_weights;      // Vector of class proportions
  classes_weights ~ dirichlet(classes_weights_pars);

 //model
  for (n in 1:N){ // for people
    real ans_by_class[C];
    for (c in 1:C){ // for classes
        ans_by_class[c] = sum(classes_weights[c]*(log_pi[c].*X[n] + log1m_exp(log_pi[c]).*(invX[n])));
    } 
    increment_log_prob(log_sum_exp(ans_by_class));
  }

}

/*
generated quantities {      // Generated quantities block. Not used presently.
}
*/
"
library(rstan)

### Run the model and examine results ###
fit = stan(model_code=stanmodelcode, data=stan_data, iter=12000, 
           warmup=2000, thin=10, chains=3)