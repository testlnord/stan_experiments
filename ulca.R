# my attempt to simmulate unrestricted LC

# let's generate some data
# we have two classes: c1 and c2, and 50 people from each class
# people from these classes answers yes/no question. For simlicity let's take two questions: q1, q2

# c1 answers
q1.c1 = rbinom(120, 1, 0.1)
q2.c1 = rbinom(120, 1, 0.9)

# c2 answers
q1.c2 = rbinom(80, 1, 0.9)
q2.c2 = rbinom(80, 1, 0.1)

# here ^ people from c1 tend to choose `yes` as second answer 

stan_data <- list(
  N = 200,
  Q = 2,
  C = 2,
  X = cbind(c(q1.c1, q1.c2), c(q2.c1, q2.c2))
  #class_priors_probs = c(3, 3)
)
# Create the stan model object using Stan's syntax
stanmodelcode = "
data {                      // Data block
int<lower=1> N;           // Number of people
int<lower=2> C;           // Number of classes
int<lower=2> Q;           // Number of questions
vector<lower=0, upper=1>[Q] X[N];           // Answers 

}


transformed data {          
vector<lower=0, upper=1>[Q] invX[N];           // Inverted Answers 
for (n in 1:N){
 invX[n] =  1 - X[n];
}

} 


parameters {                // Parameters block
  simplex[Q] pi[C];           // Question probabilities for classes
  simplex[C] classes_weights;      // Vector of class proportions parameters
  simplex[C] class_probs[N];   // probability of person belongs to class
  //vector<lower=0>[Q] class_priors_probs; 
}


model {                     // Model block
 // priors
  //vector[Q] pi[C];
  vector[Q] log_pi[C];
  vector[Q] log_1m_pi[C];

  //for (c in 1:C){
  //    pi[c] ~ dirichlet(class_priors_probs);
  //}  

    
  for (c in 1:C){
    log_pi[c] = log(pi[c]);
    log_1m_pi[c] = log(1-pi[c]);
  }  
  
 //model
  for (n in 1:N){ // for people
    real ans_by_class[C];
    for (c in 1:C){ // for classes
        ans_by_class[c] = log(classes_weights[c]) + sum(log_pi[c].*X[n] + log_1m_pi[c].*invX[n]);
    } 
    target += log_sum_exp(ans_by_class);
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
           warmup=2000, thin=10, chains=3, control=list(adapt_delta=0.9))