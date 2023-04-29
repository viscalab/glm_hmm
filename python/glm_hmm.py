def glm_hmm(matrices, num_states, obs_dim, input_dim, num_categories, my_seed):
  
  npr.seed(my_seed)
  
  def true_choices_fun(df):
    true_choices_r = df.iloc[:, 0]
    true_choices = -true_choices_r + 1 # proportion left is fitted in ssm
    true_choices = true_choices.to_numpy()
    true_choices = np.array([true_choices]).T.astype(int)
    return(true_choices)

  true_choices = [true_choices_fun(item) for item in matrices]
  
 
  def inpts_fun(df):
    inpts = df.iloc[: , 1:]
    inpts = inpts.to_numpy()
    return(inpts)

  inpts = [inpts_fun(item) for item in matrices]
  

  new_glmhmm = ssm.HMM(num_states, obs_dim, input_dim, observations="input_driven_obs",
                       observation_kwargs=dict(C=num_categories), transitions="standard")
                       

  fit_ll = new_glmhmm.fit(true_choices, inputs=inpts, method="em", num_iters=500, tolerance=10**-4, verbose = 1)
  

  
  log_prob = new_glmhmm.log_probability(true_choices, inputs=inpts)

  log_prior = new_glmhmm.log_prior()

  posterior_probs = [new_glmhmm.expected_states(data=data, input=inpt)[0]
                for data, inpt
                in zip(true_choices, inpts)]

  return([new_glmhmm, posterior_probs, log_prob, log_prior])

