glm_hmm_one_seed_r <- function(.model_matrix_with_session, #.names, 
                      .num_states = as.integer(2), 
                      .obs_dim = as.integer(1), 
                      .input_dim = as.integer(2),
                      .num_categories = as.integer(2), 
                      .seed = as.integer(1)) {
  
  .input_dim <- length(.model_matrix_with_session) - 2
  
  n_par <- .input_dim * .num_states + .num_states^2 - .num_states
  
  .matrices <- .model_matrix_with_session %>% 
    group_by(across(1)) %>% 
    nest() %>% 
    pluck("data")
  
  glm_hmm_call <- glm_hmm(.matrices, #.names,
                              as.integer(.num_states),
                              as.integer(.obs_dim),
                              as.integer(.input_dim),
                              as.integer(.num_categories),
                              as.integer(.seed))
  
  glm_hmm_object <- glm_hmm_call[[1]]
  posterior_probs <- glm_hmm_call[[2]]
  log_prob <- glm_hmm_call[[3]]
  log_prior <- glm_hmm_call[[4]]
  
  log_lik <- log_prob - log_prior
  
  recovered_weights <- glm_hmm_object$observations$params %>% 
    as_tibble() %>% 
    rownames_to_column("state") %>% 
    pivot_longer(-state, names_to = "coef", values_to = "value") %>% 
    mutate(state = paste0("S", state))

  transitions <- glm_hmm_object$transitions$log_Ps
  
  posterior_probs <- enframe(posterior_probs) %>% 
    rename(session = name) %>% 
    mutate(value = map(value, as_tibble)) %>% 
    unnest(value) %>% 
    rownames_to_column("trial") %>%
    mutate(trial = as.integer(trial)) %>%
    group_by(session, trial) %>% 
    pivot_longer(-c(trial, session), names_to = "state", values_to = "p") %>% 
    select(session, everything()) %>% 
    mutate(state = str_replace(state, "V", "S"))


   list(model_matrix_with_session = .model_matrix_with_session, 
        log_prob = log_prob,
        log_prior = log_prior,
        log_lik = log_lik,
        n_par = n_par,
        recovered_weights = recovered_weights,
        transitions_tengo_que_reordenar_segun_estados_ordenados = transitions,
        posterior_probs = posterior_probs)
}

