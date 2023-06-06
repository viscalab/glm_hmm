glm_hmm_r <- function(.model_matrix_with_session, 
                      .num_states = as.integer(2), 
                      .obs_dim = as.integer(1), 
                      .input_dim = as.integer(2),
                      .num_categories = as.integer(2), 
                      .seeds = 1:1,
                      .all_seeds = FALSE) {
  
  fits <- tibble(seed = .seeds) %>% 
    mutate(model_matrix_with_session = list(.model_matrix_with_session)) %>% 
    rowwise() %>% 
    mutate(fit = list(
      glm_hmm_one_seed_r(model_matrix_with_session, 
                         .num_states = .num_states, 
                         .obs_dim = .obs_dim,  
                         .input_dim = .input_dim,
                         .num_categories = .num_categories,
                         .seed = seed))) %>% 
    ungroup() %>% 
    mutate(log_lik = map_dbl(fit, "log_lik"), 
           n_par = map_dbl(fit, "n_par")) %>% 
    select(seed, log_lik, n_par, fit) 
  
  
  if (.all_seeds) {
    fits
  }
  else {
    fits %>% 
      glm_hmm_extract_best()
  }
  
}