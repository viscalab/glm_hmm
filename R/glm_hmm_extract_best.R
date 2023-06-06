glm_hmm_extract_best <- function(.glm_hmm) {
  fit <- .glm_hmm %>% 
    filter(log_lik == max(log_lik)) %>% 
    pluck("fit") %>% 
    pluck(1)
  
  posterior_wins <- fit$posterior_probs %>% 
    group_by(trial) %>%
    filter(p == max(p)) %>%
    select(-p) 
  
  ordered_states <- posterior_wins %>% 
    group_by(state) %>% 
    count(name = "n_trials") %>% 
    arrange(desc(n_trials)) %>% 
    ungroup() %>% 
    mutate(ordered_state = paste0("S", row_number()),
           prop = n_trials / sum(n_trials))
  
  states_correspondence <- ordered_states %>% 
    select(state, ordered_state)
  
  fit$recovered_weights <- fit$recovered_weights %>%
    left_join(states_correspondence , by = "state") %>%
    mutate(state = ordered_state) %>%
    select(-ordered_state) %>%
    arrange(state)
  
  fit$posterior_probs <- fit$posterior_probs %>% 
    left_join(states_correspondence , by = "state") %>% 
    mutate(state = ordered_state) %>% 
    select(-ordered_state)
  
  fit$ordered_states <- ordered_states %>% 
    mutate(state = ordered_state) %>% 
    select(-ordered_state) %>% 
    arrange(state) 
  
  fit
}