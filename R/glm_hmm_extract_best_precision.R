glm_hmm_extract_best_precision <- function(.glm_hmm) {
  fit <- .glm_hmm %>% 
    filter(log_lik == max(log_lik)) %>% 
    pluck("fit") %>% 
    pluck(1)
  
  posterior_wins <- fit$posterior_probs %>% 
    group_by(trial) %>%
    filter(p == max(p)) %>%
    select(-p) 
  
  
  ordered_states <- order_states_glm_hmm(fit)
  
  ordered_states

  # states_correspondence <- ordered_states %>%
  #   select(state, ordered_state)
  # 
  # fit$recovered_weights <- fit$recovered_weights %>%
  #   left_join(states_correspondence , by = "state") %>%
  #   mutate(state = ordered_state) %>%
  #   select(-ordered_state) %>%
  #   arrange(state)
  # 
  # fit$posterior_probs <- fit$posterior_probs %>%
  #   left_join(states_correspondence , by = "state") %>%
  #   mutate(state = ordered_state) %>%
  #   select(-ordered_state)
  # 
  # fit$ordered_states <- ordered_states %>%
  #   mutate(state = ordered_state) %>%
  #   select(-ordered_state) %>%
  #   arrange(state)
  # 
  # fit
}