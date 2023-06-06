order_states_glm_hmm <- function(glm_hmm){
  
  glm_hmm |> select(-data) |> 
  unnest_wider(fit) |> 
  select(subject, recovered_weights) |> 
  unnest(recovered_weights)

order <- weights_glm_hmm1 |> 
  filter(coef == "V2") |> 
  group_by(subject) |> 
  arrange(desc(value), .by_group = TRUE) |> 
  mutate(ordered_state = paste0("S", row_number())) |> 
  select(subject, state, ordered_state)

weights_glm_hmm1 |> 
  left_join(order) |> 
  select(-state) |> 
  rename(state = ordered_state)
} 
  