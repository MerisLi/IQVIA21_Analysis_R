
top_group <- list(
  antnfct_broad = antnfct_broad,     
  antnfct_systemic = antnfct_systemic,
  b_lactam = b_lactam,                 
  ophthalmic = ophthalmic,            
  otic = otic,                         
  oxazolidinones = oxazolidinones,     
  sulf_trim = sulf_trim,              
  vaginal = vaginal                    
)

stack_antibiotic_groups <- function(top_group) {
  stacked <- NULL
  
  for (group_name in names(top_group)) {
    drugs <- top_group[[group_name]]  
    
    result <- usage %>%
      filter(antibiotic_name %in% drugs) %>% 
      group_by(provider_description) %>%
      summarise(total = sum(count, na.rm = TRUE)) %>%
      mutate(drug_group = group_name) %>%
      slice_max(n = 10, order_by = total)
    
    stacked <- bind_rows(stacked, result)
  }
  
  return(stacked)}

results <- stack_antibiotic_groups(top_group)

library(writexl)
write_xlsx(results, "stack_antibiotic_groups.xlsx")

