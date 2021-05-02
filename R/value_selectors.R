
# get a choice of element by priority
preferred_choice <- function(x, preferences, default=1) {
  if(length(x)==0)
    stop('no elements from which to choose!')
  
  # if no `preferences` are in `x`, use the first element of `x`
  preferences %<>% append(pluck(x, default))

  # return the first match in ranked `preferences` that is in `x`
  x %>%
    str_c(collapse='|') %>%
    sprintf(fmt='^(%s)$') %>%
    str_subset(string=preferences) %>%
    head(n=1)
}

