library(tidyverse)

# db is a data.frame. It can be a tibble too.
# entity is the analytical unit, which will be evalauted at the end of the function.
# trait is the name a portion of the (activities of) the entity
# value is the proportion of the activity !!! IT MUST BE ALREADY expressed as a relative frequency !!!
# d_matrix is a matrix of dissimilarities (NOT OF SIMILARITIES)

f <- function(db, entity, trait, value, d_matrix) {
  {{ db }} %>%
    reframe(i = expand.grid({{ trait }},{{ trait }}),
            .by = {{ entity }}) %>%
    unnest(i) %>%
    rename(
      i = Var1,
      j = Var2,
    ) %>%
    left_join({{ db }} %>% transmute(
      {{ entity }},
      i = {{ trait }},
      p_i = {{ value }}
    )
    ) %>%
    left_join({{ db }} %>% transmute(
      {{ entity }},
      j = {{ trait }},
      p_j = {{ value }}
    )
    ) %>%
    mutate(d = map2_dbl(i,j,~{{ d_matrix }}[.y, .x])) %>%
    summarise(.by = {{ entity }},
              RS = sum(p_i * p_j * d),
              True_RS = 1 / (1 - RS)
    )
}
