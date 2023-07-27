library(lsa)
library(tibble)

# Sample data
prova <- tibble(A = c("apple", "banana","pear",
                      "apple", "banana","pear",
                      "apple","banana","pear"),
               B = c("fruit", "fruit", "fruit",
                     "color", "color", "color",
                     "size", "size", "size"),
               V = c(3, 5, 3, 2, 4, 2, 1, 1, 2))


###


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
      summarise(.by = entity,
                RS = sum(p_i * p_j * d),
                True_RS = 1 / (1 - RS)
    )
}

f <- function(db, entity, trait) {
  db %>%
    reframe(cross_join(pick({{ trait }}), pick({{ trait }})),
            .by = {{ entity }})
}

db <- data.frame(
  entity = c("A", "A", "B", "B"),
  beta = c("X", "Y", "X", "Z"),
  value = c(.5,.5,.1,.9)
  )

f(db,entity,beta)


matrix(c("A","B","C","D"), ncol=2)[1,2]



###

db_ECOS %>%
  filter(Country == "Italy",
         Year == 2015) %>%
  f("Buyer","Seller")



Functions$diversity <- function(db,entity,trait,v,d_matrix){
  db %>%
    summarize(crossed = list(crossing(trait, trait)),
              .by = entity) %>%
    unnest(crossed) %>%
    rename(
      i = trait...1,
      j = trait...2
    ) %>%
    left_join(db %>% transmute(
      entity,
      i = trait,
      p_i = value
    )
    ) %>%
    left_join(db %>% transmute(
      entity,
      j = trait,
      p_j = value
    )
    ) %>%
    mutate(d = map2_dbl(i,j,~1 - d_matrix[.y, .x])) %>%
    summarise(.by = entity,
              RS = sum(p_i * p_j * d),
              True_RS = 1 / (1 - RS))
}

###


db_ECOS %>%
  select(Buyer,Seller,value) %>%
  summarise(.by = c(Buyer, Seller),
            value = sum(value)) %>%
  pivot_wider(names_from = Seller,
              values_from = value) %>%
  column_to_rownames("Buyer") %>%
  as.matrix() %>%
  dist(method = "cosine", by_rows = T,
       diag = T, upper = T) %>%
  as.matrix() -> d

diversity(as.data.frame(
  db_ECOS %>%
    filter(Country == "Italy",
           Year == "2015") %>%
    transmute(Buyer,Seller,value) %>%
    mutate(value = value/sum(value),
           .by = Buyer)),
  type = "rs", dis = d,
  category_row = T)


db_ECOS %>%
  filter(Country == "Italy",
         Year == "2015") %>%
  transmute(Buyer,Seller,value) -> X

matrix(X$value,
       nrow = length(unique(X$Buyer)),
       dimnames = list(X$Buyer %>% unique(),
                       X$Seller %>% unique())) %>%
  robustrao::RaoStirling(d)


proxy::pr_DB %>% class()

dp <- sum(vec1 * vec2)
den <- sqrt(sum(vec1^2)) * sqrt(sum(vec2^2))
cos_sim <- dp / den
