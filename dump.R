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
