iotable_get(labelling = "iotables") %>% View()

iotable_get( labelling = "iotables" ) %>%
  .[c(1:7,15),c(1:7)] -> de_in

de_in %>%
  input_coefficient_matrix_create() %>% View()
  leontief_inverse_create() %>%
  backward_linkages() %>%
  vector_transpose_longer()

ecosys_matrices$FRA2015 %>%
  select(-f,-total) %>%
  filter(!Buyer %in% c("f")) %>%
  mutate(Buyer =
             case_when(
               Buyer == "total" ~ "output",
               T ~ Buyer,
             )
  ) %>%
  coefficient_matrix_create()

ecosys_matrices$FRA2015 %>%
  select(-f,-total) %>%
  filter(!Buyer %in% c("f","total","output")) %>%
  mutate(across(where(is.numeric),
                ~.x/sum(.x)
  )) %>%
  leontief_inverse_create() %>%
  backward_linkages() %>%
  vector_transpose_longer()

de_in %>%
  as_tibble() %>%
  mutate(iotables_row = iotables_row %>% as.character()) %>%
  filter(!iotables_row %in% c("f","total")) %>%
    mutate(across(where(is.numeric),
                  ~.x/.x[de_in$iotables_row == "total"]
    )) %>%
  filter(!iotables_row %in% c("output")) %>%
  leontief_inverse_create() %>%
  backward_linkages() %>%
  vector_transpose_longer()


iotables::netherlands_2006 %>%
  .[c(1:7,13),c(1:8)] -> nl_in

nl_in %>%
  input_coefficient_matrix_create() %>%
  leontief_inverse_create() %>%
  backward_linkages() %>%
  vector_transpose_longer()

ecosys_matrices$FRA2018 %>%
  select(-f) %>%
  filter(Buyer != "f") %>%
  add_row(.[nrow(.),]) %>%
  mutate(Buyer =
           case_when(
             row_number(.) < 16 ~ Buyer,
             row_number(.) > 15 ~ "ciao",
           )) -> FRA2018_in

FRA2018_in %>%
  input_coefficient_matrix_create() %>%
  leontief_inverse_create() %>%
  backward_linkages() %>%
  vector_transpose_longer()


ecosys_matrices$FRA2018 %>%
  select(-f) %>%
  filter(Buyer != "f") %>%
  add_row(.[nrow(.),]) %>%
  mutate(Buyer =
           case_when(
             row_number(.) < nrow(.) ~ Buyer,
             T ~ "output"
           )) -> FRA2018_in

tibble(
  names = c("A","B","C","D","E","G","H"),
  A = MASS::rnegbin (7, 12610, .4),
  B = MASS::rnegbin (7, 8020, .5),
  C = MASS::rnegbin (7, 21306, .6),
  D = MASS::rnegbin (7, 11500, .3),
  E = MASS::rnegbin (7, 13700, .2),
  G = MASS::rnegbin (7, 17600, .2),
  H = MASS::rnegbin (7, 17600, .7),
) %>%
  adorn_totals(c("row","col")) %>%
  add_row(.[nrow(.),]) %>%
  mutate(names =
           case_when(
             row_number(.) < nrow(.) ~ names,
             T ~ "output"
           )) %>%
  input_coefficient_matrix_create() %>%
  leontief_inverse_create() %>%
  backward_linkages() %>%
  vector_transpose_longer()

iotable_get( labelling = "iotables" ) %>%
  .[c(1:7,15),c(1:8)] -> de_in

iotable_get( labelling = "eurostat",
             source = "naio_10_cp1750",
             geo = "IT",
             unit = "MIO_EUR",
             stk_flow = "DOM",
             year = 2018) -> it_in

iotables_metadata_get(source = "naio_10_cp1750") %>% View()
