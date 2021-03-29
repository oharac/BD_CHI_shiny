### setup

### dt_join is datatable version of dplyr joins, for speed
dt_join <- function (df1, df2, by, type) {
  a <- case_when(type == 'left'  ~ c(FALSE, TRUE, FALSE), 
                 type == 'full'  ~ c(TRUE, TRUE, TRUE), 
                 type == 'inner' ~ c(FALSE, FALSE, FALSE))
  dt1 <- data.table::data.table(df1, key = by)
  dt2 <- data.table::data.table(df2, key = by)
  dt_full <- merge(dt1, dt2, all = a[1], all.x = a[2], all.y = a[3])
  return(as.data.frame(dt_full))
}

message('taxa/stressor name mappings for UI')
taxa_names <- read_csv(here('data/iucn_taxa_2020-1.csv')) %>%
  select(tx_field = assess_gp, tx_name = desc) %>%
  distinct() %>%
  bind_rows(data.frame(tx_field = 'all', tx_name = 'all at-risk species')) %>%
  arrange(tx_name) %>%
  mutate(tx_field = fct_inorder(tx_field),
         tx_name = fct_inorder(tx_name))

str_names <- read_csv(here('data/stressor_names.csv')) %>%
  distinct() %>%
  mutate(str_field = fct_inorder(str_field),
         str_name = fct_inorder(str_name),
         category = fct_inorder(category))

str_table <- str_names %>%
  filter(!str_detect(str_field, 'cum_')) %>%
  mutate(str_name = str_remove(str_name, ' \\(.+\\)'),
         category = str_to_title(category)) %>%
  select(Category = category, `Stressor name` = str_name)
