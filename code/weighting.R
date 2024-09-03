library(tidyverse)

library(googlesheets4)
library(googledrive)

gs4_found <- gs4_find()

gs4_found %>% filter(str_detect(name, "cps")) %>% select(id, name)
view(gs4_found)

dict <- tribble(~var, ~label,
        "ucgid", c("Bronx" = 5,
                    "Kings (Brooklyn)" = 47,
                    "New York (Manhattan)" = 61,
                    "Queens" = 81,
                    "Richmond (Staten Island)" = 85),
        "race", c("white" = 1,
                  "black" = 2,
                  "american indian, alaskan native" = 3,
                  "asian" = 4,
                  "hawaiian/pacific islander only" = 5,
                  "two or more" = 6,
                  "hispanic" = 7),
        "sex", c("male" = 1,
                 "female" = 2)
        )

query1 <- "https://api.census.gov/data/2022/cps/basic/may?tabulate=weight(PWSSWGT)&row+ucgid&row+PEHSPNON&row+PESEX&row+PTDTRACE&ucgid=0500000US36005,0500000US36047,0500000US36061,0500000US36081,0500000US36085"
result1 <- httr::GET(query1)
listed <- jsonlite::fromJSON(rawToChar(result1$content))
as_matrix <- listed[2:nrow(listed),]
column_names <- listed[1,]
colnames(as_matrix) <- column_names

cps <- as_tibble(as_matrix) %>%
  rename_all(str_to_lower) %>%
  rename(hispanic = pehspnon,
         sex = pesex,
         race = ptdtrace) %>%
  mutate(race = ifelse(str_detect(race, "0[6-9]|[1-2][0-9]"), "06", race),
         ucgid = str_extract(ucgid, "[:digit:]{2}$"),
         #across(matches("[:alpha:]"), as.integer),
         across(everything(), as.numeric),
         race = ifelse(hispanic == 1, 7, race)
         ) %>%
  group_by(ucgid, hispanic, sex, race) %>%
  summarize(tabulate = sum(tabulate)) %>% ungroup %>%
  select(-hispanic) %>%
  mutate(id = row_number())

lapply(as.list(cps), function(var) {
  return(list(sort(unique(var))))
}) %>% as_tibble %>%
  pivot_longer(cols = everything(), names_to = "variable") %>% filter(!str_detect(variable, "tabulate"))

#dict <-
#  read_delim("sp22/data/input/codebook_2022-06-11T160737.txt") %>%
#  filter(Codebook %in% column_names | str_detect(Codebook, "[:digit:]")) %>%
#  filter(!row_number() %in% c(1, 2, 3)) %>%
#  unite(Label, Created, `06/11/2022`, sep = " ", na.rm = TRUE) %>%
#  select(Codebook, Label) %>%
#  mutate(type = ifelse(str_detect(Codebook, "[:alpha:]"), "name", "value"),
#         group = c(rep(1, 3), rep(2, 3), rep(3, 27)),
#         Label = ifelse(str_detect(Codebook, "0[6-9]|[1-2][0-9]"), "two or more", Label),
#         Codebook = ifelse(str_detect(Codebook, "0[6-9]|[1-2][0-9]"), "06", Codebook)) %>%
#  distinct() %>%
#  pivot_wider(
#    id_cols = group,
#    names_from = "type",
#    values_from = c(Codebook, Label), values_fn = list) %>% unnest(c(Codebook_name, Label_name))

final <- lapply(dict$var, function(name) {
  labels <- unlist(dict$label[dict$var == name])
  out <- cps[c("id", name)] %>% mutate(across(name, ~haven::labelled(., labels)))
  return(out)
}) %>% reduce(full_join, by = c("id")) %>%
  full_join(cps %>% select(id, tabulate), by = "id")

