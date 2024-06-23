library(tidyverse)
library(factoextra)

boston_cocktails <- read.csv("/Users/allisonking/Downloads/boston_cocktails.csv")

#Data Cleaning
cocktails_parsed <- boston_cocktails %>%
  mutate(
    ingredient = str_to_lower(ingredient),
    ingredient = str_replace_all(ingredient, "-", " "),
    ingredient = str_remove(ingredient, " liqueur$"),
    ingredient = str_remove(ingredient, " (if desired)$"),
    ingredient = case_when(
      str_detect(ingredient, "bitters") ~ "bitters",
      str_detect(ingredient, "lemon") ~ "lemon juice",
      str_detect(ingredient, "lime") ~ "lime juice",
      str_detect(ingredient, "grapefruit") ~ "grapefruit juice",
      str_detect(ingredient, "orange") ~ "orange juice",
      TRUE ~ ingredient
    ),
    measure = case_when(
      str_detect(ingredient, "bitters") ~ str_replace(measure, "oz$", "dash"),
      TRUE ~ measure
    ),
    measure = str_replace(measure, " ?1/2", ".5"),
    measure = str_replace(measure, " ?3/4", ".75"),
    measure = str_replace(measure, " ?1/4", ".25"),
    measure_number = parse_number(measure),
    measure_number = if_else(str_detect(measure, "dash$"),
                             measure_number / 50,
                             measure_number
    )
  ) %>%
  add_count(ingredient) %>%
  filter(n > 15) %>%
  select(-n) %>%
  distinct(row_id, ingredient, .keep_all = TRUE) %>%
  na.omit()

cocktails_parsed

#Pivot data so that each row is a cocktail
cocktails_df <- cocktails_parsed %>%
  select(-ingredient_number, -row_id, -measure) %>%
  pivot_wider(names_from = ingredient, values_from = measure_number, values_fill = 0) %>%
  janitor::clean_names() %>%
  na.omit()

cocktails_df

#PCA
alcPCA <- prcomp(cocktails_df[,3:42], center=T, scale=T)
summary(alcPCA)
alcPCA

#Loadings
alcPCA$rotation
alcPCA$rotation[,1:3]

fviz_eig(alcPCA, ncp=40)

#Biplot
fviz_pca_biplot(alcPCA, repel = TRUE, col.var = "blue", 
                col.ind = "red")

