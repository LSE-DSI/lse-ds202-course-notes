library(tidyverse)
library(tidymodels)

aut_spells <- read_csv('Authoritarian spells.csv')

class_groups <- c('agrar','busin', 'ubour','rbour','uprol','rprol')

class_support <-
  aut_spells %>% 
  select(period_id, contains(class_groups)) %>%
  mutate(across(contains(class_groups), ~if_else(is.na(.x), 0, .x))) %>% 
  rename_with(~ str_remove(.x, '^(?:pavg|pfy|p)\\_'), everything()) 

pca_support <-
  recipe(~ ., data = class_support) %>% 
  update_role(period_id, new_role = 'id') %>% 
  step_normalize(all_predictors()) %>% 
  step_pca(all_predictors()) %>% 
  prep()

sdev <- pca_support$steps[[2]]$res$sdev

percent_variation <- sdev^2 / sum(sdev^2)

break

tibble(component = 1:length(percent_variation),
       percent_variation = cumsum(percent_variation)) %>% 
  ggplot(aes(component, percent_variation)) +
  geom_line() +
  scale_x_continuous(breaks = 1:12)

tidy(pca_support, 2) %>%
  filter(component %in% paste0('PC', 1:4)) %>% 
  mutate(component = fct_inorder(component),
         support = if_else(str_detect(terms, '\\_sup'), 'Support', 'Oppose'),
         support = fct_rev(support),
         class = str_remove(terms, '\\_(?:opp|sup)'),
         class = fct_rev(class)) %>% 
  ggplot(aes(value, class, fill = support)) +
  geom_col(position = 'dodge', alpha = 0.675) +
  facet_wrap(. ~ component, ncol = 2) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = 'bottom') +
  scale_fill_aaas() +
  labs(x = NULL, y = NULL, fill = NULL,
       title = 'Class support and opposition networks in transitioning autocracies',
       subtitle = 'Principle component analysis using V-Dem data',
       caption = paste('Proportion of variance explained:', 
                       percent(sum(percent_variation[1:4]))))

pca_support %>% 
  juice() %>% 
  ggplot(aes(PC1, PC2)) +
  geom_text_repel(aes(label = period_id), max.overlaps = 5) +
  theme_minimal()
