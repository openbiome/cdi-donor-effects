library(tidyverse)

# load FUF data
fuf <- read_tsv("data.tsv") %>%
  select(cure, delivery, severity, partner, donor) %>%
  drop_na() %>%
  mutate(
    # recode enema -> other
    delivery = recode(delivery, enema = "other"),
    # order delivery so lower is baseline
    delivery = factor(delivery, levels = c("lower", "upper", "capsule", "other")),
    # order severity so mild/moderate is baseline
    severity = factor(severity, levels = c("mild_moderate", "severe", "severe_complicated"))
  )
