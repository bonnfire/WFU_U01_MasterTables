### PLOT FOR WFU QC 

names(shipments_df)

pdf("WFU_QC_PLOTS.pdf")

ggplot(shipments_df, aes(sex)) +
  geom_histogram(stat = "count") + d
  facet_grid(~U01) + 
  labs(title = "Sex of Rats by U01, from WFU shipments")

ggplot(shipments_df, aes(cohort)) +
  geom_bar(stat = "count") + 
  facet_grid(~U01) + 
  labs(title = "Size of Cohorts by U01, from WFU shipments") + 
  theme(axis.text=element_text(size=8, angle = 45)) +
  geom_text(stat='count', aes(label=..count..), size=3, vjust=-1)

###############################################################
# create data object with data combined with shipments info
# shipments_df

ggplot(shipments_df, aes(cohort, fill = source)) +
  geom_bar(stat = "count", position='dodge') + 
  facet_grid(~U01) + 
  labs(title = "Size of Cohorts by U01, from WFU shipments and Data Received") + 
  theme(axis.text=element_text(size=8, angle = 45)) +
  geom_text(stat='count', aes(label=..count..), size=3, vjust=-1)


ggplot(shipments_df, aes(dames, sires)) +
  geom_point(aes(color = U01)) + 
  # facet_grid(~U01) + 
  labs(title = "Sires and Dames by U01, from WFU shipments") + 
  theme(axis.text=element_text(size=4))

# get apurva's thoughts on this! 

ggplot(shipments_df, aes(dob)) +
  geom_histogram(aes(fill = U01, alpha = 0.5), bins = 30) + 
  # facet_grid(~U01) + 
  labs(title = "******Rat Birthday Timeline by U01, from WFU shipments") + 
  theme(axis.text=element_text(size=10, angle = 45)) + 
  scale_x_datetime(date_breaks = "25 day")

ggplot(shipments_df, aes(shipmentdate)) +
  geom_histogram(aes(fill = U01), bins = 45, alpha = 0.5) + 
  # facet_grid(~U01) + 
  labs(title = "*********Shipment Dates Timeline by U01, from WFU shipments") + 
  theme(axis.text=element_text(size=8, angle = 45)) + 
  scale_x_datetime(date_breaks = "25 day")


ggplot(shipments_df, aes(shipmentdate)) +
  geom_density(aes(color = U01), bins = 45) + 
  # facet_grid(~U01) + 
  labs(title = "***************Another way to graph... Shipment Dates Timeline by U01, from WFU shipments") + 
  theme(axis.text=element_text(size=8, angle = 45)) + 
  scale_x_datetime(date_breaks = "25 day")


ggplot(shipments_df, aes(litternumber)) +
  geom_histogram(stat = "count") +
  # aes(color = litternumber)
  facet_grid(~U01) +
  labs(title = "Litter number by U01, from WFU shipments")

# ggplot(shipments_df, aes(litternumber, group = cohort, fill = cohort)) +
#   geom_histogram(stat = "count") +
#   # aes(color = litternumber)
#   facet_grid(~U01) +
#   labs(title = "Litter number by U01, from WFU shipments")

ggplot(shipments_df, aes(littersize)) +
  geom_histogram(stat = "count") +
  # aes(color = litternumber)
  facet_grid(~U01) + 
  labs(title = "Litter size by U01, from WFU shipments") 

shipments_df %>% 
  group_by(sires, dames, cohort, U01) %>% 
  add_count() %>% 
  select(U01, sires, dames, cohort, n) %>% 
  ungroup() %>% 
  rename("pairsbycohort"="n") %>% 
  group_by(sires,dames, U01) %>% 
  add_count() %>% 
  rename("pairsbyexp"="n") %>% 
  dplyr::filter(pairsbycohort != pairsbyexp) %>% 
  unique() %>% 
  data.frame() %>% 
  gridExtra::grid.table()


# ggplot(shipments_df, aes(littersize, group = cohort, fill = cohort)) +
#   geom_histogram(stat = "count") +
#   # aes(color = litternumber)
#   facet_grid(~U01) + 
#   labs(title = "Litter size by U01, from WFU shipments") 

# get apurva's insight on this too
ggplot(shipments_df, aes(coatcolor, earpunch)) +
  geom_point() +
  # aes(color = litternumber)
  facet_grid(~U01) + 
  labs(title = "**************Rat identifiers coat color and ear punch by U01, from WFU shipments") 

# best way to visualize this???
ggplot(shipments_df, aes(rack, shipmentbox)) +
  geom_point() +
  # aes(color = litternumber)
  facet_grid(~U01) + 
  labs(title = "************Rack vs box by U01, from WFU shipments") 

ggplot(shipments_df, aes(weanage, shipmentage)) +
  geom_jitter() +
  # aes(color = litternumber)
  facet_grid(~U01) + 
  labs(title = "Age at shipment vs age at wean by U01, from WFU shipments") 

dev.off()
