### PLOT FOR WFU QC 

names(shipments_df)

pdf("WFU_QC_PLOTS.pdf")

ggplot(shipments_df, aes(sex)) +
  geom_histogram(stat = "count") + 
  facet_grid(~U01) + 
  labs(title = "Sex of Rats by U01, from WFU shipments")

ggplot(shipments_df, aes(cohort)) +
  geom_bar(stat = "count") + 
  facet_grid(~U01, scales = "free") + 
  labs(title = "Size of Cohorts by U01, from WFU shipments") + 
  theme(axis.text=element_text(size=8, angle = 45)) +
  geom_text(stat='count', aes(label=..count..), size=3, vjust=-1)

###############################################################
# create data object with data combined with shipments info
# shipments_df


# ggplot(shipments_df, aes(cohort, fill = source)) +
#   geom_bar(stat = "count", position='dodge') + 
#   facet_grid(~U01) + 
#   labs(title = "Size of Cohorts by U01, from WFU shipments and Data Received") + 
#   theme(axis.text=element_text(size=8, angle = 45)) +
#   geom_text(stat='count', aes(label=..count..), size=3, vjust=-1)
###############################3

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


#### clean up here
# look at birthday and litternumber? 
# expect: birthday should be consistent across sire dame pair and litternumber
shipments_df[,c('dames','sires', 'litternumber')] # select columns to check duplicates
siblings_insamelitter <- shipments_df[duplicated(shipments_df[,c('dames','sires', 'litternumber')]) | duplicated(shipments_df[,c('dames','sires', 'litternumber')], fromLast=TRUE),] # filter out animals that share the dames, sires, and litternumber
siblings_insamelitter %>% group_by(dames,sires,litternumber) %>% mutate(uniquebday = n_distinct(dob)) %>% ungroup() %>% select(u01, uniquebday) %>% unique()
siblings_insamelitter %>% group_by(dames,sires,litternumber) %>% mutate(uniquebday = n_distinct(dob)) %>% ungroup() %>% dplyr::filter(uniquebday!=1) %>% arrange(dames,sires,litternumber) %>% select(-uniquebday) %>%  View()
# siblings_insamelitter %>% group_by(dames,sires,litternumber,littersize) %>% mutate(uniquelittersize = n_distinct(littersize)) %>% View()
# split(siblings_insamelitter, dplyr::group_indices(dames,sires,litternumber))


## pick up from here: 
nonuniquebdaywfu <- siblings_insamelitter %>% group_by(dames,sires,litternumber) %>% mutate(uniquebday = n_distinct(dob), group = rep(letters, length.out = n()), ) %>% ungroup() %>% dplyr::filter(uniquebday!=1) %>% arrange(dames,sires,litternumber)

# expecting only 1 but we see some two values

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

# pick up from here 
shipments_df %>% 
  group_by(U01, sires, dames, litternumber) %>% 
  count() %>% arrange(desc(n))

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

shipments_df %>% 
  group_by(U01, cohort, shipmentbox, rack) %>% 
  count() %>% 
ggplot(aes(n)) +
  geom_histogram() +
  facet_grid(~U01) + 
  labs(title = "Number of rats per rack per shipment box by U01, from WFU shipments") 
# shipments_df %>% group_by(U01, cohort, shipmentbox) %>% count() %>% ungroup() %>% group_by(U01) %>% select(U01, n) %>% summarize(mean = mean(n), min = min(n), max = max(n))

shipments_df %>% 
  group_by(U01, cohort, shipmentbox) %>% 
  count() %>% 
  ggplot(aes(as.factor(n))) +
  geom_histogram(stat = "count") +
  facet_grid(~U01) + 
  labs(title = "Number of rats per shipment box by U01, from WFU shipments") 
# shipments_df %>% group_by(U01, cohort, shipmentbox) %>% count() %>% ungroup() %>% group_by(U01) %>% select(U01, n) %>% summarize(mean = mean(n), min = min(n), max = max(n))

shipments_df %>% 
  group_by(U01, cohort, shipmentbox) %>% 
  count() %>% 
  ggplot(aes(cohort, fill = as.factor(n))) +
  geom_bar() +
  facet_grid(~U01) + 
  labs(title = "** Can delete ** Number of rats per shipment box per cohort by U01, from WFU shipments") 

ggplot(shipments_df, aes(weanage, shipmentage)) +
  geom_jitter() +
  # aes(color = litternumber)
  facet_grid(~U01) + 
  labs(title = "Age at shipment vs age at wean by U01, from WFU shipments") 

dev.off()

########################################################
###### QC Number of siblings
########################################################

# for shipment timeline graph
wfu_shipmentsiblings_allexpswdate <- shipments_df %>% 
  dplyr::filter(!comment %in% c("Naive", "Pregnant female")) %>%
  group_by(sires, dames, cohort, u01) %>% 
  add_count() %>% 
  select(U01, sires, dames, cohort, n, shipmentdate) %>% 
  ungroup() %>% 
  rename("siredamepair_in_cohort"="n") %>% 
  group_by(sires,dames, u01) %>% 
  add_count() %>% 
  rename("siredamepair_in_u01"="n") %>% 
  dplyr::filter(siredamepair_in_cohort != siredamepair_in_u01)
  
  
# for details
wfu_shipmentsiblings_allexps <- shipments_df %>% 
  dplyr::filter(!comment %in% c("Naive", "Pregnant female")) %>%
  group_by(sires, dames, cohort, u01) %>% 
  add_count() %>% 
  select(u01, sires, dames, cohort, n) %>% 
  ungroup() %>% 
  rename("siredamepair_in_cohort"="n") %>% 
  group_by(sires,dames, u01) %>% 
  add_count() %>% 
  rename("siredamepair_in_u01"="n") %>% 
  dplyr::filter(siredamepair_in_cohort != siredamepair_in_u01) %>% 
  unique() %>% 
  arrange(u01, sires) %>%
  data.frame() 

# for overview
wfu_shipmentsiblings_byu01 <- wfu_shipmentsiblings_allexps %>%
  ungroup() %>% 
  select(u01, sires, dames) %>%
  distinct() %>% # count was each 2, so every incident only appeared in one other cohort
  count(u01)

# clearer overview text 
wfu_shipmentsiblings_byu01$u01 <- mgsub::mgsub(wfu_shipmentsiblings_byu01$u01,
                               c("Olivier_Co", "Olivier_Oxy"),
                               c("Olivier Cocaine", "Olivier Oxycodone"))
# for details grid reformatting, need to cut the cases up to fit onto pages
tg <- tableGrob(wfu_shipmentsiblings_allexps, rows = seq_len(nrow(wfu_shipmentsiblings_allexps)))

fullheight <- convertHeight(sum(tg$heights), "cm", valueOnly = TRUE)
margin <- unit(0.51,"in")
margin_cm <- convertHeight(margin, "cm", valueOnly = TRUE)
a4height <- 29.7 - margin_cm
nrows <- nrow(tg)
npages <- ceiling(fullheight / a4height)

heights <- convertHeight(tg$heights, "cm", valueOnly = TRUE)
rows <- cut(cumsum(heights), include.lowest = FALSE,
            breaks = c(0, cumsum(rep(a4height, npages))))

groups <- split(seq_len(nrows), rows)

gl <- lapply(groups, function(id) tg[id,])

myTableGrob <- function(data_dt, title_v, fontsize_v = 14){
  #' Create custom table grob with title
  #' @description Creates table grob in format that is most common for my usage.
  #' @param data_dt Data.table that the grob will be made out of
  #' @param title_v Title for display
  #' @param fontsize_v Fontsize for title. Default is 14 (goes well with my_theme)
  #' @value gtable object
  #' @export
  
  ## Table
  table_grob <- tableGrob(data_dt, rows = rep('', nrow(data_dt)), theme = ttheme_minimal())
  ## Title
  title_grob <- textGrob(title_v, gp = gpar(fontsize = fontsize_v))
  ## Add title
  table_grob <- gtable_add_rows(table_grob, heights = grobHeight(title_grob) + unit(5,'mm'), pos = 0)
  table_grob <- gtable_add_grob(table_grob, title_grob, 1, 1, 1, ncol(table_grob), clip = "off")
}

#  compile into pdf
pdf("WFU_QC_Siblings.pdf", paper = "a4", width = 0, height = 0)

df<-data.frame(blurb = c("In WFU's rat shipments to U01 projects, we expect each cohort to represent different generations or a separate set of sire-dame pairs. 
Therefore, we would expect to find the number of rats from the same sire-dame pair in one cohort to be equal to the the number of rats from the same sire-dame pair in the overall U01. 
We have found that to not be the case for four of the U01's. The first figure and the second figure summarize the number of cases along with the timeline of shipments, while the last figure provides detailed information of each case.
                         In the first two figures, one case is defined as the sire-dame pair itself that was repeated across cohorts; the data only observed duplicates at the time this was compiled. In other words, pairs were used in at most two cohorts. "))
d = sapply(lapply(df$blurb, strwrap, width=70), paste, collapse="\n")
grid.table(d)

grid.newpage()

default_grob <- myTableGrob(wfu_shipmentsiblings_byu01, "Cases of sharing parents within cohorts") 
grid.draw(default_grob)

ggplot(wfu_shipmentsiblings_allexpswdate, aes(U01, shipmentdate)) +
  geom_count()+ 
  labs(title = "Shipment Dates Timeline by U01, from WFU shipments") + 
  scale_y_datetime(date_breaks = "25 day") 

for(page in seq_len(npages)){
  grid.newpage()
  grid.rect(width=unit(21,"cm") - margin,
            height=unit(29.7,"cm")- margin)
  grid.draw(gl[[page]])
}

# alternative to explicit loop:
# print(marrangeGrob(grobs=gl, ncol=1, nrow=1, top=NULL))
dev.off()


# check the number of males and females in the shipment boxes
shipments_df %>% 
  dplyr::filter(!comment %in% c("Naive", "Pregnant female")) %>% 
  select(U01, shipmentdate, rack, sex) %>% 
  unique() %>% 
  group_by(U01, shipmentdate, rack) %>% 
  count(sex) %>% 
  arrange(rack) %>% 
  dplyr::filter(n!=1) # PASS check and tried with removing shipment date as variable and passed too

## for rsm/wfu, send out more metadata about the spilling sires/dames
shipments_df %>% dplyr::filter(u01 %in% wfu_shipmentsiblings_allexps$u01,
                               sires %in% wfu_shipmentsiblings_allexps$sires,
                               dames %in% wfu_shipmentsiblings_allexps$dames,
                               cohort %in% wfu_shipmentsiblings_allexps$cohort)

