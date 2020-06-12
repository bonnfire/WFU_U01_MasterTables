## GRAPHS 

extractions_flowcell %>% 
  ggplot(aes(x = `nanodropng_ul`)) + 
  geom_histogram() + 
  facet_grid(~ u01) +
  labs(title = paste0("Nanodrop(ng/ul) values by U01")) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"))

extractions_flowcell %>% 
  ggplot(aes(x = `u01`, y = `nanodropng_ul`)) + 
  geom_boxplot() + 
  labs(title = paste0("Nanodrop(ng/ul) values by U01")) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"))


extractions_flowcell %>% dplyr::filter(u01 == "Olivier_Co_03") %>% 
  ggplot(aes(x = `u01`, y = `nanodropng_ul`, fill = userid)) + 
  geom_boxplot() + 
  labs(title = paste0("Nanodrop(ng/ul) values by U01")) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"))

extractions_flowcell %>% 
  ggplot(aes(x = `260_280`, color = u01)) + 
  geom_density() + 
  # facet_grid(~ u01) +
  labs(title = paste0("260_280 values by U01")) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"),
        legend.text=element_text(size=20))

extractions_flowcell %>% 
  ggplot(aes(x = `260_280`, color = u01)) + 
  geom_density() + 
  facet_grid(~ u01) +
  labs(title = paste0("260_280 values by U01")) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"))

extractions_flowcell %>% 
  ggplot(aes(x = `u01`, y = `260_280`)) + 
  geom_boxplot() + 
  labs(title = paste0("260_280 values by U01")) + 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"))

extractions_flowcell %>% dplyr::filter(u01 == "Olivier_Co_03") %>% 
  ggplot(aes(x = `u01`, y = `260_280`, fill = userid)) + 
  geom_boxplot() + 
  labs(title = paste0("260_280 values by U01")) + 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"))

extractions_flowcell %>% 
  ggplot(aes(x = `260_230`, color = u01)) + 
  geom_density() + 
  # facet_grid(~ u01) +
  labs(title = paste0("260_230 values by U01")) + 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"),
        legend.text=element_text(size=20))


extractions_flowcell %>% 
  ggplot(aes(x = `u01`, y = `260_230`)) + 
  geom_boxplot() + 
  labs(title = paste0("260_230 values by U01"))  + 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"))

extractions_flowcell %>% dplyr::filter(u01 == "Olivier_Co_03") %>% 
  ggplot(aes(x = `u01`, y = `260_230`, fill = userid)) + 
  geom_boxplot() + 
  labs(title = paste0("260_280 values by U01")) + 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"))


# extractions_flowcell %>% 
#   ggplot(aes(x = u01, y = `260_230`, color = comments)) + 
#   geom_boxplot() + 
#   facet_grid(~ userid) +
#   labs(title = paste0("260_230 values by U01 and tech"))

