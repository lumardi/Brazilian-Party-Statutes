
library(tidyverse)
library(umap)
library(splitstackshape)
library(vtreat)

#https://datavizpyr.com/how-to-make-umap-plot-in-r/
#https://www.r-bloggers.com/2020/02/a-guide-to-encoding-categorical-features-using-r/

setwd("C:/Users/lucas/Documents/PROJECTS/!Doutorado/Estudos/statute project/5 - SD 2022/dados")

alteracao.df = read_csv("alteracao estatutaria.csv") %>%
  filter(!PARTIDO %in% c("DEM","PSL")) %>%
  select(-contains("FEDERACAO"),-ID,-ARQUIVO) %>%
  cSplit(splitCols = c("FUSAO_VOTO", "INCORPORACAO_VOTO",
                       "DISSOLUCAO_VOTO","ESTATUTO_VOTO"),sep = ",",type.convert=FALSE) 
  

nomes = names(select(alteracao.df,where(is.character),-PARTIDO))

partido = alteracao.df$PARTIDO

alteracao.tz = designTreatmentsZ(alteracao.df, c(nomes)) 

alteracao.df2 = vtreat::prepare(alteracao.tz, alteracao.df, extracols = "target") %>%
  select(-contains("NÃO_MENCIONA"))

alteracao.umap = alteracao.df %>%
  select(-nomes,-PARTIDO) %>%
  bind_cols(alteracao.df2) %>%
  scale() %>% 
  umap(.,controlscale=TRUE,scale=3,labels = partido) %>%
  .$layout %>%
  as.data.frame() %>%
  rename(UMAP1="V1",
         UMAP2="V2") %>%
  mutate(ID=row_number())


alteracao.umap.df = alteracao.df %>%
  mutate(ID = row_number(),
         PARTIDO = partido,
         LOPP = ifelse(PARTIDO == "LOPP","1","0"),
         PARTIDO.FACET = case_when(
           PARTIDO %in% c("AGIR", "AVANTE", "CID") ~ "1",
           PARTIDO %in% c("DC", "MDB","NOVO") ~ "2",
           PARTIDO %in% c("PATRI", "PCB", "PCDOB") ~ "3",
           PARTIDO %in% c("PCO","PDT","PL") ~ "4",
           PARTIDO %in% c("PMB","PMN","PODE") ~ "5",
           PARTIDO %in% c("PP","PROS","PRTB") ~ "6",
           PARTIDO %in% c("PSB","PSC","PSD") ~ "7",
           PARTIDO %in% c("PSDB","PSOL","PSTU") ~ "8",
           PARTIDO %in% c("PT","PTB","PV") ~ "9",
           PARTIDO %in% c("REDE","REP","SD") ~ "10",
           PARTIDO %in% c("UNIAO","UP","LOPP") ~ "11",
           TRUE ~ as.character(PARTIDO))) %>%
#  mutate(PARTIDO.FACET = as.numeric(PARTIDO.FACET)) %>%
  bind_cols(alteracao.df2) %>%
  left_join(alteracao.umap)


# Salvando dados

write_csv(alteracao.umap.df,"alteracao-estatuto-umap.csv")


####################################
alteracao.umap.df %>%
  ggplot() +
  geom_point(aes(x=UMAP1,y=UMAP2,fill=INCORPORACAO_MAJ,
                 color=INCORPORACAO_MAJ)) 


alteracao.umap.df %>%
  ggplot() +
  geom_point(aes(x=UMAP1,y=UMAP2,fill=PARTIDO,
                 color=PARTIDO)) + 
  facet_wrap(~PARTIDO.FACET)


alteracao.umap.df %>%
  mutate(LOPP = ifelse(PARTIDO == "LOPP", "LOPP", NA)) %>%
#  filter(PARTIDO.FACET %in% 4 | PARTIDO == "LOPP") %>%
  filter(PARTIDO %in% c("LOPP","PT","PSDB","MDB","UNIAO","PSOL","PSTU","PSD","PATRI","REP","AVANTE","PCO","SD","PSD","PP","PL")) %>%
  ggplot() +
#  geom_point(aes(x=UMAP1,y=UMAP2,fill=PARTIDO,
#                 color=PARTIDO)) +
  geom_text(aes(x=UMAP1,y=UMAP2,label = PARTIDO)) +
            #  geom_point(aes(x=UMAP1,y=UMAP2,fill=LOPP)) +
  theme_minimal()

  
alteracao.umap.df %>%
  group_by(PARTIDO) %>%
  mutate(UMAP1.AVG = mean(UMAP1),
         UMAP2.AVG = mean(UMAP2),
         LOPP = ifelse(PARTIDO == "LOPP", "LOPP", "PARTIDOS")
         ) %>%
  distinct(UMAP1.AVG,.keep_all = T) %>%
  ggplot(aes(x=UMAP1,y=UMAP2,label = PARTIDO)) +
  #  geom_point(aes(x=UMAP1,y=UMAP2,fill=PARTIDO,
  #                 color=PARTIDO)) +
  geom_text(position = position_dodge(0.8)) +
#  scale_colour_viridis_d(direction = -1) +
#  scale_fill_manual(values = c("#000000","#FF0000")) + 
  #  geom_point(aes(x=UMAP1,y=UMAP2,fill=LOPP)) +
  theme_minimal()


alteracao.umap.df %>%
  group_by(PARTIDO) %>%
  mutate(UMAP1.AVG = mean(FUSAO_QUORUM),
         UMAP2.AVG = mean(FUSAO_QUORUM),
         LOPP = ifelse(PARTIDO == "LOPP", "LOPP", "PARTIDOS")
  ) %>%
  distinct(UMAP1.AVG,.keep_all = T) %>%
  ggplot(aes(x=UMAP1,y=UMAP2,label = FUSAO_QUORUM)) +
  geom_point(aes(x=UMAP1,y=UMAP2,fill=FUSAO_QUORUM,
                   color=FUSAO_QUORUM)) +
#  geom_text(position = position_dodge(0.8)) +
  #  scale_colour_viridis_d(direction = -1) +
  #  scale_fill_manual(values = c("#000000","#FF0000")) + 
  #  geom_point(aes(x=UMAP1,y=UMAP2,fill=LOPP)) +
  theme_minimal()


alteracao.umap.df %>%
  group_by(PARTIDO) %>%
  mutate(UMAP1.AVG = mean(FUSAO_MAJ),
         UMAP2.AVG = mean(FUSAO_MAJ),
         LOPP = ifelse(PARTIDO == "LOPP", "LOPP", "PARTIDOS")
  ) %>%
  distinct(UMAP1.AVG,.keep_all = T) %>%
  ggplot(aes(x=UMAP1,y=UMAP2,label = FUSAO_MAJ)) +
  geom_point(aes(x=UMAP1,y=UMAP2,fill=FUSAO_MAJ,
                 color=FUSAO_MAJ)) +
  theme_minimal()




alteracao.umap.df %>%
  ggplot() +
  geom_point(aes(x=UMAP1,y=UMAP2,fill=PARTIDO,
                 color=PARTIDO)) 



alteracao.umap.df %>%
  ggplot() +
  geom_point(aes(x=UMAP1,y=UMAP2,fill=FUSAO_MAJ,
                 color=FUSAO_MAJ)) 


alteracao.data %>%
  ggplot() +
  geom_point(aes(x=UMAP1,y=UMAP2,fill=ESTATUTO_INSTANCIA,
                 color=ESTATUTO_INSTANCIA)) 


alteracao.data %>%
  ggplot() +
  geom_point(aes(x=UMAP1,y=UMAP2,fill=FUSAO_INSTANCIA,
                 color=FUSAO_INSTANCIA)) 



a = alteracao.umap.df %>%
  group_by(PARTIDO) %>%
  mutate(UMAP1.AVG = mean(PARTIDO),
         UMAP2.AVG = mean(PARTIDO),
         LOPP = ifelse(PARTIDO == "LOPP", "LOPP", "PARTIDOS")) %>%
  ungroup() %>%
  distinct(PARTIDO,.keep_all = T) 
  
a %>%  
  filter(PARTIDO.FACET %in% 11 | PARTIDO == "LOPP") %>%
  ggplot() +
  geom_text(aes(x=UMAP1,y=UMAP2,label = PARTIDO)) +
  xlim(-10,10) +
  ylim(-10,10) +
  #  geom_point(aes(x=UMAP1,y=UMAP2,fill=LOPP)) +
  theme_minimal()

PARTIDO %in% c("AGIR", "AVANTE", "CID") ~ "1",
PARTIDO %in% c("DC", "MDB","NOVO") ~ "2",
PARTIDO %in% c("PATRI", "PCB", "PCDOB") ~ "3",
PARTIDO %in% c("PCO","PDT","PL") ~ "4",
PARTIDO %in% c("PMB","PMN","PODE") ~ "5",
PARTIDO %in% c("PP","PROS","PRTB") ~ "6",
PARTIDO %in% c("PSB","PSC","PSD") ~ "7",
PARTIDO %in% c("PSDB","PSOL","PSTU") ~ "8",
PARTIDO %in% c("PT","PTB","PV") ~ "9",
PARTIDO %in% c("REDE","REP","SD") ~ "10",
PARTIDO %in% c("UNIAO","UP","LOPP") ~ "11",

alteracao.umap.df %>%
  filter(PARTIDO == "PP") %>%
  ggplot() +
  geom_text(aes(x=UMAP1,y=UMAP2,label = PARTIDO)) +
#  xlim(-10,10) +
#  ylim(-10,10) +
  theme_minimal()

