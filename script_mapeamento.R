## Mapeamento interno - março 2025

if(require(tidyverse) == F) install.packages("tidyverse"); require(tidyverse)
if(require(ggplot2) == F) install.packages("ggplot2"); require(ggplot2)
if(require(readxl) == F) install.packages("readxl"); require(readxl)
if(require(openxlsx) == F) install.packages("openxlsx"); require(openxlsx)
if(require(lubridate) == F) install.packages("lubridate"); require(lubridate)
if(require(ggalt) == F) install.packages("ggalt"); require(ggalt)
if(require(gridExtra) == F) install.packages("gridExtra"); require(gridExtra)
if(require(VennDiagram) == F) install.packages("VennDiagram"); require(VennDiagram)
if(require(eulerr) == F) install.packages("eulerr"); require(eulerr)
if(require(ggrepel) == F) install.packages("ggrepel"); require(ggrepel)
if(require(magrittr) == F) install.packages("magrittr"); require(magrittr)
if(require(here) == F) install.packages("here"); require(here)
if(require(janitor) == F) install.packages("janitor"); require(janitor)
if(require(rio) == F) install.packages("rio"); require(rio)
if(require(stringr) == F) install.packages("stringr"); require(stringr)

theme_set(theme_bw())

# Carregando o banco

setwd(here("Banco"))

mapeamento <- read_excel("mapeamento_interno_secult.xlsx")[,c(-1,-2, -5,-6)] %>% rownames_to_column(var="id")

colnames(mapeamento)<- c("id","nome","nascimento","genero","raca","comunidade","pcd","tipo_pcd","escolaridade",
                         "formacao","formacao_outra","setor","setor_outro","funcao","tempo_secult",
                         "municipio","municipio_outro","bairro","vinculo","empresa_terceirizada",
                         "tempo_transporte","meio_transporte","atuacao_gestao_publica","assedio",
                         "bem_estar","saude_mental","saude_mental_outro","satisfacao", "discutir_saude_mental",
                         "relato", "palavra_sentimento")
                      
mapeamento <- mapeamento %>% distinct(nome,.keep_all = TRUE)

# Gênero

mapeamento %>% count(genero)

gênero <-mapeamento %>% count(genero)

ggplot(gênero, aes(x = reorder(genero, - n), y = n)) +
  geom_bar(stat = "identity", width = .6, color = "black", fill = "#716CA7") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(face = "bold")) +
  geom_text(aes(label = paste0(n, " (",
                               scales::percent(n/sum(n)), ")")),
            hjust = 0.5, vjust=-.5) +
  expand_limits(y = c(0, 150)) +
  labs(y = "Colaboradores", x = "Gênero")

setwd (here ("graficos"))

ggsave ("genero_mapeamento.png", height = 6, width = 8)

# Raça

mapeamento %>% count(raca)

raca <-mapeamento %>% count(raca)

ggplot(raca, aes(x = reorder(raca, - n), y = n)) +
  geom_bar(stat = "identity", width = .6, color = "black", fill = "#A0D2DB") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(face = "bold")) +
  geom_text(aes(label = paste0(n, " (",
                               scales::percent(n/sum(n)), ")")),
            hjust = 0.5, vjust=-.5) +
  expand_limits(y = c(0, 100)) +
  labs(y = "Colaboradores", x = "Identidade Étnico Racial")

ggsave ("raça_mapeamento.png", height = 6, width = 8)

# Comunidades

mapeamento %>% count(comunidade)

comunidade <-mapeamento %>% count(comunidade)

ggplot(comunidade, aes (x = reorder(comunidade, n), y = n))+
  geom_bar(stat = "identity", width = .6, color = "black", fill = "#A71C1F") +
  coord_flip()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(face = "bold")) +
  geom_text(aes(label = paste0(n, " (",
                               scales::percent(n/sum(n)), ")")),
            hjust = - 0.1) +
  expand_limits(y = c(0, 250)) +
  labs(y = "Colaboradores", x = "Comunidade")

ggsave ("comunidade_mapeamento.png", height = 6, width = 8)

#PCD

pcd <- mapeamento %>% count(pcd)

# Não fizemos gráfico

table (mapeamento$tipo_pcd)

#Escolaridade

escolaridade <-mapeamento %>% count(escolaridade)

ggplot(escolaridade, aes(x = reorder(escolaridade, n), y = n)) +
  geom_bar(stat = "identity", width = .6, color = "black", fill = "#FCCD00") +
  coord_flip()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(face = "bold")) +
  geom_text(aes(label = paste0(n, " (",
                               scales::percent(n/sum(n)), ")")),
            hjust = - 0.1, vjust=-.1) +
  expand_limits(y = c(0, 120)) +
  labs(y = "Colaboradores", x = "Escolaridade")

ggsave ("escolaridade_mapeamento.png")

#Formação

mapeamento %>% count(formacao)

formacao <- mapeamento %>% count(formacao) %>% arrange(-n) %>% filter(is.na(formacao)==FALSE) %>% 
  rownames_to_column(var = "ranking") %>% 
  mutate(formacao=ifelse(n<4, "Outra", formacao)) %>% group_by(formacao) %>% summarise(n=sum(n))

ggplot(formacao, aes(x = reorder(formacao,n), y = n)) +
  geom_segment(aes(xend=formacao, yend = 0))+
  geom_point(size=2,color="#623E3E") +
  coord_flip()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(face = "bold")) +
  geom_text(aes(label = paste0(n, " (",
                               scales::percent(n/sum(n)), ")")),
            hjust = - 0.1, vjust=-.1) +
  expand_limits(y = c(0, 90)) +
  labs(y = "Colaboradores", x = "Cursos")

ggsave ("formacao.png", height = 6, width = 8)

# Setores

setores <- mapeamento %>% count(setor)%>%
  mutate(setor= gsub(".* - ", "", setor),
         setor=ifelse(setor== "Observatório de Indicadores Culturais (ObIC)", "ObIC", 
                      setor))

ggplot(setores, aes(x = reorder(setor,n), y = n)) +
  geom_segment(aes(xend=setor, yend = 0))+
  geom_point(size=2,color="#623E3E") +
  coord_flip()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(face = "bold")) +
  geom_text(aes(label = paste0(n, " (",
                               scales::percent(n/sum(n)), ")")),
            hjust = - 0.1, vjust=-.1) +
  expand_limits(y = c(0, 50)) +
  labs(y = "Colaboradores", x = "Setor")

ggsave ("setores_mapeamento.png", height = 6, width = 8)

# Tempo de trabalho na Secult

tempo <- mapeamento %>% count(tempo_secult) %>% 
  mutate(tempo_secult = factor(tempo_secult, levels = c("Menos de 1 ano", "Entre 1 e 3 anos",
                                                        "Entre 3 e 6 anos", "Entre 6 e 10 anos",
                                                        "Mais de 10 anos")))

ggplot(tempo, aes(x = tempo_secult, y = n)) +
  geom_bar(stat = "identity", width = .6, color = "black", fill = "#8D35A1") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(face = "bold")) +
  geom_text(aes(label = paste0(n, " (",
                               scales::percent(n/sum(n)), ")")),
            hjust =0.5, vjust=-0.6) +
  expand_limits(y = c(0, 150)) +
  labs(y = "Colaboradores", x = "Tempo que está trabalhando na Secult")

ggsave ("tempo_de_trabalho_secult_mapeamento.png", height = 6, width = 8)


# Vínculo com Secult 

mapeamento %>% count(vinculo)

vinculo <- mapeamento %>% count(vinculo)

ggplot(vinculo, aes(x = reorder(vinculo, -n), y = n)) +
  geom_bar(stat = "identity", width = .6, color = "black", fill = "#3D00E5") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(face = "bold")) +
  geom_text(aes(label = paste0(n, " (",
                               scales::percent(n/sum(n)), ")")),
            hjust =0.6, vjust=-0.3) +
  expand_limits(y = c(0, 180)) +
  labs(y = "Colaboradores", x = "Tipo de vínculo")

ggsave ("vinculo_mapeamento.png", height = 6, width = 8)

#Empresas terceirizadas 

mapeamento <- read_xlsx("mapeamento_interno_secult.xlsx") 

terceirizadas <- mapeamento %>% count(tercerizado_empresa)%>% 
  drop_na()

theme_set(theme_bw())

g <- ggplot(terceirizadas, aes(x = reorder(tercerizado_empresa, n), y = n)) +
  geom_bar(stat = "identity", width = .6, color = "black", fill = "#84E27C") +
  coord_flip() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(face = "bold")) +
  geom_text(aes(label = paste0(n, " (",
                               scales::percent(n/sum(n)), ")")),
            hjust = -.2) +
  expand_limits(y = c(0, 65)) +
  labs(y = "Colaboradores", x = "Empresas Terceirizadas")
g

ggsave("EMPRESA_MAPEAMENTO.png", height = 6, width = 8)

#Município

mapeamento %>% count(município)

municipio <- mapeamento %>% count(município) %>% 
  filter(n >= 4)

ggplot(municipio, aes(x = reorder(município, n), y = n)) +
  geom_segment(aes(xend=município, yend = 0))+
  geom_point(size=3,color="#752023")+
  coord_flip()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(face = "bold")) +
  geom_text(aes(label = paste0(n, " (",
                               scales::percent(n/sum(n)), ")")),
            hjust =-0.2, vjust=-0.3) +
  expand_limits(y = c(0, 180)) +
  labs(y = "Colaboradores", x = "Município")

ggsave ("municipio_mapeamento.png", height = 6, width = 8)

#Meio de transporte

mapeamento %>% count(meio_de_transporte)

transporte <- mapeamento %>% count(meio_de_transporte)

ggplot(transporte, aes(x = reorder(meio_de_transporte, n), y = n))+
  geom_bar(stat = "identity", width = .6, color = "black", fill = "#FFAFBA") +
  coord_flip()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(face = "bold")) +
  geom_text(aes(label = paste0(n, " (",
                               scales::percent(n/sum(n)), ")")),
            hjust =-0.1, vjust=-0.3) +
  expand_limits(y = c(0, 130)) +
  labs(y = "Colaboradores", x = "Meio de transporte utilizados pelos funcionários")

ggsave ("meio_de_transporte_mapeamento.png", height = 6, width = 8)

# Atuação na gestão pública

atuacao <- mapeamento %>% count(atuação_gestão_publica) %>% 
  mutate(atuação_gestão_publica = factor(atuação_gestão_publica, levels = c("Menos de 1 ano", "Entre 1 e 3 anos",
                                                                            "Entre 3 e 6 anos", "Entre 6 e 10 anos",
                                                                            "Mais de 10 anos")))

ggplot(atuacao, aes(x = atuação_gestão_publica, y = n)) +
  geom_bar(stat = "identity", width = .6, color = "black", fill = "#0C2856") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(face = "bold")) +
  geom_text(aes(label = paste0(n, " (",
                               scales::percent(n/sum(n)), ")")),
            hjust =0.5, vjust=-0.6) +
  expand_limits(y = c(0, 70)) +
  labs(y = "Colaboradores", x = "Tempo de atuação na gestão pública")

ggsave ("tempo_de_atuação_gestão_pública_mapeamento.png", height = 6, width = 8)

#Idade

mapeamento$idade <- floor(as.numeric(difftime(Sys.Date(), mapeamento$data_nascimento, units = "days")) / 365.25)

idade<- mapeamento %>% count (idade)

g2 <- ggplot(mapeamento, aes(idade)) +
  geom_histogram(binwidth = 2, color = "black", fill = "#C49D79") + 
  scale_x_continuous(breaks = c(seq(from = 17, to = 75, by = 2)), limits = c(17, 75))+
  theme(panel.grid = element_blank()) +
  labs(x = "Idade",
       y = "Colaboradores",
       caption = "") +
  theme(plot.caption = element_text(hjust = 1))+
  theme(panel.grid = element_blank(),
        axis.title = element_text(face = "bold")) 

g2

min(mapeamento$idade)
max(mapeamento$idade)
mean(mapeamento$idade)
sd(mapeamento$idade)

nrow(mapeamento %>% filter(idade >= 60))

ggsave ("idade_mapeamento.png", height = 6, width = 8)



# Tempo para chegar a secult

tempo <- mapeamento %>% count(tempo_para_chegar_secult)

tempo$fraction <- tempo$n/sum(tempo$n)

tempo$ymax <-cumsum (tempo$fraction)

tempo$ymin <- c(0, head(tempo$ymax, n=-1))

tempo$labelPosition <- (tempo$ymax+tempo$ymin)/2

tempo$label <- paste0(tempo$tempo_para_chegar_secult, "\n  ")


ggplot(tempo) +
  geom_rect(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = tempo_para_chegar_secult)) +
  geom_text(x =2, aes(y = labelPosition, label = paste0("Colaboradores: ", n),
                      color = tempo_para_chegar_secult, bold), size = 5)+
  scale_fill_brewer(palette = 3) +
  scale_color_brewer(palette =3) +
  coord_polar(theta = "y") +
  xlim(c(-1, 4)) +
  theme_void() +
  labs(x = "Tempo para chegar à Secult-PE",
       y = "Colaboradores",
       caption = "")+
  guides(fill = guide_legend(title = "Tempo para chegar à Secult"), 
         color = guide_legend(title = "Tempo para chegar à Secult"))


tempo <- mapeamento %>% count(tempo_para_chegar_secult) %>% 
  mutate(tempo_para_chegar_secult = factor(tempo_para_chegar_secult, levels =
                                             c("Menos de 20 minutos", "Entre 20 e 40 minutos", "Entre 40 e 1h",
                                               "Mais de 1h")))

ggplot(tempo, aes(x = tempo_para_chegar_secult, y = n)) +
  geom_bar(stat = "identity", width = .6, color = "black", fill = "#88419D") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(face = "bold")) +
  geom_text(aes(label = paste0(n, " (",
                               scales::percent(n/sum(n)), ")")),
            hjust =0.5, vjust=-0.6) +
  expand_limits(y = c(0, 80)) +
  labs(y = "Colaboradores", x = "Tempo para chegar a Secult-PE")

ggsave ("tempo_para_chegar_secult_mapeamento.png", height = 6, width = 8)


