### GESTÃO DE RISCOS NA AVALIAÇÃO DE PROGRAMAS DE PÓS-GRADUAÇÃO
### André Luiz Brasil Varandas Pinto
### andre.brasil@capes.gov.br

##############################
# Instala pacotes necessários
##############################

install.packages("tidyverse")
install.packages("plyr")
install.packages("caret")
install.packages("leaps")
install.packages("DescTools")
install.packages("mlogit")
install.packages("rms")
install.packages("snakecase")
install.packages("Hmisc")
install.packages("lubridate")
install.packages("scales")
install.packages("randomForest")
install.packages("party")
install.packages("rpart")
install.packages("lubridate")
install.packages("broom")
install.packages("ltm")
install.packages("ggpubr")
install.packages("pROC")
install.packages("plotROC")
install.packages("binr")
install.packages("Hmisc")
install.packages("naivebayes")
install.packages("GGally")
install.packages("corrplot")
install.packages("partykit")

#---- Atualiza pacotes

tidyverse_update()


##############################
# Carrega pacotes
##############################

library(plyr)
library(caret)
library(leaps)
library(DescTools)
library(mlogit)
library(rms)
library(snakecase)
library(data.table)
library(Hmisc)
library(psych)
library(lubridate)
library(scales)
library(readxl)
library(randomForest)
library(party)
library(rpart)
library(lubridate)
library(broom)
library(ltm)
library(tidyverse)
library(ggpubr)
library(pROC)
library(plotROC)
library(binr)
library(Hmisc)
library(naivebayes)
library(GGally)
library(corrplot)
library(partykit)

##############################
# Bases de dados
##############################

#---- limpa o ambiente de variáveis para a execução do R

rm(list=ls()) 

#---- lendo as primeiras bases de dados

PPGs <- read_excel("Dados/DadosPPGs.xlsx", col_names = TRUE)
MunicipiosIBGE <- read_excel("Dados/municipios_ibge.xlsx", col_names = TRUE)
IES <- read_excel("Dados/dados_ies.xlsx", col_names = TRUE)
AtlasBrasil <- read_excel("Dados/atlas_brasil.xlsx", col_names = TRUE)
AreasAvaliacao <- read_excel("Dados/AreasAvaliacao.xlsx", col_names = TRUE)
ConsultaCGEE <- read_excel("Dados/ConsultaCGEE.xlsx", col_names = TRUE)
DocentesMinimo <- read_excel("Dados/DocentesMinimo.xlsx", col_names = TRUE)
IDEB2015 <- read_excel("Dados/IDEB2015.xlsx", col_names = TRUE)

#---- calculando populações por recortes regionais

Temp.populacao <- left_join(MunicipiosIBGE, AtlasBrasil, by = c("Lugar" = "Lugar"), suffix = c("","_2"))
colnames(Temp.populacao)[22] <- "PopulacaoTotal2010"
Temp.populacao <- Temp.populacao %>%
                    group_by(cod_rgi) %>%
                    mutate(PopulacaoRGI = sum(PopulacaoTotal2010, na.rm = TRUE)) %>%
                    group_by(cod_rgint) %>%
                    mutate(PopulacaoRGInt = sum(PopulacaoTotal2010, na.rm = TRUE)) %>%
                    ungroup()
Temp.populacao$MunicípioCDCompleto <- as.numeric(Temp.populacao$MunicípioCDCompleto)
Temp.populacao <- dplyr::select(Temp.populacao, c(9,35:36))

#---- fazendo um join de colunas de duas tabelas

Temp1 <- left_join(PPGs, IES, by = c("IES_CD" = "IesCd"), suffix = c("","_2"))
Temp2 <- left_join(Temp1, MunicipiosIBGE, by = c("Local" = "Lugar"), suffix = c("","_2"))
Temp3 <- left_join(Temp2, AtlasBrasil, by = c("Local" = "Lugar"), suffix = c("","_2"))
Temp4 <- left_join(Temp3, Temp.populacao, by = c("MunicipioCD" = "MunicípioCDCompleto"), suffix = c("","_2"))
Temp5 <- left_join(Temp4, AreasAvaliacao, by = c("Área de Avaliação" = "Área de Avaliação"), suffix = c("","_2"))
Temp6 <- left_join(Temp5, ConsultaCGEE, by = c("Código do Programa" = "Código do Programa"), suffix = c("","_2"))

##############################
# Limpeza dos dados
##############################

#---- excluir colunas duplicadas 

Temp6 <- data.table(Temp6)
drop.cols <- grep("\\_2$", colnames(Temp6))
Dados <- Temp6[, (drop.cols) := NULL]

#---- alterar nomes de colunas para formato válido para as operações que serão realizadas

clean_names <- function(dat, case = c("snake", "small_camel", "big_camel", "screaming_snake", 
                                      "parsed", "mixed", "lower_upper", "upper_lower",
                                      "all_caps", "lower_camel", "upper_camel", "old_janitor")) {
  case <- match.arg(case)
  if (case == "old_janitor"){
    return(old_clean_names(dat))  
  }
  old_names <- names(dat)
  new_names <- old_names %>%
    gsub("'", "", .) %>% 
    gsub("\"", "", .) %>% 
    gsub("%", ".perc", .) %>% 
    gsub("#", ".n_", .) %>%
    gsub("^[[:space:][:punct:]]+", "", .) %>% 
    make.names(.) %>%
    snakecase::to_any_case(case = case, preprocess = "\\.", 
                           replace_special_characters = c("Latin-ASCII"))
  dupe_count <- vapply(1:length(new_names), function(i) { 
    sum(new_names[i] == new_names[1:i]) }, integer(1))
  
  new_names[dupe_count > 1] <- paste(new_names[dupe_count > 1],
                                     dupe_count[dupe_count > 1],
                                     sep = "_")
  stats::setNames(dat, new_names)
}

Dados <- clean_names(Dados, "upper_camel")
DocentesMinimo <- clean_names(DocentesMinimo, "upper_camel")
IDEB2015 <- clean_names(IDEB2015, "upper_camel")

#---- segunda rodade de exclusão de colunas duplicadas, a partir da padronização de nomes

Dados <- data.table(Dados)
drop.cols <- grep("\\_2$", colnames(Dados))
Dados <- Dados[, (drop.cols) := NULL]

#---- Excluindo colunas desnecessárias

Dados <- subset(Dados, select = -c(Espacialidade,CodIbge))

#---- ordenar linhas

Dados <- Dados[order(Dados$Uf,-Dados$Municipio)]

#---- ajustando DocentesMinimo, pois temos variáveis exibidas como valores 

head(DocentesMinimo) # verifica situação
DocentesMinimo <- spread(DocentesMinimo, Minimo, -AreaDeAvaliacao) # ajuste necessário
head(DocentesMinimo) # confirma alteração

#---- fazendo um join de colunas de duas tabelas, após ajuste realizado

Dados <- left_join(Dados, DocentesMinimo, by = c("AreaDeAvaliacao" = "AreaDeAvaliacao"), suffix = c("","_2"))

#---- ajustando IDEB, para cálculo por município

IDEB2015 <- IDEB2015 %>%
  filter(., !is.na(Ideb2015)) %>%
  group_by(CodigoMunicipio) %>%
  mutate(MediaIDEB = round(mean(Ideb2015), 2))

#---- criando recorte por Município, para mesclagem

IDEB.Lugar <- IDEB2015 %>%
  group_by(CodigoMunicipio)%>%
  dplyr::select(MediaIDEB)%>%
  unique()

summary(IDEB.Lugar)

#---- fazendo um join de colunas de duas tabelas, após ajuste realizado

Dados <- left_join(Dados, IDEB.Lugar, by = c("MunicipioCd" = "CodigoMunicipio"), suffix = c("","_2"))

#---- exporta para Excel

write_excel_csv(IDEB2015, path = "Dados/IDEB2015.csv", na = "")

#---- elimina espaços no início e no final dos registros

str_trim(Dados)


##############################
# Campos calculados
##############################

#---- cria detalhamento de categoria de IES, baseada em dados do e-MEC

Dados <- Dados %>%
  mutate(NaturezaJuridicaDetalhada = ifelse(is.na(Comunitaria) & is.na(Filantropica) & is.na(Confessional), NaturezaJuridicaMec,
                                            ifelse(Confessional == "SIM", "Confessional", NA)))
Dados$NaturezaJuridicaDetalhada[is.na(Dados$NaturezaJuridicaDetalhada)] <- "Filantrópica ou Comunitária"
summary(factor(Dados$NaturezaJuridicaDetalhada))

#---- calcula risco de descontinuidade

Dados <- Dados %>%
  mutate(Descontinuidade = ifelse(NotaFinal < 3 | !is.na(DoutoradoDescredenciado), 1,0))

#---- calcula risco de involução

Dados <- Dados %>%
  mutate(Involucao = if_else(NotaFinal > 2 & is.na(DoutoradoDescredenciado) & NotaFinal < 7,
                             if_else(NotaFinal < Nota2013 | NotaFinal < Nota2010 | NotaFinal < Recomend, 1, 0, missing = 0),0))

#---- calcula risco de estagnação

Dados <- Dados %>%
  mutate(Estagnacao = if_else(NotaFinal == 4 & NotaFinal == Nota2013 & NotaFinal == Nota2010 & NotaFinal <= Nota2007, 1, 
                               if_else(NotaFinal == 3 & NotaFinal == Nota2013 & NotaFinal == Nota2010, 1, 0, missing = 0), missing = 0))

#---- consolida os três tipos de risco identificados em um indicador de risco

Dados <- Dados %>%
  mutate(Risco = if_else(Estagnacao == 1 | Involucao == 1 | Descontinuidade == 1, 1,0)) 

#---- calcula tipo de estagnação

Dados <- Dados %>%
  mutate(EstagnadoTipo = if_else(Estagnacao == 1, if_else(NotaFinal == 4, "4x4", "3x3"),""))

#---- nota que o PPG tinha antes da Avaliação Quadrienal 2017

Dados <- Dados %>%
  mutate(NotaAnterior = ifelse(is.na(Recomend), Nota2013, Recomend))

#---- variação de nota em relação à nota anterior

Dados <- Dados %>%
  mutate(VariacaoNota = NotaFinal-NotaAnterior)

#---- calcula número de PPGs por município

Dados <- Dados %>%
  group_by(Local) %>%
  mutate(PPGsPorMunicipio = n()) 

#---- calcula número de PPGs por região geográfica imediata

Dados <- Dados %>%
  group_by(RegiaoGeograficaImediata) %>%
  mutate(PPGsPorRGI = n()) 

#---- calcula número de PPGs por região geográfica intermediária

Dados <- Dados %>%
  group_by(RegiaoGeograficaIntermediaria) %>%
  mutate(PPGsPorRGInt = n()) 

#---- calcula número de PPGs por UF

Dados <- Dados %>%
  group_by(Uf) %>%
  mutate(PPGsPorUF = n())

#---- calcula percentual de risco identificado por UF  

Dados <- Dados %>%
  group_by(Uf) %>%
  mutate(PercRiscoPorUf = sum(Risco == '1', na.rm = TRUE)/n())

#---- calcula percentual de risco identificado por município

Dados <- Dados %>%
  group_by(Local) %>%
  mutate(PercRiscoPorMunicipio = sum(Risco == '1', na.rm = TRUE)/n())

#---- calcula percentual de risco identificado por Região

Dados <- Dados %>%
  group_by(Regiao) %>%
  mutate(PercRiscoPorRegiao = sum(Risco == '1', na.rm = TRUE)/n())

#---- calcula percentual de risco identificado por Região Geográfica Imediata

Dados <- Dados %>%
  group_by(CodRgi) %>%
  mutate(PercRiscoPorRGI = sum(Risco == '1', na.rm = TRUE)/n())

#---- calcula percentual de risco identificado por Região Geográfica Intermediária

Dados <- Dados %>%
  group_by(CodRgint) %>%
  mutate(PercRiscoPorRGInt = sum(Risco == '1', na.rm = TRUE)/n())

#---- calcula número de PPGs por IES

Dados <- Dados %>%
  group_by(IesSiglaAgrupada) %>%
  mutate(PPGsPorIES = n())

#---- calcula média ponderada das notas obtidas pelos PPGs de cada IES 

Dados <- Dados %>%
  group_by(IesSiglaAgrupada) %>%
  mutate(MediaPonderadaNotas = round(sum(NotaFinal/n()), 2))

#---- calcula proporção de notas 5, 6 e 7 para cada IES

Dados <- Dados %>%
  group_by(IesSiglaAgrupada) %>%
  mutate(ProporcaoNotas567 = sum(NotaFinal>4)/n())

#---- desagrupando Tibble

Dados%>%
  ungroup()

#---- função de cáculo de idade

calc_age <- function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period <- as.period(interval(birthDate, refDate),
                      unit = "year")
  period$year
}

#---- ajusta formato dos campos de data de início

Dados$DataInicioMestrado <- as.Date.POSIXct(Dados$DataInicioMestrado) 
Dados$DataInicioDoutorado <- as.Date.POSIXct(Dados$DataInicioDoutorado) 

#---- calcula idade do PPG com referência no último dia do quadriênio: 31/12/2016

Dados$IdadePPG <- ifelse(is.na(Dados$DataInicioDoutorado), 
                         calc_age(Dados$DataInicioMestrado, "2016-12-31"),
                         ifelse(is.na(Dados$DataInicioMestrado),
                                calc_age(Dados$DataInicioDoutorado, "2016-12-31"),
                                ifelse(calc_age(Dados$DataInicioMestrado)>calc_age(Dados$DataInicioDoutorado),
                                       calc_age(Dados$DataInicioMestrado, "2016-12-31"),
                                       calc_age(Dados$DataInicioDoutorado, "2016-12-31")
                                       )
                                )
                         )

##############################
# Dados adicionais
# Censo 2010 (Educação)
##############################

#---- lendo e manipulando planilhas do Censo 2010 - Educação

ArquivosCenso <- dir(path = "Dados/Censo2010_Educação/", pattern = "*.xls", recursive = TRUE, full.names = TRUE)
tipo.coluna.censo <- c("text", "text", "numeric", "numeric", "numeric", "numeric",
                 "numeric", "numeric", "numeric", "numeric", "numeric", "text")
lista.censo <- lapply(ArquivosCenso, read_xls, sheet = 1, col_names = TRUE, na = "", skip = 5, col_types = tipo.coluna.censo)
Censo.temp <- bind_rows(lista.censo)
Censo.temp <- dplyr::select(Censo.temp, c(1:3,10:12))
colnames(Censo.temp) <- c("UF", "Municipio", "Populacao", "Graduados", "PosGraduados", "CodMunicipio")

# juntar com planilha de municípios
Censo.temp <- left_join(Censo.temp, MunicipiosIBGE, by = c("CodMunicipio" = "MunicípioCDCompleto"), suffix = c("","_2"))

# calcular proporções de graduados e pós-graduados por recortes regionais
Censo.temp <- Censo.temp %>%
                mutate(PropPosGraduadosMun = PosGraduados / Populacao)

Censo.temp <- Censo.temp %>%
                group_by(cod_rgi) %>%
                mutate(PropPosGraduadosRGI = sum(PosGraduados, na.rm = TRUE) / sum(Populacao))

Censo.temp <- Censo.temp %>%
                group_by(cod_rgint) %>%
                mutate(PropPosGraduadosRGInt = sum(PosGraduados, na.rm = TRUE) / sum(Populacao))%>%
                ungroup()

Censo.temp <- Censo.temp %>%
  mutate(PropSuperiorMun = (rowSums(.[,c("Graduados", "PosGraduados")], na.rm=TRUE)) / Populacao)

Censo.temp <- Censo.temp %>%
  group_by(cod_rgi) %>%
  mutate(PropSuperiorRGI = (sum(Graduados, na.rm = TRUE) + sum(PosGraduados, na.rm = TRUE)) / sum(Populacao))

Censo.temp <- Censo.temp %>%
  group_by(cod_rgint) %>%
  mutate(PropSuperiorRGInt = (sum(Graduados, na.rm = TRUE) + sum(PosGraduados, na.rm = TRUE))/ sum(Populacao))%>%
  ungroup()

# manter apenas colunas relevantes
Censo.temp <- dplyr::select(Censo.temp, c(6,21:26))

# mesclar com base principal
Censo.temp$CodMunicipio <- as.numeric(Censo.temp$CodMunicipio)
Dados <- left_join(Dados, Censo.temp, by = c("MunicipioCd" = "CodMunicipio"), suffix = c("","_2"))
any(is.na(Dados$PropPosGraduadosRGI)) 


##############################
# Dados adicionais
# Planilha das áreas
##############################

#---- lendo as planilhas de docentes das áreas

Arquivos <- dir(path = "Dados/PlanilhasAreas/", pattern = "*.xlsx", recursive = TRUE, full.names = TRUE)
tipo.coluna <- c("numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text",
                 "text", "numeric", "numeric", "text", "numeric", "text", "text", "text", "text", "guess", 
                 "text", "text", "numeric", "text", "text", "text", "text", "text", "text", "text", 
                 "text", "text", "text", "text", "text", "text", "text", "numeric", "text", "text", 
                 "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                 "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                 "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "skip", "skip", "skip", 
                 "skip", "skip", "skip", "skip")

lista.areas <- lapply(Arquivos, read_xlsx, sheet = 3, col_names = TRUE, na = "", skip = 5, col_types = tipo.coluna)
Docentes.temp <- bind_rows(lista.areas)

#---- limpa nomes da planilha temporária de docentes

Docentes.temp<- clean_names(Docentes.temp, "upper_camel")

#---- recorte de dados para análise

Docentes.perm <- subset(Docentes.temp, Categoria == "PERMANENTE" & AnoBase == 2016)

#---- ajusta formato dos campos de data 

Docentes.perm$DataDeNascimento <- dmy(Docentes.perm$DataDeNascimento) 
Docentes.perm$Inicio <- dmy(Docentes.perm$Inicio) 
Docentes.perm$Fim <- dmy(Docentes.perm$Fim) 
Docentes.perm$InicioDaBolsa <- as.yearmon(Docentes.perm$InicioDaBolsa, "%m/%Y")
Docentes.perm$FimDaBolsa <- as.yearmon(Docentes.perm$FimDaBolsa, "%m/%Y") 
any(is.na(Docentes.perm$DataDeNascimento))


##############################
# Novos campos calculados
##############################

#---- calcula densidade de PPGs por população com nível superior

Dados <- Dados %>%
  ungroup() %>%
  mutate(DensidadePPGMun = round((PPGsPorMunicipio/(PropSuperiorMun*PopulacaoTotal2010))*100000),6) %>%
  mutate(DensidadePPGRGI = round((PPGsPorRGI/(PropSuperiorRGI*PopulacaoRgi))*100000),6) %>%
  mutate(DensidadePPGRGInt = round((PPGsPorRGInt/(PropSuperiorRGInt*PopulacaoRgInt))*100000),6)
  
any(is.na(Dados$DensidadePPGMun))
any(is.na(Dados$DensidadePPGRGI))
any(is.na(Dados$DensidadePPGRGInt))

#---- calcula idade do docente com referência no último dia do quadriênio: 31/12/2016

Docentes.perm$IdadeDocente <- calc_age(Docentes.perm$DataDeNascimento, "2016-12-31")
any(is.na(Docentes.perm$IdadeDocente))

#---- calcula tempo de titulação do docente com referência no último dia do quadriênio: 31/12/2016

Docentes.perm$TempoTitulacao <- 2016 - Docentes.perm$AnoTitulacao

#---- calcula número de docentes permanentes com referência no último dia do quadriênio: 31/12/2016

Docentes.perm <- Docentes.perm %>%
  group_by(CodPpg) %>%
  mutate(Permanentes = n())

#---- sumário e histograma de idade dos docentes

summary(Docentes.perm$IdadeDocente)
ggplot(data=Docentes.perm, aes(Docentes.perm$IdadeDocente)) + 
  geom_histogram(breaks=seq(20, 90, by = 5), 
                 col="black", 
                 aes(fill=..count..),
                 alpha = .8) + 
  scale_fill_gradient("Docentes")+
  scale_y_continuous(breaks=seq(0,14000,by=2000), limits=c(0,14000))+
  labs(title="Histograma de idade dos docentes permanentes") +
  labs(x="Idade", y="Número de docentes")

#---- Categoriza docentes como júnior ou senior, com referência no último dia do quadriênio: 31/12/2016

Docentes.perm$CategoriaDocenteIdade <- ifelse(Docentes.perm$MaiorNivel == "DOUTORADO", 
                                              ifelse(Docentes.perm$IdadeDocente >= 65, "Senior", 
                                                     ifelse(Docentes.perm$TempoTitulacao <= 5, "Junior", "Pleno")), 
                                              "SemDoutorado")
any(is.na(Docentes.perm$CategoriaDocenteIdade))

#---- calcula proporção de docentes senior para cada PPG

Docentes.perm <- Docentes.perm %>%
  group_by(CodPpg) %>%
  mutate(PropSenior = sum(CategoriaDocenteIdade == "Senior")/n())

#---- calcula proporção de docentes junior para cada PPG

Docentes.perm <- Docentes.perm %>%
  group_by(CodPpg) %>%
  mutate(PropJunior = sum(CategoriaDocenteIdade == "Junior")/n())

#---- calcula proporção de docentes senior e júnior para cada PPG

Docentes.perm <- Docentes.perm %>%
  group_by(CodPpg) %>%
  mutate(PropSeniorJunior = sum(CategoriaDocenteIdade == "Senior" | CategoriaDocenteIdade == "Junior")/n())
any(is.na(Docentes.perm$PropSeniorJunior))

#---- criando recorte de docentes, para exportar para Tableau

Dados.Docentes <- Docentes.perm %>%
  group_by(NomeDoDocente) %>%
  dplyr::select(CodPpg, IdadeDocente, Sexo, MaiorNivel, TempoTitulacao, PaisDaIes, Vinculo, CategoriaDocenteIdade)%>%
  unique()

#---- criando recorte por PPG, para mesclagem

Docentes.PPG <- Docentes.perm%>%
  group_by(CodPpg)%>%
  dplyr::select(Permanentes, PropSenior, PropJunior, PropSeniorJunior)%>%
  unique()

#---- mescla dados dos docentes com planilha principal

Dados <- left_join(Dados, Docentes.PPG, by = c("CodigoDoPrograma" = "CodPpg"), suffix = c("","_2"))

#---- verifica se todos os PPGs receberam dados da planilha de docentes

any(is.na(Dados$Permanentes))
sum(is.na(Dados$Permanentes)) # identificados 5 PPGs sem dados de permanentes

# vamos verificar quais PPGs não tinham dados de permanentes nas planilhas de área

Dados[which(is.na(Dados$Permanentes)),c(1:3,7)] 

# Listados os 5 PPGs, foi feita consulta na Plataforma Sucupira, identificando que programas 
# sem dados de docentes não informaram atividade em 2016. Logo, podemos completar os campos 
# com o número de fato observado: zero.

Dados$Permanentes[is.na(Dados$Permanentes)] <- 0


#---- calcula relação de docentes do PPG com mínimo exigigo área

Dados$PermanentesPorMinimo <- round(ifelse(Dados$Nivel == "Mestrado", Dados$Permanentes / Dados$MinimoDocentesME,
                                     ifelse(Dados$Nivel == "Mestrado Profissional", Dados$Permanentes / Dados$MinimoDocentesMP,
                                            Dados$Permanentes / Dados$MinimoDocentesDO)), 2)


##############################
# Limpeza do ambiente 
# Exportação dos dados
##############################

#---- limpa bases temporárias

rm(list = grep("Dados", ls(), value = TRUE, invert = TRUE)) #-- mantém somente base "Dados"

#---- analisa estrutura dos Tibbles

glimpse(Dados)
summary(Dados)

glimpse(Dados.Docentes)
summary(Dados.Docentes)

#----- altera formato de campos selecionados

Dados.Docentes <- select(Dados.Docentes, CodPpg, NomeDoDocente, IdadeDocente, TempoTitulacao, Sexo:CategoriaDocenteIdade)
Dados.Docentes$Sexo <- factor(Dados.Docentes$Sexo)
Dados.Docentes$MaiorNivel <- factor(Dados.Docentes$MaiorNivel)
Dados.Docentes$Vinculo <- factor(Dados.Docentes$Vinculo)
Dados.Docentes$PaisDaIes <- factor(Dados.Docentes$PaisDaIes)
Dados.Docentes$CategoriaDocenteIdade <- factor(Dados.Docentes$CategoriaDocenteIdade)

summary(Dados.Docentes)

#---- exporta para Excel

write_excel_csv(Dados, path = "Dados/DadosR.csv", na = "")
write_excel_csv(Dados.Docentes, path = "Dados/DadosDocentes.csv", na = "")


##############################
# Análise dos indicadores
##############################

#---- plotar exemplo de correlação linear

a <- c(1,2,3,4,5,5,6,7,10,4,8)
b <- c(4,5,6,8,8,4,10,12,15,9,12)
c <- c(1,1,1,1,0,1,0,1,0,1,0,1,1,0,0,0,0,0,0,0,1,0)
d <- c(4,5,6,8,8,4,10,12,15,9,12,14,18,13,15,13,16,16,17,19,10,18)
data <- data.frame(a, b)
data2 <- data.frame(c, d)
cor(a,b) # 0.8954361
cor(c,d) #-0.5783678

ggplot(data, aes(x=a, y=b)) + 
  geom_point(size=3, color="red", fill="orange", shape=21) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(y="Variável a", x = "Variável b") + 
  annotate("text", x=8.8, y=0.3, label = "Correlação == 0.8954361", parse=T)

ggplot(data2, aes(x=c, y=d)) + 
  geom_point(size=3, color="red", fill="orange", shape=21, alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(y="", x = "") + 
  annotate("text", x=0.5, y=0.3, label = "Correlação == -0.5783678", parse=T)+
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())

# ---- exemplo de regressão logística
ExemploLogit <- glm(factor(c) ~ d, 
                    family = binomial(link = "logit"), data = data2)
summary(ExemploLogit)
exp(coef(ExemploLogit))

ggplot(data2, aes(x=d, y=c)) + 
  geom_jitter(width = 0.0, height = 0.05, size=3, color="red", fill="orange", shape=21, alpha = 0.7) + 
  geom_smooth(method = "glm",se = FALSE, method.args = structure(list(family = "binomial"), .Names = "family")) +
  geom_line(stat = "smooth", method = "lm", se = FALSE, color = "red", alpha = 0.3) +
  labs(y="Prob (y = 1)", x = expression(x[i]))

# alteração necessária para permitir plotagem dos pontos no boxplot
data2$c[data2$c == 1] <- "Risco Observado" 
data2$c[data2$c == 0] <- "Sem Risco"
data2$c <- factor(data2$c)
data2$c <- with(data2, relevel(data2$c, "Sem Risco"))

#---- gera boxplot a partir dos dados dicotômicos

test <- wilcox.test(data2$d ~ data2$c)
pvalue <- test$p.value
biserialcor <- biserial.cor(data2$d, data2$c, use = c("complete.obs"), level=2)

ggplot(data2, aes(factor(c), d)) +  
  stat_boxplot(geom = "errorbar", width = 0.5) +
  scale_color_manual(values = c('slateblue', 'firebrick')) +
  geom_jitter(width = 0, size=5, aes (color=factor(c)), show.legend = FALSE, shape=20, alpha = 0.7) +
  geom_boxplot(alpha = 0.65, outlier.alpha = 0) + 
  ggtitle("", subtitle = paste("P-Valor = ", format(pvalue, digits=3, nsmall=2), "     Corr. Pt. Bisserial = ", format(biserialcor, digits=3, nsmall=2))) +
  theme(axis.title = element_blank(), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))


#---- gerar gráficos para quarteto de Ascombe

anscombe_m <- data.frame()
for(i in 1:4){
  anscombe_m <- rbind(anscombe_m, data.frame(set=i, x=anscombe[,i], y=anscombe[,i+4]))}
ggplot(anscombe_m, aes(x, y)) + geom_point(size=3, color="red", fill="orange", shape=21) + geom_smooth(method="lm", fill=NA, fullrange=TRUE) + facet_wrap(~set, ncol=2)


#---- risco por IDHM 

summary(Dados$Idhm2010)
glimpse(Dados$Idhm2010)

filter(Dados, Dados$Idhm2010 == 0.86)

biserial.cor(Dados$Idhm2010, Dados$Risco, use = c("complete.obs"), level=2)
cor.test(Dados$Risco, Dados$Idhm2010)

RiscoIDHM <- glm(Risco ~ Idhm2010, 
                 family = binomial(link = "logit"), data = Dados)

summary(RiscoIDHM)
PseudoR2(RiscoIDHM)
summary(RiscoIDHM)$coefficients[,4]

RiscoIDHM2 <- augment(RiscoIDHM)
ROC <- roc(Risco ~ .fitted, data = RiscoIDHM2)

ggplot(RiscoIDHM2, aes(m = .fitted, d = Risco)) + 
  geom_roc(n.cuts = 0, labels = FALSE, color = "blue") + 
  style_roc(guide = TRUE, ylab="Sensibilidade", xlab = "Especificidade", theme = theme_grey) +
  annotate("text", x=0.625, y=0.175, label = paste("AUC = ", round(auc(ROC),4)))

ggplot(Dados, aes(x=Idhm2010, y=Risco)) + 
  geom_point(size=1, color="red", fill="orange", shape=21, alpha = 0.2) + 
  geom_smooth(method = "glm",se = FALSE, method.args = structure(list(family = "binomial"), .Names = "family")) +
  ggtitle("Regressão logística de Risco por IDHM") +
  labs(y="Risco Identificado", x = "IDHM") + 
  annotate("text", x=0.8, y=0.3, label = "P~Valor == 0.0963", parse=T)

ggplot(Dados, aes(x = Idhm2010, y =  Risco)) + 
  geom_jitter(width = 0.03, height = 0.05, size=1, color="red", fill="orange", shape=21, alpha = 0.2) + 
  geom_smooth(method = "glm",se = FALSE, method.args = structure(list(family = "binomial"), .Names = "family"))+
  ggtitle("Regressão logística de Risco por IDHM") +
  labs(y="Risco Identificado", x = "IDHM")+ 
  annotate("text", x=0.8, y=0.3, label = "P~Valor == 0.0963", parse=T)


#---- Analise inicial da experiência do coordenador de PPG (boxplot)

Dados.ExpCoord <- Dados[complete.cases(Dados[,67]),] # cria subset somente com PPGs que responderam consulta do CGEE
names(Dados.ExpCoord)
# alteração necessária para permitir plotagem dos pontos no boxplot
for (i in 74:77) {
  Dados.ExpCoord[,i] <- ifelse(Dados.ExpCoord[,i] == 1, "Risco Observado", "Sem Risco") 
}

Dados.ExpCoord$Risco <- with(Dados.ExpCoord, relevel(factor(Dados.ExpCoord$Risco), "Sem Risco"))
Dados.ExpCoord$Estagnacao <- with(Dados.ExpCoord, relevel(factor(Dados.ExpCoord$Estagnacao), "Sem Risco"))
Dados.ExpCoord$Involucao <- with(Dados.ExpCoord, relevel(factor(Dados.ExpCoord$Involucao), "Sem Risco"))
Dados.ExpCoord$Descontinuidade <- with(Dados.ExpCoord, relevel(factor(Dados.ExpCoord$Descontinuidade), "Sem Risco"))


# calcula estatísticas
test <- wilcox.test(Dados.ExpCoord$ExperienciaCoordenador ~ Dados.ExpCoord$Risco)
pvalue <- test$p.value
biserialcor <- biserial.cor(Dados.ExpCoord$ExperienciaCoordenador, Dados.ExpCoord$Risco, use = c("complete.obs"), level=2)

list.ggplot.coord <- list()

# gera plots
list.ggplot.coord[[1]] <- ggplot(Dados.ExpCoord, aes(Risco, ExperienciaCoordenador), outline = FALSE) +  
    stat_boxplot(geom = "errorbar", width = 0.5) +
    scale_color_manual(values = c('slateblue', 'firebrick')) +
    geom_jitter(width = 0, size=2, aes (color=factor(Risco)), show.legend = FALSE, shape=20, alpha = 0.4) +
    geom_boxplot(alpha = 0.65, outlier.alpha = 0) + 
    ggtitle("Experiência do Coordenador de PPG", subtitle = paste("P-Valor = ", format(pvalue, digits=3, nsmall=2), "     Corr. Pt. Bisserial = ", format(biserialcor, digits=3, nsmall=2))) +
    theme_bw() +   
    theme(axis.title = element_blank(), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
    theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

list.ggplot.coord[[2]] <- ggplot(Dados.ExpCoord, aes(Risco, ExperienciaCoordenador), outline = FALSE) +  
    stat_boxplot(geom = "errorbar", width = 0.5) +
    scale_color_manual(values = c('slateblue', 'firebrick')) +
    geom_jitter(width = 0.35, size=2, aes (color=factor(Risco)), show.legend = FALSE, shape=20, alpha = 0.4) +
    geom_boxplot(alpha = 0.65, outlier.alpha = 0) + 
    ggtitle("Experiência do Coordenador de PPG", subtitle = paste("P-Valor = ", format(pvalue, digits=3, nsmall=2), "     Corr. Pt. Bisserial = ", format(biserialcor, digits=3, nsmall=2))) +
    theme_bw() + 
    theme(axis.title = element_blank(), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
    theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

list.ggplot.coord[[3]] <- ggplot(Dados.ExpCoord, aes(Risco, ExperienciaCoordenador), outline = FALSE) +  
    stat_boxplot(geom = "errorbar", width = 0.5) +
    scale_color_manual(values = c('slateblue', 'firebrick')) +
    geom_jitter(width = 0.35, size=2, aes (color=factor(Risco)), show.legend = FALSE, shape=20, alpha = 0.4) +
    geom_boxplot(alpha = 0.65, outlier.alpha = 0) + 
    coord_cartesian(ylim = c(0, quantile(Dados.ExpCoord$ExperienciaCoordenador)[4] + 1.5*IQR(Dados.ExpCoord$ExperienciaCoordenador)))+
    ggtitle("Experiência do Coordenador de PPG", subtitle = paste("P-Valor = ", format(pvalue, digits=3, nsmall=2), "     Corr. Pt. Bisserial = ", format(biserialcor, digits=3, nsmall=2))) +
    theme_bw() + 
    theme(axis.title = element_blank(), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
    theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

ggarrange(plotlist = list.ggplot.coord, labels = "auto", hjust = -4, vjust = -4, ncol = 3)

aggregate(ExperienciaCoordenador ~ Risco, data = Dados.ExpCoord, 
          FUN = function(x) c(Média = mean(x), Máximo = max(x), Mínimo = min(x), DesvioPadrão = sd(x)))

list.ggplot.coord <- list()

# calcula estatísticas segmentadas
test <- wilcox.test(Dados.ExpCoord$ExperienciaCoordenador ~ Dados.ExpCoord$Estagnacao)
pvalue <- test$p.value
biserialcor <- biserial.cor(Dados.ExpCoord$ExperienciaCoordenador, Dados.ExpCoord$Estagnacao, use = c("complete.obs"), level=2)

list.ggplot.coord[[1]] <- ggplot(Dados.ExpCoord, aes(factor(Estagnacao), ExperienciaCoordenador), outline = FALSE) +  
  stat_boxplot(geom = "errorbar", width = 0.5) +
  scale_color_manual(values = c('slateblue', 'firebrick')) +
  geom_jitter(width = 0.35, size=2, aes (color=factor(Estagnacao)), show.legend = FALSE, shape=20, alpha = 0.4) +
  geom_boxplot(alpha = 0.65, outlier.alpha = 0) + 
  coord_cartesian(ylim = c(0, quantile(Dados.ExpCoord$ExperienciaCoordenador)[4] + 1.5*IQR(Dados.ExpCoord$ExperienciaCoordenador)))+
  ggtitle("Experiência do Coordenador de PPG", subtitle = paste("P-Valor = ", format(pvalue, digits=3, nsmall=2), "     Corr. Pt. Bisserial = ", format(biserialcor, digits=3, nsmall=2))) +
  theme_bw() + 
  labs(y = "Risco de Estagnação") +
  theme(axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

test <- wilcox.test(Dados.ExpCoord$ExperienciaCoordenador ~ Dados.ExpCoord$Involucao)
pvalue <- test$p.value
biserialcor <- biserial.cor(Dados.ExpCoord$ExperienciaCoordenador, Dados.ExpCoord$Involucao, use = c("complete.obs"), level=2)

list.ggplot.coord[[2]] <- ggplot(Dados.ExpCoord, aes(factor(Involucao), ExperienciaCoordenador), outline = FALSE) +  
  stat_boxplot(geom = "errorbar", width = 0.5) +
  scale_color_manual(values = c('slateblue', 'firebrick')) +
  geom_jitter(width = 0.35, size=2, aes (color=factor(Involucao)), show.legend = FALSE, shape=20, alpha = 0.4) +
  geom_boxplot(alpha = 0.65, outlier.alpha = 0) + 
  coord_cartesian(ylim = c(0, quantile(Dados.ExpCoord$ExperienciaCoordenador)[4] + 1.5*IQR(Dados.ExpCoord$ExperienciaCoordenador)))+
  ggtitle("Experiência do Coordenador de PPG", subtitle = paste("P-Valor = ", format(pvalue, digits=3, nsmall=2), "     Corr. Pt. Bisserial = ", format(biserialcor, digits=3, nsmall=2))) +
  theme_bw() + 
  labs(y = "Risco de Involução") +
  theme(axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

test <- wilcox.test(Dados.ExpCoord$ExperienciaCoordenador ~ Dados.ExpCoord$Descontinuidade)
pvalue <- test$p.value
biserialcor <- biserial.cor(Dados.ExpCoord$ExperienciaCoordenador, Dados.ExpCoord$Descontinuidade, use = c("complete.obs"), level=2)

list.ggplot.coord[[3]] <- ggplot(Dados.ExpCoord, aes(factor(Descontinuidade), ExperienciaCoordenador), outline = FALSE) +  
  stat_boxplot(geom = "errorbar", width = 0.5) +
  scale_color_manual(values = c('slateblue', 'firebrick')) +
  geom_jitter(width = 0.35, size=2, aes (color=factor(Descontinuidade)), show.legend = FALSE, shape=20, alpha = 0.4) +
  geom_boxplot(alpha = 0.65, outlier.alpha = 0) + 
  coord_cartesian(ylim = c(0, quantile(Dados.ExpCoord$ExperienciaCoordenador)[4] + 1.5*IQR(Dados.ExpCoord$ExperienciaCoordenador)))+
  ggtitle("Experiência do Coordenador de PPG", subtitle = paste("P-Valor = ", format(pvalue, digits=3, nsmall=2), "     Corr. Pt. Bisserial = ", format(biserialcor, digits=3, nsmall=2))) +
  theme_bw() + 
  labs(y = "Risco de Descontinuidade") +
  theme(axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

ggarrange(plotlist = list.ggplot.coord, labels = c("(a)","(b)","(c)"), hjust = -0.5, vjust = 1, ncol = 3)

#---- Boxplot de múltiplas variáveis
Dados.BoxPlot <- as.data.frame(subset(Dados, select=c(Risco,
                                                      Estagnacao,
                                                      Involucao,
                                                      Descontinuidade,
                                                      ProporcaoNotas567,
                                                      MediaPonderadaNotas,
                                                      PPGsPorMunicipio,
                                                      IndiceDeGini2010,
                                                      PercDosOcupadosComSuperiorCompleto18AnosOuMais2010,
                                                      PopulacaoTotal2010,
                                                      Idhm2010,
                                                      IdhmRenda2010,
                                                      IdhmEducacao2010,
                                                      SubindiceDeEscolaridadeIdhmEducacao2010,
                                                      PercDe25AnosOuMaisComSuperiorCompleto2010,
                                                      ExpectativaDeAnosDeEstudo2010,
                                                      TaxaDeFrequenciaBrutaAoSuperior2010,
                                                      RendaPerCapita2010,
                                                      PropPosGraduadosMun,
                                                      PropPosGraduadosRGI,
                                                      PropPosGraduadosRGInt,
                                                      DensidadePPGMun,
                                                      DensidadePPGRGI,
                                                      DensidadePPGRGInt,
                                                      PropSenior,
                                                      PropJunior,
                                                      PropSeniorJunior,
                                                      PermanentesPorMinimo,
                                                      MediaIDEB,
                                                      ExperienciaCoordenador,
                                                      IdadePPG) ) )

# alteração necessária para permitir plotagem dos pontos no boxplot
for (i in 1:4) {
  Dados.BoxPlot[,i] <- ifelse(Dados.BoxPlot[,i] == 1, "Risco Observado", "Sem Risco") 
}

Dados.BoxPlot$Risco <- with(Dados.BoxPlot, relevel(factor(Dados.BoxPlot$Risco), "Sem Risco"))
Dados.BoxPlot$Estagnacao <- with(Dados.BoxPlot, relevel(factor(Dados.BoxPlot$Estagnacao), "Sem Risco"))
Dados.BoxPlot$Involucao <- with(Dados.BoxPlot, relevel(factor(Dados.BoxPlot$Involucao), "Sem Risco"))
Dados.BoxPlot$Descontinuidade <- with(Dados.BoxPlot, relevel(factor(Dados.BoxPlot$Descontinuidade), "Sem Risco"))
Dados.BoxPlot$PopulacaoTotal2010 <- Dados.BoxPlot$PopulacaoTotal2010/1000

# observando correlação entre variáveis
colnames(Dados.BoxPlot) <- c("Risco", "REst", "RInv", "RDes", "PNota", "MPond", "PPGsMun", "IGini",
                             "OcSup18", "PopMun", "IDHM", "IDHM_R", "IDHM_E", "SubEsc", "PopSup25",
                             "ExpEst", "TxFreqSup", "Renda", "PropPosM", "PropPosRGI", "PropPosRGInt",
                             "DensPPGM", "DensPPGRGI", "DensPPGRGInt", "PropSen", "PropJr", "PropSenJr",
                             "PerMin", "IDEB", "ExpCoord","IdadePPG")
M <- cor(Dados.BoxPlot[5:31])
plot.new(); dev.off()
corrplot.mixed(M, upper = "ellipse", lower.col = "black", tl.col = "black")

summary(lm(Renda ~ PopSup25, data = Dados.BoxPlot))

# muda título das variáveis para plotagem
colnames(Dados.BoxPlot) <- c("Risco",
                             "Risco de Estagnação",
                             "Risco de Involução",
                             "Risco de Descontinuidade",
                             "Proporção de Notas 5,6 e 7 (IES)",
                             "Média Ponderada das Notas (IES)",
                             "PPGs por Municipio",
                             "Índice de Gini",
                             "Ocupados com Sup. Completo (18+ anos)",
                             "População do Município (milhares) ",
                             "IDHM",
                             "IDHM Renda",
                             "IDHM Educação",
                             "Subíndice de Escolaridade (IDHM)",
                             "Pop. com Superior Completo (25+ anos)",
                             "Expectativa de Anos de Estudo",
                             "Taxa de Frequência Bruta ao Superior",
                             "Renda per Capita",
                             "Prop. de Pós-Graduados por Município",
                             "Prop. de Pós-Graduados por RGI",
                             "Prop. de Pós-Graduados por RGInt",
                             "Densidade de PPGs por Município",
                             "Densidade de PPGs por RGI",
                             "Densidade de PPGs por RGInt",
                             "Proporção de Docentes Sênior",
                             "Proporção de Docentes Júnior",
                             "Proporção de Docentes Sênior e Júnior",
                             "Razão de Perm. pelo Mínimo da Área",
                             "IDEB Médio do Município",
                             "Experiência do Coordenador de PPG",
                             "Idade do PPG")

names(Dados.BoxPlot)

#---- gera boxplots relacionando risco e todas as demais variáveis no dataframe

CriaBoxPlots <- function(risco, variavel, min, max){
  n <- risco
  i <- variavel + 4
  y <- Dados.BoxPlot[, i]
  test <- wilcox.test(y ~ Dados.BoxPlot[, n])
  biserialcor <- biserial.cor(y, Dados.BoxPlot[, n], use = c("complete.obs"), level=2)
  ggplot(Dados.BoxPlot, aes(x = Dados.BoxPlot[, n], y = y)) +  
    stat_boxplot(geom = "errorbar", width = 0.5) +
    scale_color_manual(values = c('slateblue', 'firebrick')) +
    geom_jitter(width = 0.35, size=1.5, aes(color=Dados.BoxPlot[, n]), show.legend = FALSE, shape=20, alpha = 0.5) +
    geom_boxplot(alpha = 0.65, outlier.alpha = 0) + 
    coord_cartesian(ylim = c(min, max))+
    ggtitle(colnames(Dados.BoxPlot)[i], subtitle = paste("P-Valor = ", format(test$p.value, digits=3, nsmall=2), "     Corr. Pt. Bisserial = ", format(biserialcor, digits=3, nsmall=2))) +
    theme_bw() + 
    labs(y = colnames(Dados.BoxPlot)[n]) +
    theme(axis.title = element_blank(), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
    theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
}

ggarrange(
CriaBoxPlots(1,1,0,1),
CriaBoxPlots(1,2,2,5.7),
CriaBoxPlots(1,3,0,140),
NULL, NULL, NULL,
CriaBoxPlots(1,4,0.4,0.7),
CriaBoxPlots(1,5,0,0.35),
CriaBoxPlots(1,6,0,2700),
NULL, NULL, NULL,
CriaBoxPlots(1,7,0.66,0.86),
CriaBoxPlots(1,8,0.62,0.89),
CriaBoxPlots(1,9,0.62,0.81),
ncol = 3, nrow = 5, hjust = -1, vjust = 1,
heights = c(1, 0.05, 1, 0.05, 1),
labels = c("(a)", "(b)", "(c)", "", "", "", 
           "(d)", "(e)", "(f)", "", "", "", 
           "(g)", "(h)", "(i)"))

ggarrange(
  CriaBoxPlots(1,10,0.51,0.8),
  CriaBoxPlots(1,11,0.03,0.32),
  CriaBoxPlots(1,12,8.5,11.4),
  NULL, NULL, NULL,
  CriaBoxPlots(1,13,0.2,0.745),
  CriaBoxPlots(1,14,250,2050),
  CriaBoxPlots(1,15,0,0.055),
  NULL, NULL, NULL,
  CriaBoxPlots(1,16,0.002,0.033),
  CriaBoxPlots(1,17,0.002,0.033),
  CriaBoxPlots(1,18,0,110),
  ncol = 3, nrow = 5, hjust = -1, vjust = 1,
  heights = c(1, 0.05, 1, 0.05, 1),
  labels = c("(j)", "(k)", "(l)", "", "", "", 
             "(m)", "(n)", "(o)", "", "", "", 
             "(p)", "(q)", "(r)"))

ggarrange(
CriaBoxPlots(1,19,2,67),
CriaBoxPlots(1,20,0,55),
CriaBoxPlots(1,21,0,0.43),
NULL, NULL, NULL,
CriaBoxPlots(1,22,0,0.58),
CriaBoxPlots(1,23,0,0.65),
CriaBoxPlots(1,24,0,3.2),
NULL, NULL, NULL,
CriaBoxPlots(1,25,3.3,6.8),
CriaBoxPlots(1,26,0,95),
CriaBoxPlots(1,27,0,60),
ncol = 3, nrow = 5, hjust = -1, vjust = 1,
heights = c(1, 0.05, 1, 0.05, 1),
labels = c("(s)", "(t)", "(u)", "", "", "",
           "(v)", "(w)", "(x)", "", "", "",
           "(y)", "(z)", "(å)"))

ggarrange(CriaBoxPlots(1,25,3.3,6.8),
          CriaBoxPlots(2,25,3.3,6.8),
          NULL, NULL,
          CriaBoxPlots(3,25,3.3,6.8),
          CriaBoxPlots(4,25,3.3,6.8), 
          hjust = -1, vjust = 2, ncol = 2, nrow = 3,
          heights = c(1, 0.05, 1),
          labels = c("(a)", "(b)", "", "", "(c)", "(d)"))


#---- efetuando análise para os diferentes tipos de risco e todas as demais variáveis no dataframe

# cria data.frame vazio
AnaliseQuantitativa <- data.frame(Indicador = "",
                                  PValor_RiscoGeral = as.numeric(""), 
                                  CorrBtBisserial_RiscoGeral = as.numeric(""),
                                  PValor_Estagnacao = as.numeric(""), 
                                  CorrBtBisserial_Estagnacao = as.numeric(""),
                                  PValor_Involucao = as.numeric(""), 
                                  CorrBtBisserial_Involucao = as.numeric(""),
                                  PValor_Descontinuidade = as.numeric(""), 
                                  CorrBtBisserial_Descontinuidade = as.numeric(""))
AnaliseQuantitativa$Indicador <- as.character(AnaliseQuantitativa$Indicador)
AnaliseQuantitativa <- AnaliseQuantitativa[-c(1),]

for (i in 5:ncol(Dados.BoxPlot)) {
  y <- Dados.BoxPlot[, i]
  test <- wilcox.test(y ~ Dados.BoxPlot$Risco)
  test2 <- wilcox.test(y ~ Dados.BoxPlot$`Risco de Estagnação`)
  test3 <- wilcox.test(y ~ Dados.BoxPlot$`Risco de Involução`)
  test4 <- wilcox.test(y ~ Dados.BoxPlot$`Risco de Descontinuidade`)
  AnaliseQuantitativa[i-4,1] <- colnames(Dados.BoxPlot)[i]
  AnaliseQuantitativa[i-4,2] <- format(test$p.value, digits=3, nsmall=2)
  AnaliseQuantitativa[i-4,3] <- format(biserial.cor(y, Dados.BoxPlot$Risco, use = c("complete.obs"), level=2), digits=3, nsmall=2)
  AnaliseQuantitativa[i-4,4] <- format(test2$p.value, digits=3, nsmall=2)
  AnaliseQuantitativa[i-4,5] <- format(biserial.cor(y, Dados.BoxPlot$`Risco de Estagnação`, use = c("complete.obs"), level=2), digits=3, nsmall=2)
  AnaliseQuantitativa[i-4,6] <- format(test3$p.value, digits=3, nsmall=2)
  AnaliseQuantitativa[i-4,7] <- format(biserial.cor(y, Dados.BoxPlot$`Risco de Involução`, use = c("complete.obs"), level=2), digits=3, nsmall=2)
  AnaliseQuantitativa[i-4,8] <- format(test4$p.value, digits=3, nsmall=2)
  AnaliseQuantitativa[i-4,9] <- format(biserial.cor(y, Dados.BoxPlot$`Risco de Descontinuidade`, use = c("complete.obs"), level=2), digits=3, nsmall=2)
}

# exportando os resultados para o Excel

write_excel_csv(AnaliseQuantitativa, path = "Dados/PValorCorr.csv", na = "")

##################
# Naive Bayes
##################

#---- Recorte de dados
Dados.NaiveBayes <- as.data.frame(subset(Dados, select=c(Risco,
                                                      Estagnacao,
                                                      Involucao,
                                                      Descontinuidade,
                                                      ProporcaoNotas567,
                                                      MediaPonderadaNotas,
                                                      PPGsPorMunicipio,
                                                      IndiceDeGini2010,
                                                      PercDosOcupadosComSuperiorCompleto18AnosOuMais2010,
                                                      PopulacaoTotal2010,
                                                      Idhm2010,
                                                      IdhmRenda2010,
                                                      IdhmEducacao2010,
                                                      SubindiceDeEscolaridadeIdhmEducacao2010,
                                                      PercDe25AnosOuMaisComSuperiorCompleto2010,
                                                      ExpectativaDeAnosDeEstudo2010,
                                                      TaxaDeFrequenciaBrutaAoSuperior2010,
                                                      RendaPerCapita2010,
                                                      PropPosGraduadosMun,
                                                      PropPosGraduadosRGI,
                                                      PropPosGraduadosRGInt,
                                                      DensidadePPGMun,
                                                      DensidadePPGRGI,
                                                      DensidadePPGRGInt,
                                                      PropSenior,
                                                      PropJunior,
                                                      PermanentesPorMinimo,
                                                      MediaIDEB,
                                                      ExperienciaCoordenador,
                                                      Modalidade,
                                                      Nivel,
                                                      NaturezaJuridica,
                                                      NaturezaJuridicaDetalhada,
                                                      Regiao,
                                                      GrandeArea,
                                                      Colegio,
                                                      AreaDeAvaliacao) ) )

names(Dados.NaiveBayes)

# Cria categorias para variáveis numéricas e converte categorias em fatores
for (i in 5:29) {
  Dados.NaiveBayes[,i] <- cut2(Dados.NaiveBayes[,i], g = 4)  
}

for (i in 30:37) {
  Dados.NaiveBayes[,i] <- factor(Dados.NaiveBayes[,i])  
}

for (i in 1:4) {
Dados.NaiveBayes[,i] <- ifelse(Dados.NaiveBayes[,i] == 1, "Risco Observado", "Sem Risco")
Dados.NaiveBayes[,i] <- factor(Dados.NaiveBayes[,i])
}

str(Dados.NaiveBayes)

ggplot(Dados.NaiveBayes, aes(ProporcaoNotas567)) + geom_bar(fill = "slateblue", alpha = 0.6) +
  labs(y="Número de Programas", x = "Categorias") 

Dados.NaiveBayes$GrandeArea <- revalue(Dados.NaiveBayes$GrandeArea, 
                                       c("Ciências Agrárias"="Agrárias", 
                                       "Ciências Biológicas"="Biológicas",
                                       "Ciências da Saúde"="C.Saúde",
                                       "Ciências Exatas e da Terra"="Exat/Terra",
                                       "Ciências Humanas"="Humanas",
                                       "Ciências Sociais Aplicadas"="Soc.Aplicadas",
                                       "Engenharias"="Eng.",
                                       "Linguística, Letras e Artes"="Let. Artes",
                                       "Multidisciplinar"="Mult."))
summary(Dados.NaiveBayes$GrandeArea)

Dados.NaiveBayes$Colegio <- revalue(Dados.NaiveBayes$Colegio, 
                                                      c("Ciências Exatas, Tecnológicas e Multidisciplinar"="C. Exatas, Tec. e Mult."))

summary(Dados.NaiveBayes$Colegio)

Dados.NaiveBayes$NaturezaJuridicaDetalhada <- revalue(Dados.NaiveBayes$NaturezaJuridicaDetalhada, 
                                       c("Filantrópica ou Comunitária" = "F/Com.", 
                                         "Confessional" = "Conf.",
                                         "Privada com fins lucrativos"="PFL",
                                         "Privada sem fins lucrativos"="PSL",
                                         "Pública Estadual"="Estadual",
                                         "Pública Federal" = "Federal",
                                         "Pública Municipal" = "Municipal"))

summary(Dados.NaiveBayes$NaturezaJuridicaDetalhada)

#---- criando modelos com base nas variáveis relevantes para cada tipo de risco

Naive.Risco <- naive_bayes(Risco ~ 
                           PPGsPorMunicipio +
                           IndiceDeGini2010 +
                           PercDosOcupadosComSuperiorCompleto18AnosOuMais2010 +
                           PopulacaoTotal2010 +
                           ExpectativaDeAnosDeEstudo2010 +
                           PropSenior +
                           PropJunior +
                           Modalidade +
                           Nivel +
                           NaturezaJuridica +
                           NaturezaJuridicaDetalhada +
                           Regiao +
                           GrandeArea +
                           Colegio, data = Dados.NaiveBayes)

Naive.Estagnacao <- naive_bayes(Estagnacao ~
                           ProporcaoNotas567 +
                           MediaPonderadaNotas +
                           IndiceDeGini2010 +
                           Idhm2010 +
                           PercDe25AnosOuMaisComSuperiorCompleto2010 +
                           ExpectativaDeAnosDeEstudo2010 +
                           PropPosGraduadosRGInt +
                           PropSenior +
                           PropJunior +
                           MediaIDEB +
                           ExperienciaCoordenador +   
                           Modalidade +
                           Nivel +
                           NaturezaJuridicaDetalhada +
                           Regiao +
                           GrandeArea, data = Dados.NaiveBayes)

Naive.Involucao <- naive_bayes(Involucao ~
                                ProporcaoNotas567 +
                                MediaPonderadaNotas +
                                PPGsPorMunicipio +
                                PercDosOcupadosComSuperiorCompleto18AnosOuMais2010 +
                                PopulacaoTotal2010 +
                                Idhm2010 +
                                IdhmRenda2010 +
                                IdhmEducacao2010 +
                                SubindiceDeEscolaridadeIdhmEducacao2010 +
                                TaxaDeFrequenciaBrutaAoSuperior2010 +
                                PropPosGraduadosMun +
                                PropSenior +
                                PropJunior +
                                MediaIDEB +
                                PermanentesPorMinimo +
                                Modalidade +
                                Nivel +
                                NaturezaJuridicaDetalhada +
                                Regiao +
                                GrandeArea, data = Dados.NaiveBayes)

Naive.Descontinuidade <- naive_bayes(Descontinuidade ~
                                ProporcaoNotas567 +
                                MediaPonderadaNotas +
                                IndiceDeGini2010 +
                                PopulacaoTotal2010 +
                                ExpectativaDeAnosDeEstudo2010 +
                                DensidadePPGRGI +
                                PropSenior +
                                MediaIDEB +
                                Modalidade +
                                Nivel +
                                NaturezaJuridicaDetalhada +
                                Regiao +
                                GrandeArea, data = Dados.NaiveBayes)

#---- prepara prancheta para os boxplots
par(mfrow=c(5,3), mar=c(2, 2.5, 2.5, 0.5), mgp = c(2, 0.2, 0), tck = -0.01, oma=c(0, 0, 1, 0), cex.lab = 2)
plot(Naive.Risco, arg.cat = list(cex.axis = 1.2))

par(mfrow=c(4,4), mar=c(2, 2.5, 2.5, 0.5), mgp = c(2, 0.2, 0), tck = -0.01, oma=c(0, 0, 1, 0), cex.lab = 2.4)
plot(Naive.Estagnacao, arg.cat = list(cex.axis = 1.2))

par(mfrow=c(5,4), mar=c(2, 2.5, 2.5, 0.5), mgp = c(2, 0.2, 0), tck = -0.01, oma=c(0, 0, 1, 0), cex.lab = 2)
plot(Naive.Involucao, arg.cat = list(cex.axis = 1.2))

par(mfrow=c(4,4), mar=c(2, 2.5, 2.5, 0.5), mgp = c(2, 0.2, 0), tck = -0.01, oma=c(0, 0, 1, 0), cex.lab = 2.7)
plot(Naive.Descontinuidade, arg.cat = list(cex.axis = 1.2))

# criar conjuntos aleatórios para elaboração do modelo e para teste
AmostraAleatoria <- sample(nrow(Dados.NaiveBayes), nrow(Dados.NaiveBayes) * 0.75)
Dados.NaiveBayes_modelo <- Dados.NaiveBayes[AmostraAleatoria,]
Dados.NaiveBayes_teste <- Dados.NaiveBayes[-AmostraAleatoria,]

# refazendo os modelos

Naive.Risco <- naive_bayes(Risco ~  
                             PPGsPorMunicipio +
                             IndiceDeGini2010 +
                             PercDosOcupadosComSuperiorCompleto18AnosOuMais2010 +
                             PopulacaoTotal2010 +
                             ExpectativaDeAnosDeEstudo2010 +
                             PropSenior +
                             PropJunior +
                             Modalidade +
                             Nivel +
                             NaturezaJuridicaDetalhada +
                             Regiao +
                             GrandeArea, data = Dados.NaiveBayes_modelo)

Naive.Estagnacao <- naive_bayes(Estagnacao ~
                                  ProporcaoNotas567 +
                                  MediaPonderadaNotas +
                                  IndiceDeGini2010 +
                                  Idhm2010 +
                                  PercDe25AnosOuMaisComSuperiorCompleto2010 +
                                  ExpectativaDeAnosDeEstudo2010 +
                                  PropPosGraduadosRGInt +
                                  PropSenior +
                                  PropJunior +
                                  MediaIDEB +
                                  ExperienciaCoordenador +   
                                  Modalidade +
                                  Nivel +
                                  NaturezaJuridicaDetalhada +
                                  Regiao +
                                  GrandeArea, data = Dados.NaiveBayes_modelo)

Naive.Involucao <- naive_bayes(Involucao ~
                                 ProporcaoNotas567 +
                                 MediaPonderadaNotas +
                                 PPGsPorMunicipio +
                                 PercDosOcupadosComSuperiorCompleto18AnosOuMais2010 +
                                 PopulacaoTotal2010 +
                                 Idhm2010 +
                                 IdhmRenda2010 +
                                 IdhmEducacao2010 +
                                 SubindiceDeEscolaridadeIdhmEducacao2010 +
                                 TaxaDeFrequenciaBrutaAoSuperior2010 +
                                 PropPosGraduadosMun +
                                 PropSenior +
                                 PropJunior +
                                 MediaIDEB +
                                 PermanentesPorMinimo +
                                 Modalidade +
                                 Nivel +
                                 NaturezaJuridicaDetalhada +
                                 Regiao +
                                 GrandeArea, data = Dados.NaiveBayes_modelo)

Naive.Descontinuidade <- naive_bayes(Descontinuidade ~
                                       ProporcaoNotas567 +
                                       MediaPonderadaNotas +
                                       IndiceDeGini2010 +
                                       PopulacaoTotal2010 +
                                       ExpectativaDeAnosDeEstudo2010 +
                                       DensidadePPGRGI +
                                       PropSenior +
                                       MediaIDEB +
                                       Modalidade +
                                       Nivel +
                                       NaturezaJuridicaDetalhada +
                                       Regiao +
                                       GrandeArea, data = Dados.NaiveBayes_modelo)

Dados.NaiveBayes_teste$previsao.risco <- predict(Naive.Risco, Dados.NaiveBayes_teste, type = "class")
Dados.NaiveBayes_teste$previsao.estagnacao <- predict(Naive.Estagnacao, Dados.NaiveBayes_teste, type = "class")
Dados.NaiveBayes_teste$previsao.involucao <- predict(Naive.Involucao, Dados.NaiveBayes_teste, type = "class")
Dados.NaiveBayes_teste$previsao.descontinuidade <- predict(Naive.Descontinuidade, Dados.NaiveBayes_teste, type = "class")

#---- Calculando a acurácia do modelo
mean(Dados.NaiveBayes_teste$previsao.risco == Dados.NaiveBayes_teste$Risco)
mean(Dados.NaiveBayes_teste$previsao.estagnacao == Dados.NaiveBayes_teste$Estagnacao)
mean(Dados.NaiveBayes_teste$previsao.involucao == Dados.NaiveBayes_teste$Involucao)
mean(Dados.NaiveBayes_teste$previsao.descontinuidade == Dados.NaiveBayes_teste$Descontinuidade)

nrow(filter(Dados.NaiveBayes_teste, Descontinuidade == "Risco Observado"))

table(Dados.NaiveBayes_teste$Risco, Dados.NaiveBayes_teste$previsao.risco)
table(Dados.NaiveBayes_teste$Estagnacao, Dados.NaiveBayes_teste$previsao.estagnacao)
table(Dados.NaiveBayes_teste$Involucao, Dados.NaiveBayes_teste$previsao.involucao)
table(Dados.NaiveBayes_teste$Descontinuidade, Dados.NaiveBayes_teste$previsao.descontinuidade)

#---- Verificando a área de avaliação no lugar de Grande Área

Naive.Involucao.Area <- naive_bayes(Involucao ~
                                      ProporcaoNotas567 +
                                      MediaPonderadaNotas +
                                      PPGsPorMunicipio +
                                      PercDosOcupadosComSuperiorCompleto18AnosOuMais2010 +
                                      PopulacaoTotal2010 +
                                      Idhm2010 +
                                      IdhmRenda2010 +
                                      IdhmEducacao2010 +
                                      SubindiceDeEscolaridadeIdhmEducacao2010 +
                                      TaxaDeFrequenciaBrutaAoSuperior2010 +
                                      PropPosGraduadosMun +
                                      PropSenior +
                                      PropJunior +
                                      MediaIDEB +
                                      PermanentesPorMinimo +
                                      Modalidade +
                                      Nivel +
                                      NaturezaJuridicaDetalhada +
                                      Regiao +
                                      AreaDeAvaliacao, data = Dados.NaiveBayes_modelo)

Dados.NaiveBayes_teste$previsao.involucao.area <- predict(Naive.Involucao.Area, Dados.NaiveBayes_teste, type = "class")
mean(Dados.NaiveBayes_teste$previsao.involucao.area == Dados.NaiveBayes_teste$Involucao)
nrow(filter(Dados.NaiveBayes_teste, Involucao == "Risco Observado"))
table(Dados.NaiveBayes_teste$Involucao, Dados.NaiveBayes_teste$previsao.involucao.area)

#---- criando ROC para os modelos principais e também para o teste com a área de avaliação

for (i in c(1:4,38:42)) {
  Dados.NaiveBayes_teste[,i] <- ifelse(Dados.NaiveBayes_teste[,i] == "Risco Observado", 1, 0)
  Dados.NaiveBayes_teste[,i] <- as.numeric(Dados.NaiveBayes_teste[,i])
}


ROC <- roc(Risco ~ previsao.risco, data = Dados.NaiveBayes_teste)
Roc1 <- ggplot(Dados.NaiveBayes_teste, aes(m = previsao.risco, d = Risco)) + 
  geom_roc(n.cuts = 0, labels = FALSE, color = "blue") + 
  ggtitle("Risco Geral") +
  style_roc(guide = TRUE, ylab="Sensibilidade", xlab = "Especificidade", theme = theme_grey) +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x=0.625, y=0.175, label = paste("AUC = ", round(auc(ROC),4)))

ROC <- roc(Estagnacao ~ previsao.estagnacao, data = Dados.NaiveBayes_teste)
Roc2 <- ggplot(Dados.NaiveBayes_teste, aes(m = previsao.estagnacao, d = Estagnacao)) + 
  geom_roc(n.cuts = 0, labels = FALSE, color = "blue") + 
  ggtitle("Risco de Estagnação") +
  style_roc(guide = TRUE, ylab="Sensibilidade", xlab = "Especificidade", theme = theme_grey) +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x=0.625, y=0.175, label = paste("AUC = ", round(auc(ROC),4)))

ROC <- roc(Involucao ~ previsao.involucao, data = Dados.NaiveBayes_teste)
Roc3 <- ggplot(Dados.NaiveBayes_teste, aes(m = previsao.involucao, d = Involucao)) + 
  geom_roc(n.cuts = 0, labels = FALSE, color = "blue") + 
  ggtitle("Risco de Involução") +
  style_roc(guide = TRUE, ylab="Sensibilidade", xlab = "Especificidade", theme = theme_grey) +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x=0.625, y=0.175, label = paste("AUC = ", round(auc(ROC),4)))

ROC <- roc(Descontinuidade ~ previsao.descontinuidade, data = Dados.NaiveBayes_teste)
Roc4 <- ggplot(Dados.NaiveBayes_teste, aes(m = previsao.descontinuidade, d = Descontinuidade)) + 
  geom_roc(n.cuts = 0, labels = FALSE, color = "blue", linejoin = "round") + 
  ggtitle("Risco de Descontinuidade") +
  style_roc(guide = TRUE, ylab="Sensibilidade", xlab = "Especificidade", theme = theme_grey) +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x=0.625, y=0.175, label = paste("AUC = ", round(auc(ROC),4)))

ggarrange(Roc1, Roc2, Roc3, Roc4,
  ncol = 2, nrow = 2, hjust = -0.5, vjust = 1,
  labels = c("(a)", "(b)", "(c)", "(d)"))


ROC <- roc(Risco ~ previsao.involucao.area, data = Dados.NaiveBayes_teste)
ggplot(Dados.NaiveBayes_teste, aes(m = previsao.involucao.area, d = Risco)) + 
  geom_roc(n.cuts = 0, labels = FALSE, color = "blue") + 
  ggtitle("Involução com Área de Avaliação entre indicadores") +
  style_roc(guide = TRUE, ylab="Sensibilidade", xlab = "Especificidade", theme = theme_grey) +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x=0.625, y=0.175, label = paste("AUC = ", round(auc(ROC),4)))


#######################
# Regressão Logística
#######################

## Para construir um modelo de previsão de risco, será necessário adotar procedimentos para 
## encontrar o conjunto de indicadores com melhor R2, R2 ajustado, CP de Mallow ou Critério BIC.
## Para isso, vamos utilizar os metodos de seleção backwards, forward and stepwise, seguido
## de varredura de todas as combinações possíveis de variáveis preditoras, método bestsubset.

## Para começar, geramos um novo subset de dados com as variáveis identificadas 
## como relevantes pelo método Naive Bayes

Dados.Logit <- as.data.frame(subset(Dados, select=c(Risco,
                                                         Estagnacao,
                                                         Involucao,
                                                         Descontinuidade,
                                                         ProporcaoNotas567,
                                                         MediaPonderadaNotas,
                                                         PPGsPorMunicipio,
                                                         IndiceDeGini2010,
                                                         PercDosOcupadosComSuperiorCompleto18AnosOuMais2010,
                                                         PopulacaoTotal2010,
                                                         Idhm2010,
                                                         IdhmRenda2010,
                                                         IdhmEducacao2010,
                                                         SubindiceDeEscolaridadeIdhmEducacao2010,
                                                         PercDe25AnosOuMaisComSuperiorCompleto2010,
                                                         ExpectativaDeAnosDeEstudo2010,
                                                         TaxaDeFrequenciaBrutaAoSuperior2010,
                                                         RendaPerCapita2010,
                                                         PropPosGraduadosMun,
                                                         PropPosGraduadosRGI,
                                                         PropPosGraduadosRGInt,
                                                         DensidadePPGMun,
                                                         DensidadePPGRGI,
                                                         DensidadePPGRGInt,
                                                         PropSenior,
                                                         PropJunior,
                                                         PermanentesPorMinimo,
                                                         MediaIDEB,
                                                         ExperienciaCoordenador,
                                                         IdadePPG,
                                                         Modalidade,
                                                         Nivel,
                                                         NaturezaJuridica,
                                                         NaturezaJuridicaDetalhada,
                                                         Regiao,
                                                         GrandeArea,
                                                         Colegio,
                                                         AreaDeAvaliacao) ) )

names(Dados.Logit)
summary(Dados.Logit)
Dados.Logit$PropSenior[is.na(Dados.Logit$PropSenior)] <- 0 # exclui NAs identificados em análise anterior
Dados.Logit$PropJunior[is.na(Dados.Logit$PropJunior)] <- 0

EscolhaModeloGLM <- data.frame()
EscolhaModeloGLM[1,1] <- "Backward"
EscolhaModeloGLM[2,1] <- "Forward"
EscolhaModeloGLM[3,1] <- "Stepwise"
EscolhaModeloGLM[4,1] <- "Prioridade BIC"
EscolhaModeloGLM[5,1] <- "Prioridade CP de Mallows"
EscolhaModeloGLM[6,1] <- "Prioridade R2"

# criar conjuntos aleatórios para elaboração do modelo e para teste
AmostraAleatoriaLogit <- sample(nrow(Dados.Logit), nrow(Dados.Logit) * 0.75)
Dados.Logit_modelo <- Dados.Logit[AmostraAleatoriaLogit,]
Dados.Logit_teste <- Dados.Logit[-AmostraAleatoriaLogit,]

summary(Dados.Logit_modelo)
summary(Dados.Logit_teste)

# A partir disso, criamos modelos logísticos com os mesmos conjuntos antes utilizados 

##### Modelo geral de risco
## metodologia stepwise para encontrar a combinação de indicadores que compõem o melhor modelo.

Logit.Risco <- glm(Risco ~ 
                     PPGsPorMunicipio +
                     IndiceDeGini2010 +
                     PercDosOcupadosComSuperiorCompleto18AnosOuMais2010 +
                     PopulacaoTotal2010 +
                     ExpectativaDeAnosDeEstudo2010 +
                     PropSenior +
                     PropJunior +  
                     IdadePPG +
                     Modalidade +
                     Nivel +
                     NaturezaJuridicaDetalhada +
                     Regiao +
                     GrandeArea, family = binomial(link = "logit"), data = Dados.Logit)

summary(Logit.Risco)
PseudoR2(Logit.Risco, which = "McFadden")
AIC(Logit.Risco)
BIC(Logit.Risco)

## Considerando Logit.Risco, já gerado, temos:

risco.backward <- step(Logit.Risco, direction = "backward")
summary(risco.backward)

risco.forward <- step(Logit.Risco, direction = "forward")
summary(risco.forward)

risco.both <- step(Logit.Risco, direction = "both")
summary(risco.both)

## O melhor modelo, encontrado pela seleção stepwise, pode ser estimado utilizando o comando "formula":

formula(risco.backward)
formula(risco.forward)
formula(risco.both)

## O segundo método que utilizaremos será pela seleção do best subset, 
## varrendo todas as combinações possíveis de variáveis preditoras para encontrar
## o conjunto com melhor R2, R2 ajustado, CP de Mallow ou Critério BIC. 

## Para começar, geramos o bestsub a partir do comando regsubsets:

bestsub.risco <- regsubsets(formula(Logit.Risco), data = Dados.Logit, nvmax = 10000)
summary.bestsub.risco <- summary(bestsub.risco)

## Analisando o summary.bestsub, vemos a significância de cada variável dentro de diferentes 
## modelos possíveis. Vemos então o melhor número de variáveis de acordo com cada critério.

summary.bestsub.risco$cp
summary.bestsub.risco$rsq
summary.bestsub.risco$adjr2
summary.bestsub.risco$bic

which.min(summary.bestsub.risco$cp)
which.max(summary.bestsub.risco$rsq)
which.max(summary.bestsub.risco$adjr2)
which.min(summary.bestsub.risco$bic)

## Sabendo o número de variáveis em cada modelo, utilizamos o R2 ajustado em detrimento do R2, e
## geramos o melhor modelo de acordo com o número de variáveis encontradas nos melhores modelos:

dt.mat.x.risco <- data.frame(model.matrix(Logit.Risco))
dt.mat.x.bic.risco <- dt.mat.x.risco[, summary.bestsub.risco$which[6,]]
dt.mat.x.adjr2.risco <- dt.mat.x.risco[, summary.bestsub.risco$which[17,]]
dt.mat.x.cp.risco <- dt.mat.x.risco[, summary.bestsub.risco$which[15,]]

dt.mat.x.bic.risco <- data.frame(Risco = Dados.Logit$Risco, dt.mat.x.bic.risco)
dt.mat.x.adjr2.risco <- data.frame(Risco = Dados.Logit$Risco, dt.mat.x.adjr2.risco)
dt.mat.x.cp.risco <- data.frame(Risco = Dados.Logit$Risco, dt.mat.x.cp.risco)

str(dt.mat.x.bic.risco)

#-- rodando modelos com variáveis selecionadas dos melhores modelos

mod.bic.risco <- glm(Risco ~ . - X.Intercept., family = binomial(link = "logit"), data = dt.mat.x.bic.risco)
summary(mod.bic.risco)

mod.cp.risco <- glm(Risco ~ . - X.Intercept., family = binomial(link = "logit"), data = dt.mat.x.cp.risco)
summary(mod.cp.risco)

mod.adjr2.risco <- glm(Risco ~ . - X.Intercept., family = binomial(link = "logit"), data = dt.mat.x.adjr2.risco)
summary(mod.adjr2.risco)

## Com isso, obtemos seis modelos com as melhores combinação de variáveis possíveis para análise.

EscolhaModeloGLM[1,2] <- PseudoR2(risco.backward)
EscolhaModeloGLM[2,2] <- PseudoR2(risco.forward)
EscolhaModeloGLM[3,2] <- PseudoR2(risco.both)
EscolhaModeloGLM[4,2] <- PseudoR2(mod.bic.risco)
EscolhaModeloGLM[5,2] <- PseudoR2(mod.cp.risco)
EscolhaModeloGLM[6,2] <- PseudoR2(mod.adjr2.risco)

EscolhaModeloGLM[1,3] <- BIC(risco.backward)
EscolhaModeloGLM[2,3] <- BIC(risco.forward)
EscolhaModeloGLM[3,3] <- BIC(risco.both)
EscolhaModeloGLM[4,3] <- BIC(mod.bic.risco)
EscolhaModeloGLM[5,3] <- BIC(mod.cp.risco)
EscolhaModeloGLM[6,3] <- BIC(mod.adjr2.risco)

EscolhaModeloGLM[1,4] <- AIC(risco.backward)
EscolhaModeloGLM[2,4] <- AIC(risco.forward)
EscolhaModeloGLM[3,4] <- AIC(risco.both)
EscolhaModeloGLM[4,4] <- AIC(mod.bic.risco)
EscolhaModeloGLM[5,4] <- AIC(mod.cp.risco)
EscolhaModeloGLM[6,4] <- AIC(mod.adjr2.risco)


#########################

##### Modelo para risco de estagnação
## metodologia stepwise para encontrar a combinação de indicadores que compõem o melhor modelo.

Logit.Estagnacao <- glm(Estagnacao ~
                          ProporcaoNotas567 +
                          MediaPonderadaNotas +
                          IndiceDeGini2010 +
                          Idhm2010 +
                          PercDe25AnosOuMaisComSuperiorCompleto2010 +
                          ExpectativaDeAnosDeEstudo2010 +
                          PropPosGraduadosRGInt +
                          PropSenior +
                          PropJunior +
                          MediaIDEB + 
                          IdadePPG +
                          Modalidade +
                          Nivel +
                          NaturezaJuridicaDetalhada +
                          Regiao +
                          GrandeArea, family = binomial(link = "logit"), data = Dados.Logit)

summary(Logit.Estagnacao)
PseudoR2(Logit.Estagnacao, which = "McFadden")
AIC(Logit.Estagnacao)
BIC(Logit.Estagnacao)

## Considerando Logit.Risco, já gerado, temos:

estagnacao.backward <- step(Logit.Estagnacao, direction = "backward")
summary(estagnacao.backward)

estagnacao.forward <- step(Logit.Estagnacao, direction = "forward")
summary(estagnacao.forward)

estagnacao.both <- step(Logit.Estagnacao, direction = "both")
summary(estagnacao.both)

## O melhor modelo, encontrado pela seleção stepwise, pode ser estimado utilizando o comando "formula":

formula(estagnacao.backward)
formula(estagnacao.forward)
formula(estagnacao.both)

## O segundo método que utilizaremos será pela seleção do best subset, 
## varrendo todas as combinações possíveis de variáveis preditoras para encontrar
## o conjunto com melhor R2, R2 ajustado, CP de Mallow ou Critério BIC. 

## Para começar, geramos o bestsub a partir do comando regsubsets:

bestsub.estagnacao <- regsubsets(formula(Logit.Estagnacao), data = Dados.Logit, nvmax = 10000)
summary.bestsub.estagnacao <- summary(bestsub.estagnacao)

## Analisando o summary.bestsub, vemos a significância de cada variável dentro de diferentes 
## modelos possíveis. Vemos então o melhor número de variáveis de acordo com cada critério.

summary.bestsub.estagnacao$cp
summary.bestsub.estagnacao$rsq
summary.bestsub.estagnacao$adjr2
summary.bestsub.estagnacao$bic

which.min(summary.bestsub.estagnacao$cp)
which.max(summary.bestsub.estagnacao$rsq)
which.max(summary.bestsub.estagnacao$adjr2)
which.min(summary.bestsub.estagnacao$bic)

## Sabendo o número de variáveis em cada modelo, utilizamos o R2 ajustado em detrimento do R2, e
## geramos o melhor modelo de acordo com o número de variáveis encontradas nos melhores modelos:

dt.mat.x.estagnacao <- data.frame(model.matrix(Logit.Estagnacao))
dt.mat.x.bic.estagnacao <- dt.mat.x.estagnacao[, summary.bestsub.estagnacao$which[4,]]
dt.mat.x.adjr2.estagnacao <- dt.mat.x.estagnacao[, summary.bestsub.estagnacao$which[19,]]
dt.mat.x.cp.estagnacao <- dt.mat.x.estagnacao[, summary.bestsub.estagnacao$which[16,]]

dt.mat.x.bic.estagnacao <- data.frame(Estagnacao = Dados.Logit$Estagnacao, dt.mat.x.bic.estagnacao)
dt.mat.x.adjr2.estagnacao <- data.frame(Estagnacao = Dados.Logit$Estagnacao, dt.mat.x.adjr2.estagnacao)
dt.mat.x.cp.estagnacao <- data.frame(Estagnacao = Dados.Logit$Estagnacao, dt.mat.x.cp.estagnacao)

str(dt.mat.x.bic.estagnacao)

#-- rodando modelos com variáveis selecionadas dos melhores modelos

mod.bic.estagnacao <- glm(Estagnacao ~ . - X.Intercept., family = binomial(link = "logit"), data = dt.mat.x.bic.estagnacao)
summary(mod.bic.estagnacao)

mod.cp.estagnacao <- glm(Estagnacao ~ . - X.Intercept., family = binomial(link = "logit"), data = dt.mat.x.cp.estagnacao)
summary(mod.cp.estagnacao)

mod.adjr2.estagnacao <- glm(Estagnacao ~ . - X.Intercept., family = binomial(link = "logit"), data = dt.mat.x.adjr2.estagnacao)
summary(mod.adjr2.estagnacao)

## Com isso, obtemos seis modelos com as melhores combinação de variáveis possíveis para análise.

EscolhaModeloGLM[1,5] <- PseudoR2(estagnacao.backward)
EscolhaModeloGLM[2,5] <- PseudoR2(estagnacao.forward)
EscolhaModeloGLM[3,5] <- PseudoR2(estagnacao.both)
EscolhaModeloGLM[4,5] <- PseudoR2(mod.bic.estagnacao)
EscolhaModeloGLM[5,5] <- PseudoR2(mod.cp.estagnacao)
EscolhaModeloGLM[6,5] <- PseudoR2(mod.adjr2.estagnacao)

EscolhaModeloGLM[1,6] <- BIC(estagnacao.backward)
EscolhaModeloGLM[2,6] <- BIC(estagnacao.forward)
EscolhaModeloGLM[3,6] <- BIC(estagnacao.both)
EscolhaModeloGLM[4,6] <- BIC(mod.bic.estagnacao)
EscolhaModeloGLM[5,6] <- BIC(mod.cp.estagnacao)
EscolhaModeloGLM[6,6] <- BIC(mod.adjr2.estagnacao)

EscolhaModeloGLM[1,7] <- AIC(estagnacao.backward)
EscolhaModeloGLM[2,7] <- AIC(estagnacao.forward)
EscolhaModeloGLM[3,7] <- AIC(estagnacao.both)
EscolhaModeloGLM[4,7] <- AIC(mod.bic.estagnacao)
EscolhaModeloGLM[5,7] <- AIC(mod.cp.estagnacao)
EscolhaModeloGLM[6,7] <- AIC(mod.adjr2.estagnacao)

#########################

##### Modelo para risco de involução
## metodologia stepwise para encontrar a combinação de indicadores que compõem o melhor modelo.

Logit.Involucao <- glm(Involucao ~
                         ProporcaoNotas567 +
                         MediaPonderadaNotas +
                         PPGsPorMunicipio +
                         PercDosOcupadosComSuperiorCompleto18AnosOuMais2010 +
                         PopulacaoTotal2010 +
                         Idhm2010 +
                         IdhmRenda2010 +
                         IdhmEducacao2010 +
                         SubindiceDeEscolaridadeIdhmEducacao2010 +
                         TaxaDeFrequenciaBrutaAoSuperior2010 +
                         PropPosGraduadosMun +
                         PropSenior +
                         PropJunior +
                         MediaIDEB +
                         PermanentesPorMinimo +  
                         IdadePPG +
                         Modalidade +
                         Nivel +
                         NaturezaJuridicaDetalhada +
                         Regiao +
                         GrandeArea, family = binomial(link = "logit"), data = Dados.Logit)
summary(Logit.Involucao)
PseudoR2(Logit.Involucao, which = "McFadden")
AIC(Logit.Involucao)
BIC(Logit.Involucao)

## Considerando Logit.Risco, já gerado, temos:

involucao.backward <- step(Logit.Involucao, direction = "backward")
summary(involucao.backward)

involucao.forward <- step(Logit.Involucao, direction = "forward")
summary(involucao.forward)

involucao.both <- step(Logit.Involucao, direction = "both")
summary(involucao.both)

## O melhor modelo, encontrado pela seleção stepwise, pode ser estimado utilizando o comando "formula":

formula(involucao.backward)
formula(involucao.forward)
formula(involucao.both)

## O segundo método que utilizaremos será pela seleção do best subset, 
## varrendo todas as combinações possíveis de variáveis preditoras para encontrar
## o conjunto com melhor R2, R2 ajustado, CP de Mallow ou Critério BIC. 

## Para começar, geramos o bestsub a partir do comando regsubsets:

bestsub.involucao <- regsubsets(formula(Logit.Involucao), data = Dados.Logit, nvmax = 10000)
summary.bestsub.involucao <- summary(bestsub.involucao)

## Analisando o summary.bestsub, vemos a significância de cada variável dentro de diferentes 
## modelos possíveis. Vemos então o melhor número de variáveis de acordo com cada critério.

summary.bestsub.involucao$cp
summary.bestsub.involucao$rsq
summary.bestsub.involucao$adjr2
summary.bestsub.involucao$bic

which.min(summary.bestsub.involucao$cp)
which.max(summary.bestsub.involucao$rsq)
which.max(summary.bestsub.involucao$adjr2)
which.min(summary.bestsub.involucao$bic)

## Sabendo o número de variáveis em cada modelo, utilizamos o R2 ajustado em detrimento do R2, e
## geramos o melhor modelo de acordo com o número de variáveis encontradas nos melhores modelos:

dt.mat.x.involucao <- data.frame(model.matrix(Logit.Involucao))
dt.mat.x.bic.involucao <- dt.mat.x.involucao[, summary.bestsub.involucao$which[4,]]
dt.mat.x.adjr2.involucao <- dt.mat.x.involucao[, summary.bestsub.involucao$which[14,]]
dt.mat.x.cp.involucao <- dt.mat.x.involucao[, summary.bestsub.involucao$which[13,]]

dt.mat.x.bic.involucao <- data.frame(Involucao = Dados.Logit$Involucao, dt.mat.x.bic.involucao)
dt.mat.x.adjr2.involucao <- data.frame(Involucao = Dados.Logit$Involucao, dt.mat.x.adjr2.involucao)
dt.mat.x.cp.involucao <- data.frame(Involucao = Dados.Logit$Involucao, dt.mat.x.cp.involucao)

str(dt.mat.x.bic.involucao)

#-- rodando modelos com variáveis selecionadas dos melhores modelos

mod.bic.involucao <- glm(Involucao ~ . - X.Intercept., family = binomial(link = "logit"), data = dt.mat.x.bic.involucao)
summary(mod.bic.involucao)

mod.cp.involucao <- glm(Involucao ~ . - X.Intercept., family = binomial(link = "logit"), data = dt.mat.x.cp.involucao)
summary(mod.cp.involucao)

mod.adjr2.involucao <- glm(Involucao ~ . - X.Intercept., family = binomial(link = "logit"), data = dt.mat.x.adjr2.involucao)
summary(mod.adjr2.involucao)

## Com isso, obtemos seis modelos com as melhores combinação de variáveis possíveis para análise.

EscolhaModeloGLM[1,8] <- PseudoR2(involucao.backward)
EscolhaModeloGLM[2,8] <- PseudoR2(involucao.forward)
EscolhaModeloGLM[3,8] <- PseudoR2(involucao.both)
EscolhaModeloGLM[4,8] <- PseudoR2(mod.bic.involucao)
EscolhaModeloGLM[5,8] <- PseudoR2(mod.cp.involucao)
EscolhaModeloGLM[6,8] <- PseudoR2(mod.adjr2.involucao)

EscolhaModeloGLM[1,9] <- BIC(involucao.backward)
EscolhaModeloGLM[2,9] <- BIC(involucao.forward)
EscolhaModeloGLM[3,9] <- BIC(involucao.both)
EscolhaModeloGLM[4,9] <- BIC(mod.bic.involucao)
EscolhaModeloGLM[5,9] <- BIC(mod.cp.involucao)
EscolhaModeloGLM[6,9] <- BIC(mod.adjr2.involucao)

EscolhaModeloGLM[1,10] <- AIC(involucao.backward)
EscolhaModeloGLM[2,10] <- AIC(involucao.forward)
EscolhaModeloGLM[3,10] <- AIC(involucao.both)
EscolhaModeloGLM[4,10] <- AIC(mod.bic.involucao)
EscolhaModeloGLM[5,10] <- AIC(mod.cp.involucao)
EscolhaModeloGLM[6,10] <- AIC(mod.adjr2.involucao)

#########################

##### Modelo para risco de descontinuidade
## metodologia stepwise para encontrar a combinação de indicadores que compõem o melhor modelo.

Logit.Descontinuidade <- glm(Descontinuidade ~
                               ProporcaoNotas567 +
                               MediaPonderadaNotas +
                               IndiceDeGini2010 +
                               PopulacaoTotal2010 +
                               ExpectativaDeAnosDeEstudo2010 +
                               DensidadePPGRGI +
                               PropSenior +
                               MediaIDEB +  
                               IdadePPG +
                               Modalidade +
                               Nivel +
                               NaturezaJuridicaDetalhada +
                               Regiao +
                               GrandeArea, family = binomial(link = "logit"), data = Dados.Logit)

summary(Logit.Descontinuidade)
PseudoR2(Logit.Descontinuidade, which = "McFadden")
AIC(Logit.Descontinuidade)
BIC(Logit.Descontinuidade)

## Considerando Logit.Risco, já gerado, temos:

descontinuidade.backward <- step(Logit.Descontinuidade, direction = "backward")
summary(descontinuidade.backward)

descontinuidade.forward <- step(Logit.Descontinuidade, direction = "forward")
summary(descontinuidade.forward)

descontinuidade.both <- step(Logit.Descontinuidade, direction = "both")
summary(descontinuidade.both)

## O melhor modelo, encontrado pela seleção stepwise, pode ser estimado utilizando o comando "formula":

formula(descontinuidade.backward)
formula(descontinuidade.forward)
formula(descontinuidade.both)

## O segundo método que utilizaremos será pela seleção do best subset, 
## varrendo todas as combinações possíveis de variáveis preditoras para encontrar
## o conjunto com melhor R2, R2 ajustado, CP de Mallow ou Critério BIC. 

## Para começar, geramos o bestsub a partir do comando regsubsets:

bestsub.descontinuidade <- regsubsets(formula(Logit.Descontinuidade), data = Dados.Logit, nvmax = 10000)
summary.bestsub.descontinuidade <- summary(bestsub.descontinuidade)

## Analisando o summary.bestsub, vemos a significância de cada variável dentro de diferentes 
## modelos possíveis. Vemos então o melhor número de variáveis de acordo com cada critério.

summary.bestsub.descontinuidade$cp
summary.bestsub.descontinuidade$rsq
summary.bestsub.descontinuidade$adjr2
summary.bestsub.descontinuidade$bic

which.min(summary.bestsub.descontinuidade$cp)
which.max(summary.bestsub.descontinuidade$rsq)
which.max(summary.bestsub.descontinuidade$adjr2)
which.min(summary.bestsub.descontinuidade$bic)

## Sabendo o número de variáveis em cada modelo, utilizamos o R2 ajustado em detrimento do R2, e
## geramos o melhor modelo de acordo com o número de variáveis encontradas nos melhores modelos:

dt.mat.x.descontinuidade <- data.frame(model.matrix(Logit.Descontinuidade))
dt.mat.x.bic.descontinuidade <- dt.mat.x.descontinuidade[, summary.bestsub.descontinuidade$which[5,]]
dt.mat.x.adjr2.descontinuidade <- dt.mat.x.descontinuidade[, summary.bestsub.descontinuidade$which[21,]]
dt.mat.x.cp.descontinuidade <- dt.mat.x.descontinuidade[, summary.bestsub.descontinuidade$which[18,]]

dt.mat.x.bic.descontinuidade <- data.frame(Descontinuidade = Dados.Logit$Descontinuidade, dt.mat.x.bic.descontinuidade)
dt.mat.x.adjr2.descontinuidade <- data.frame(Descontinuidade = Dados.Logit$Descontinuidade, dt.mat.x.adjr2.descontinuidade)
dt.mat.x.cp.descontinuidade <- data.frame(Descontinuidade = Dados.Logit$Descontinuidade, dt.mat.x.cp.descontinuidade)

str(dt.mat.x.bic.descontinuidade)

#-- rodando modelos com variáveis selecionadas dos melhores modelos

mod.bic.descontinuidade <- glm(Descontinuidade ~ . - X.Intercept., family = binomial(link = "logit"), data = dt.mat.x.bic.descontinuidade)
summary(mod.bic.descontinuidade)

mod.cp.descontinuidade <- glm(Descontinuidade ~ . - X.Intercept., family = binomial(link = "logit"), data = dt.mat.x.cp.descontinuidade)
summary(mod.cp.descontinuidade)

mod.adjr2.descontinuidade <- glm(Descontinuidade ~ . - X.Intercept., family = binomial(link = "logit"), data = dt.mat.x.adjr2.descontinuidade)
summary(mod.adjr2.descontinuidade)

## Com isso, obtemos seis modelos com as melhores combinação de variáveis possíveis para análise.

EscolhaModeloGLM[1,11] <- PseudoR2(descontinuidade.backward)
EscolhaModeloGLM[2,11] <- PseudoR2(descontinuidade.forward)
EscolhaModeloGLM[3,11] <- PseudoR2(descontinuidade.both)
EscolhaModeloGLM[4,11] <- PseudoR2(mod.bic.descontinuidade)
EscolhaModeloGLM[5,11] <- PseudoR2(mod.cp.descontinuidade)
EscolhaModeloGLM[6,11] <- PseudoR2(mod.adjr2.descontinuidade)

EscolhaModeloGLM[1,12] <- BIC(descontinuidade.backward)
EscolhaModeloGLM[2,12] <- BIC(descontinuidade.forward)
EscolhaModeloGLM[3,12] <- BIC(descontinuidade.both)
EscolhaModeloGLM[4,12] <- BIC(mod.bic.descontinuidade)
EscolhaModeloGLM[5,12] <- BIC(mod.cp.descontinuidade)
EscolhaModeloGLM[6,12] <- BIC(mod.adjr2.descontinuidade)

EscolhaModeloGLM[1,13] <- AIC(descontinuidade.backward)
EscolhaModeloGLM[2,13] <- AIC(descontinuidade.forward)
EscolhaModeloGLM[3,13] <- AIC(descontinuidade.both)
EscolhaModeloGLM[4,13] <- AIC(mod.bic.descontinuidade)
EscolhaModeloGLM[5,13] <- AIC(mod.cp.descontinuidade)
EscolhaModeloGLM[6,13] <- AIC(mod.adjr2.descontinuidade)

# formula dos modelos 

formula(risco.forward)
formula(estagnacao.forward)
formula(involucao.forward)
formula(descontinuidade.forward)

# rodando os modelos com dados de base reduzida

ModeloRisco <- glm(formula(risco.forward), family = binomial(link = "logit"), data = Dados.Logit_modelo)
ModeloEstagnacao <- glm(formula(estagnacao.forward), family = binomial(link = "logit"), data = Dados.Logit_modelo) 
ModeloInvolucao <- glm(formula(involucao.forward), family = binomial(link = "logit"), data = Dados.Logit_modelo) 
ModeloDescontinuidade <- glm(formula(descontinuidade.forward), family = binomial(link = "logit"), data = Dados.Logit_modelo) 

#---- criando ROC para os modelos 

names(Dados.Logit_teste)

ROC <- roc(Dados.Logit_teste$Risco, Dados.Logit_teste$previsao.risco)
Roc1 <- ggplot(Dados.Logit_teste, aes(m = previsao.risco, d = Risco)) + 
  geom_roc(n.cuts = 0, labels = FALSE, color = "blue") + 
  ggtitle("Risco Geral") +
  style_roc(guide = TRUE, ylab="Sensibilidade", xlab = "Especificidade \n", theme = theme_grey) +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x=0.625, y=0.175, label = paste("AUC = ", round(auc(ROC),4)))

ROC <- roc(Dados.Logit_teste$Risco, Dados.Logit_teste$previsao.estagnacao)
Roc2 <- ggplot(Dados.Logit_teste, aes(m = previsao.estagnacao, d = Estagnacao)) + 
  geom_roc(n.cuts = 0, labels = FALSE, color = "blue") + 
  ggtitle("Risco de Estagnação") +
  style_roc(guide = TRUE, ylab="Sensibilidade", xlab = "Especificidade \n", theme = theme_grey) +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x=0.625, y=0.175, label = paste("AUC = ", round(auc(ROC),4)))

ROC <- roc(Dados.Logit_teste$Risco, Dados.Logit_teste$previsao.involucao)
Roc3 <- ggplot(Dados.Logit_teste, aes(m = previsao.involucao, d = Involucao)) + 
  geom_roc(n.cuts = 0, labels = FALSE, color = "blue") + 
  ggtitle("Risco de Involução") +
  style_roc(guide = TRUE, ylab="Sensibilidade", xlab = "Especificidade \n", theme = theme_grey) +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x=0.625, y=0.175, label = paste("AUC = ", round(auc(ROC),4)))

ROC <- roc(Dados.Logit_teste$Risco, Dados.Logit_teste$previsao.descontinuidade)
Roc4 <- ggplot(Dados.Logit_teste, aes(m = previsao.descontinuidade, d = Descontinuidade)) + 
  geom_roc(n.cuts = 0, labels = FALSE, color = "blue", linejoin = "round") + 
  ggtitle("Risco de Descontinuidade") +
  style_roc(guide = TRUE, ylab="Sensibilidade", xlab = "Especificidade \n", theme = theme_grey) +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x=0.625, y=0.175, label = paste("AUC = ", round(auc(ROC),4)))

ggarrange(Roc1, Roc2, Roc3, Roc4,
          ncol = 2, nrow = 2, hjust = -0.5, vjust = 1,
          labels = c("(a)", "(b)", "(c)", "(d)"))


### Teste dos 4 fórmulas encontradas, com valores de corte c = 0.2, 0.4 e 0.6, e elaboração
### das respectivas matrizes de confusão

Dados.Logit_teste$previsao.risco <- predict(ModeloRisco, Dados.Logit_teste, type = "response")
Dados.Logit_teste$previsao.estagnacao <- predict(ModeloEstagnacao, Dados.Logit_teste, type = "response")
Dados.Logit_teste$previsao.involucao <- predict(ModeloInvolucao, Dados.Logit_teste, type = "response")
Dados.Logit_teste$previsao.descontinuidade <- predict(ModeloDescontinuidade, Dados.Logit_teste, type = "response")

Dados.Logit_teste$previsao.02.risco <- ifelse(Dados.Logit_teste$previsao.risco > 0.2, 1, 0)
Dados.Logit_teste$previsao.02.estagnacao <- ifelse(Dados.Logit_teste$previsao.estagnaca > 0.2, 1, 0)
Dados.Logit_teste$previsao.02.involucao <- ifelse(Dados.Logit_teste$previsao.involucao > 0.2, 1, 0)
Dados.Logit_teste$previsao.02.descontinuidade <- ifelse(Dados.Logit_teste$previsao.descontinuidade > 0.2, 1, 0)

Dados.Logit_teste$previsao.04.risco <- ifelse(Dados.Logit_teste$previsao.risco > 0.4, 1, 0)
Dados.Logit_teste$previsao.04.estagnacao <- ifelse(Dados.Logit_teste$previsao.estagnaca > 0.4, 1, 0)
Dados.Logit_teste$previsao.04.involucao <- ifelse(Dados.Logit_teste$previsao.involucao > 0.4, 1, 0)
Dados.Logit_teste$previsao.04.descontinuidade <- ifelse(Dados.Logit_teste$previsao.descontinuidade > 0.4, 1, 0)

Dados.Logit_teste$previsao.06.risco <- ifelse(Dados.Logit_teste$previsao.risco > 0.6, 1, 0)
Dados.Logit_teste$previsao.06.estagnacao <- ifelse(Dados.Logit_teste$previsao.estagnaca > 0.6, 1, 0)
Dados.Logit_teste$previsao.06.involucao <- ifelse(Dados.Logit_teste$previsao.involucao > 0.6, 1, 0)
Dados.Logit_teste$previsao.06.descontinuidade <- ifelse(Dados.Logit_teste$previsao.descontinuidade > 0.6, 1, 0)

nrow(filter(Dados.Logit_teste, Dados.Logit_teste$Risco == 1))
nrow(filter(Dados.Logit_teste, Dados.Logit_teste$Involucao == 1))
nrow(filter(Dados.Logit_teste, Dados.Logit_teste$Estagnacao == 1))
nrow(filter(Dados.Logit_teste, Dados.Logit_teste$Descontinuidade == 1))

table(Dados.Logit_teste$Risco, Dados.Logit_teste$previsao.02.risco)
table(Dados.Logit_teste$Risco, Dados.Logit_teste$previsao.04.risco)
table(Dados.Logit_teste$Risco, Dados.Logit_teste$previsao.06.risco)

table(Dados.Logit_teste$Estagnacao, Dados.Logit_teste$previsao.02.estagnacao)
table(Dados.Logit_teste$Estagnacao, Dados.Logit_teste$previsao.04.estagnacao)
table(Dados.Logit_teste$Estagnacao, Dados.Logit_teste$previsao.06.estagnacao)

table(Dados.Logit_teste$Involucao, Dados.Logit_teste$previsao.02.involucao)
table(Dados.Logit_teste$Involucao, Dados.Logit_teste$previsao.04.involucao)
table(Dados.Logit_teste$Involucao, Dados.Logit_teste$previsao.06.involucao)

table(Dados.Logit_teste$Descontinuidade, Dados.Logit_teste$previsao.02.descontinuidade)
table(Dados.Logit_teste$Descontinuidade, Dados.Logit_teste$previsao.04.descontinuidade)
table(Dados.Logit_teste$Descontinuidade, Dados.Logit_teste$previsao.06.descontinuidade)

#---- Calculando a acurácia do modelo
mean(Dados.Logit_teste$previsao.02.risco== Dados.Logit_teste$Risco)
mean(Dados.Logit_teste$previsao.02.estagnacao == Dados.Logit_teste$Estagnacao)
mean(Dados.Logit_teste$previsao.02.involucao == Dados.Logit_teste$Involucao)
mean(Dados.Logit_teste$previsao.02.descontinuidade == Dados.Logit_teste$Descontinuidade)

mean(Dados.Logit_teste$previsao.04.risco== Dados.Logit_teste$Risco)
mean(Dados.Logit_teste$previsao.04.estagnacao == Dados.Logit_teste$Estagnacao)
mean(Dados.Logit_teste$previsao.04.involucao == Dados.Logit_teste$Involucao)
mean(Dados.Logit_teste$previsao.04.descontinuidade == Dados.Logit_teste$Descontinuidade)

mean(Dados.Logit_teste$previsao.06.risco== Dados.Logit_teste$Risco)
mean(Dados.Logit_teste$previsao.06.estagnacao == Dados.Logit_teste$Estagnacao)
mean(Dados.Logit_teste$previsao.06.involucao == Dados.Logit_teste$Involucao)
mean(Dados.Logit_teste$previsao.06.descontinuidade == Dados.Logit_teste$Descontinuidade)


#---- Analise adicional da experiência do coordenador de PPG

Dados.ExpCoord <- Dados[complete.cases(Dados[,66:68]),] # cria subset somente com PPGs que responderam consulta do CGEE
mod.expcoord <- glm(Risco ~ ExperienciaCoordenador, 
                  family = binomial(link = "logit"), data = Dados.ExpCoord)

mod.expcoord.estag <- glm(Estagnacao ~ ExperienciaCoordenador, 
                    family = binomial(link = "logit"), data = Dados.ExpCoord)

mod.expcoord.inv <- glm(Involucao ~ ExperienciaCoordenador, 
                    family = binomial(link = "logit"), data = Dados.ExpCoord)

mod.expcoord.desc <- glm(Descontinuidade ~ ExperienciaCoordenador, 
                    family = binomial(link = "logit"), data = Dados.ExpCoord)

summary(mod.expcoord)

summary(mod.expcoord.estag)

summary(mod.expcoord.inv)

summary(mod.expcoord.desc)

plot.coord1 <- ggplot(Dados.ExpCoord, aes(x=ExperienciaCoordenador, y=Risco)) + 
  ggtitle("Risco Identificado") +
  labs(y="", x = "Experiência de gestão do Coordenador de PPG (meses) \n") + 
  geom_jitter(width = 0.0, height = 0.05, size=2, color="red", fill="orange", shape=21, alpha = 0.2) + 
  annotate("text", x=100, y=0.3, label = paste("P-Valor =", format(coef(summary(mod.expcoord))[,4][2], digits=3, nsmall=2), 
                                              "  R2 =", format(PseudoR2(mod.expcoord, which = "McFadden"), digits=3, nsmall=2))) +
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) +
  theme(plot.title = element_text(hjust = 0.5))

plot.coord2 <- ggplot(Dados.ExpCoord, aes(x=ExperienciaCoordenador, y=Estagnacao)) + 
  ggtitle("Risco de Estagnação") +
  labs(y="", x = "Experiência de gestão do Coordenador de PPG (meses) \n") + 
  geom_jitter(width = 0.0, height = 0.05, size=2, color="red", fill="orange", shape=21, alpha = 0.2) + 
  annotate("text", x=100, y=0.3, label = paste("P-Valor =", format(coef(summary(mod.expcoord.estag))[,4][2], digits=3, nsmall=2), 
                                              "  R2 =", format(PseudoR2(mod.expcoord.estag, which = "McFadden"), digits=3, nsmall=2))) +
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) +
  theme(plot.title = element_text(hjust = 0.5))

plot.coord3 <- ggplot(Dados.ExpCoord, aes(x=ExperienciaCoordenador, y=Involucao)) + 
  ggtitle("Risco de Involução") +
  labs(y="", x = "Experiência de gestão do Coordenador de PPG (meses) \n") + 
  geom_jitter(width = 0.0, height = 0.05, size=2, color="red", fill="orange", shape=21, alpha = 0.2) + 
  annotate("text", x=100, y=0.3, label = paste("P-Valor =", format(coef(summary(mod.expcoord.inv))[,4][2], digits=3, nsmall=2), 
                                              "  R2 =", format(PseudoR2(mod.expcoord.inv, which = "McFadden"), digits=3, nsmall=2))) +
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) +
  theme(plot.title = element_text(hjust = 0.5))

plot.coord4 <- ggplot(Dados.ExpCoord, aes(x=ExperienciaCoordenador, y=Descontinuidade)) + 
  ggtitle("Risco de Descontinuidade") +
  labs(y="", x = "Experiência de gestão do Coordenador de PPG (meses) \n") + 
  geom_jitter(width = 0.0, height = 0.05, size=2, color="red", fill="orange", shape=21, alpha = 0.2) + 
  annotate("text", x=100, y=0.3, label = paste("P-Valor =", format(coef(summary(mod.expcoord.desc))[,4][2], digits=3, nsmall=2), 
                                              "  R2 =", format(PseudoR2(mod.expcoord.desc, which = "McFadden"), digits=3, nsmall=2))) +
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) +
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(plot.coord1, plot.coord2, plot.coord3, plot.coord4,
          hjust = -1, vjust = 2, ncol = 2, nrow = 2,
          labels = c("(a)", "(b)", "(c)", "(d)"))

#---- encontra matrizes de "confusão" com valores de corte c = 0.3, 0.5 e 0.7. 

Dados.ExpCoord$PredRisco <- ifelse(mod.expcoord$fitted.values > 0.3, 1, 0)
table(Dados.ExpCoord$PredRisco, Dados.ExpCoord$Risco)

Dados.ExpCoord$PredRisco <- ifelse(mod.expcoord$fitted.values > 0.5, 1, 0)
table(Dados.ExpCoord$PredRisco, Dados.ExpCoord$Risco)

Dados.ExpCoord$PredRisco <- ifelse(mod.expcoord$fitted.values > 0.7, 1, 0)
table(Dados.ExpCoord$PredRisco, Dados.ExpCoord$Risco)

# Resultados para corte 0.3
Dados.ExpCoord$PredRisco <- ifelse(mod.expcoord$fitted.values > 0.3, 1, 0)
cmatrix <- table(Dados.ExpCoord$PredRisco, Dados.ExpCoord$Risco)


#---- Analisar regionalidades

mod.regiao <- glm(Risco ~ as.factor(Regiao), 
                family = binomial(link = "logit"), data = Dados)
summary(mod.regiao)
PseudoR2(mod.regiao, which = "McFadden")
AIC(mod.regiao); BIC(mod.regiao)

# Região Geográfica Intermediária

mod.RGInt <- glm(Risco ~ as.factor(RegiaoGeograficaIntermediaria), 
                family = binomial(link = "logit"), data = Dados)
summary(mod.RGInt)
PseudoR2(mod.RGInt, which = "McFadden")
AIC(mod.RGInt); BIC(mod.RGInt)

# UF
mod.uf <- glm(Risco ~ as.factor(Uf), 
                family = binomial(link = "logit"), data = Dados)
summary(mod.uf)
PseudoR2(mod.uf, which = "McFadden")
AIC(mod.uf); BIC(mod.uf)

# Região Geográfica Imediata
mod.RGI <- glm(Risco ~ as.factor(RegiaoGeograficaImediata), 
                family = binomial(link = "logit"), data = Dados)
summary(mod.RGI)
PseudoR2(mod.RGI, which = "McFadden")
AIC(mod.RGI); BIC(mod.RGI)

# Municipio
mod.municipio <- glm(Risco ~ as.factor(Municipio), 
                family = binomial(link = "logit"), data = Dados)
summary(mod.municipio)
PseudoR2(mod.municipio, which = "McFadden")
AIC(mod.municipio); BIC(mod.municipio)



##############################
# Árvore de Classificação
##############################

#---- Recorte de dados
Dados.CaRTs <- as.data.frame(subset(Dados, select=c(Risco,
                                                         Estagnacao,
                                                         Involucao,
                                                         Descontinuidade,
                                                         ProporcaoNotas567,
                                                         MediaPonderadaNotas,
                                                         PPGsPorMunicipio,
                                                         IndiceDeGini2010,
                                                         PercDosOcupadosComSuperiorCompleto18AnosOuMais2010,
                                                         PopulacaoTotal2010,
                                                         PopulacaoUrbana2010,
                                                         Idhm2010,
                                                         IdhmRenda2010,
                                                         IdhmLongevidade2010,
                                                         IdhmEducacao2010,
                                                         SubindiceDeEscolaridadeIdhmEducacao2010,
                                                         PercDe25AnosOuMaisComSuperiorCompleto2010,
                                                         ExpectativaDeAnosDeEstudo2010,
                                                         TaxaDeFrequenciaBrutaAoSuperior2010,
                                                         RendaPerCapita2010,
                                                         PropPosGraduadosMun,
                                                         PropPosGraduadosRGI,
                                                         PropPosGraduadosRGInt,
                                                         DensidadePPGMun,
                                                         DensidadePPGRGI,
                                                         DensidadePPGRGInt,
                                                         PropSenior,
                                                         PropJunior,
                                                         PropSeniorJunior,
                                                         PermanentesPorMinimo,
                                                         MediaIDEB,
                                                         IdadePPG,
                                                         ExperienciaCoordenador,
                                                         Modalidade,
                                                         Nivel,
                                                         NaturezaJuridica,
                                                         NaturezaJuridicaDetalhada,
                                                         Regiao,
                                                         GrandeArea,
                                                         Colegio,
                                                         AreaDeAvaliacao) ) )

names(Dados.CaRTs)

# Converte categorias de risco em fatores

for (i in 1:4) {
  Dados.CaRTs[,i] <- ifelse(Dados.CaRTs[,i] == 1, "Risco", "Sem Risco")
  Dados.CaRTs[,i] <- factor(Dados.CaRTs[,i])
}

Dados.CaRTs$Risco <- with(Dados.CaRTs, relevel(factor(Dados.CaRTs$Risco), "Sem Risco"))
Dados.CaRTs$Estagnacao <- with(Dados.CaRTs, relevel(factor(Dados.CaRTs$Estagnacao), "Sem Risco"))
Dados.CaRTs$Involucao <- with(Dados.CaRTs, relevel(factor(Dados.CaRTs$Involucao), "Sem Risco"))
Dados.CaRTs$Descontinuidade <- with(Dados.CaRTs, relevel(factor(Dados.CaRTs$Descontinuidade), "Sem Risco"))

for (i in 34:41) {
  Dados.CaRTs[,i] <- factor(Dados.CaRTs[,i])
}

Dados.CaRTs$GrandeArea <- revalue(Dados.CaRTs$GrandeArea, 
                                       c("Ciências Agrárias"="Agrárias", 
                                         "Ciências Biológicas"="Biológicas",
                                         "Ciências da Saúde"="C.Saúde",
                                         "Ciências Exatas e da Terra"="Exat/Terra",
                                         "Ciências Humanas"="Humanas",
                                         "Ciências Sociais Aplicadas"="Soc.Aplicadas",
                                         "Engenharias"="Eng.",
                                         "Linguística, Letras e Artes"="Let. Artes",
                                         "Multidisciplinar"="Mult."))

Dados.CaRTs$GrandeArea <- with(Dados.CaRTs, relevel(factor(Dados.CaRTs$GrandeArea), "Biológicas"))
summary(Dados.CaRTs$GrandeArea)

Dados.CaRTs$Colegio <- revalue(Dados.CaRTs$Colegio, 
                                    c("Ciências Exatas, Tecnológicas e Multidisciplinar"="C. Exatas, Tec. e Mult."))

summary(Dados.CaRTs$Colegio)

Dados.CaRTs$NaturezaJuridicaDetalhada <- revalue(Dados.CaRTs$NaturezaJuridicaDetalhada, 
                                                      c("Filantrópica ou Comunitária" = "F/Com.", 
                                                        "Confessional" = "Conf.",
                                                        "Privada com fins lucrativos"="PFL",
                                                        "Privada sem fins lucrativos"="PSL",
                                                        "Pública Estadual"="Estadual",
                                                        "Pública Federal" = "Federal",
                                                        "Pública Municipal" = "Municipal"))

summary(Dados.CaRTs$NaturezaJuridicaDetalhada)

Dados.CaRTs$Nivel <- revalue(Dados.CaRTs$Nivel, 
                                           c("Doutorado" = "DO", 
                                             "Mestrado" = "ME",
                                             "Mestrado Profissional"="MP",
                                             "Mestrado/Doutorado"="M/D"))

Dados.CaRTs$Nivel <- with(Dados.CaRTs, relevel(factor(Dados.CaRTs$Nivel), "ME"))

summary(Dados.CaRTs$Nivel)

Dados.CaRTs$Modalidade <- revalue(Dados.CaRTs$Modalidade, 
                             c("ACADÊMICO" = "ACAD.", 
                               "PROFISSIONAL" = "PROF."))

summary(Dados.CaRTs$Modalidade)

str(Dados.CaRTs)

# elaborar as primeiras quatro árvores, com algoritmo Breiman
par(mfrow=c(2,2), mar=c(6, 3.5, 3.5, 0.5), mgp = c(1.5, 0.5, 0), tck = -0.01, oma=c(0, 0, 1, 0), cex.lab = 1)
tree <- list()
for (i in 1:4) {
tree[[i]] <- rpart(Dados.CaRTs[,i] ~ ProporcaoNotas567
             + MediaPonderadaNotas
             + PPGsPorMunicipio
             + IndiceDeGini2010
             + PercDosOcupadosComSuperiorCompleto18AnosOuMais2010
             + PopulacaoTotal2010
             + PopulacaoUrbana2010
             + Idhm2010
             + IdhmRenda2010
             + IdhmLongevidade2010
             + IdhmEducacao2010
             + SubindiceDeEscolaridadeIdhmEducacao2010
             + PercDe25AnosOuMaisComSuperiorCompleto2010
             + ExpectativaDeAnosDeEstudo2010
             + TaxaDeFrequenciaBrutaAoSuperior2010
             + RendaPerCapita2010
             + PropPosGraduadosMun
             + PropPosGraduadosRGI
             + PropPosGraduadosRGInt
             + MediaIDEB
             + ExperienciaCoordenador
             + PermanentesPorMinimo
             + IdadePPG
             + PropSenior
             + PropJunior
             + PropSeniorJunior
             + Modalidade
             + Nivel
             + NaturezaJuridicaDetalhada
             + Regiao
             + GrandeArea
             + AreaDeAvaliacao, data=Dados.CaRTs, control = rpart.control(cp = 0))

printcp(tree[[i]]) # exibe os resultados 
plotcp(tree[[i]], uniform = TRUE) # visualiza resultados de validação cruzada 
summary(tree[[i]]) # sumário detalhados dos splits
}

tree[[1]] # exibe árvore em formato texto

# plotar árvore 
plot(tree[[1]], uniform=TRUE, 
     main="Árvore de Classificação para Risco")
text(tree[[1]], use.n=TRUE, all=TRUE, cex=.8)

# executando prune da árvore para risco
tree2 <- prune(tree[[1]], cp = 0.029)
plotcp(tree2)

# plotar árvore 
plot(tree2, uniform=TRUE, 
     main="Árvore de Classificação para Risco")
text(tree2, use.n=TRUE, all=TRUE, cex=.8)
tree2

#############################################
# Classification Tree Conditional Inference
#############################################
tree.inf <- list()
for (i in 1:4) {
  tree.inf[[i]] <- ctree(Dados.CaRTs[,i] ~ ProporcaoNotas567
                     + MediaPonderadaNotas
                     + PPGsPorMunicipio
                     + IndiceDeGini2010
                     + PercDosOcupadosComSuperiorCompleto18AnosOuMais2010
                     + PopulacaoTotal2010
                     + PopulacaoUrbana2010
                     + Idhm2010
                     + IdhmRenda2010
                     + IdhmLongevidade2010
                     + IdhmEducacao2010
                     + SubindiceDeEscolaridadeIdhmEducacao2010
                     + PercDe25AnosOuMaisComSuperiorCompleto2010
                     + ExpectativaDeAnosDeEstudo2010
                     + TaxaDeFrequenciaBrutaAoSuperior2010
                     + RendaPerCapita2010
                     + PropPosGraduadosMun
                     + PropPosGraduadosRGI
                     + PropPosGraduadosRGInt
                     + MediaIDEB
                     + ExperienciaCoordenador
                     + PermanentesPorMinimo
                     + IdadePPG
                     + PropSenior
                     + PropJunior
                     + PropSeniorJunior
                     + Modalidade
                     + Nivel
                     + NaturezaJuridicaDetalhada
                     + Regiao
                     + GrandeArea, data=Dados.CaRTs, maxsurrogate = 3, control = ctree_control(minsplit = 10, minbucket = 5, multiway = TRUE))
  
  summary(tree.inf[[i]]) # sumário detalhados dos splits
}


plot(tree.inf[[1]], uniform = TRUE, gp = gpar(fontsize = 8),  
     inner_panel=node_inner,
     ip_args=list(abbreviate = FALSE, id = TRUE),
     ep_args=list(abbreviate = TRUE))

plot(tree.inf[[2]], uniform = TRUE, gp = gpar(fontsize = 10),  
     inner_panel=node_inner,
     ip_args=list(abbreviate = FALSE, id = TRUE),
     ep_args=list(abbreviate = TRUE))

plot(tree.inf[[3]], uniform = TRUE, gp = gpar(fontsize = 10),  
     inner_panel=node_inner,
     ip_args=list(abbreviate = FALSE, id = TRUE),
     ep_args=list(abbreviate = TRUE))

plot(tree.inf[[4]], uniform = TRUE, gp = gpar(fontsize = 10),  
     inner_panel=node_inner,
     ip_args=list(abbreviate = FALSE, id = TRUE),
     ep_args=list(abbreviate = TRUE))
