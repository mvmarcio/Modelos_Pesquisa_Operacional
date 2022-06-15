
#Analise Discriminante Linear - ADL

#Pacotes necessarios
library(readxl)
library(openxlsx)
library(MASS)
library(caret)
library(e1071)
library(biotools)
library(MVN)
library(goftest)
library(nlme)
library(klaR)
library(Hmisc)
library(psych)
library(DiscriMiner)
library(car)
library(mvtnorm)

#1. Importacao do Banco de dados em .xlsx

BD_DF <- read_excel("Consultorias/Lais_Moreira/Setembro_2020/BD_DF.xlsx")

#Remocao de NA's (celulas vazias por sigilo do IBGE ou DPP sem morador)
bancodedados <- na.omit(BD_DF)
View(bancodedados)

#Novo Banco de Dados sem NAs: bancodedados. 
#24 linhas foram apagadas: 19 de sigilo e 4 de DPP's sem nenhum morador no DF, restando 4069 observacoes.

#Salvar no PC 
write.xlsx(bancodedados,"D:/Metodologia_R/Resultado_final_LDA/bancodedados.xlsx")

#2. Calculo de Indicadores

attach(bancodedados)

#1 Numero de DPP no Setor:
i_num_dpp <- V001_Bas_Num_DPP
i_num_dpp <- data.frame(i_num_dpp)

#2 Numero de Pessoas Residentes/Moradores em DPP:
i_num_mor_dpp <- V002_Bas_Num_Mor_em_DPP
i_num_mor_dpp <- data.frame(i_num_mor_dpp)

#3 Numero de Moradores por DPP:
i_num_med_mor_por_dpp <- (V002_Bas_Num_Mor_em_DPP/V001_Bas_Num_DPP)
i_num_med_mor_por_dpp <- data.frame(i_num_med_mor_por_dpp)

#4 Percentual de Pessoas Residentes Brancas
i_perc_pes_res_brancas <- (V002_P03_Pes_Res_brancas/V001_P03_Pessoas_Residentes)*(100)
i_perc_pes_res_brancas <- data.frame(i_perc_pes_res_brancas)

#5 Percentual de PR do sexo feminino
i_perc_PR_sexo_fem <- (V001_R01_PR_sexo_fem/V001_R02_Pes_Resp)*(100)
i_perc_PR_sexo_fem <- data.frame(i_perc_PR_sexo_fem)

#6 Numero de Domicilios Improvisados:
i_num_dom_imp <- V001_DR_Num_Dom_Imp
i_num_dom_imp <- data.frame(i_num_dom_imp)

#7 Porcentagem de DPP com outra forma de ocupacao
i_perc_dpp_outra_ocup <- (V011_D01_Mor_DPP_outra_cond_ocup/V002_Bas_Num_Mor_em_DPP)*(100)
i_perc_dpp_outra_ocup <- data.frame(i_perc_dpp_outra_ocup)


#8 Porcentagem de PR de ate 30 anos:
i_perc_pr_ate_30 <- ((V002_R02_PR_com_10+V003_R02_PR_com_11+
                        V004_R02_PR_com_12+V005_R02_PR_com_13+V006_R02_PR_com_14+
                        V007_R02_PR_com_15+V008_R02_PR_com_16+V009_R02_PR_com_17+
                        V010_R02_PR_com_18+V011_R02_PR_com_19+V012_R02_PR_com_20+
                        V013_R02_PR_com_21+V014_R02_PR_com_22+V015_R02_PR_com_23+
                        V016_R02_PR_com_24+V017_R02_PR_com_25+V018_R02_PR_com_26+
                        V019_R02_PR_com_27+V020_R02_PR_com_28+V021_R02_PR_com_29)/
                       (V001_R02_Pes_Resp))*(100)
i_perc_pr_ate_30 <- data.frame(i_perc_pr_ate_30)

#9 Porcentagem de PR nao alfabetizadas:
i_perc_PR_nalf <- ((1)-(V093_R02_PR_alf/V001_R02_Pes_Resp))*(100)
i_perc_PR_nalf <- data.frame(i_perc_PR_nalf)

#10 Porcentagem de PR que ate 29 anos e nao sao alfabetizadas:
i_perc_PR_ate_30_nalf <- (((V002_R02_PR_com_10 + V003_R02_PR_com_11 + V004_R02_PR_com_12+ V005_R02_PR_com_13 +
                              V006_R02_PR_com_14 + V007_R02_PR_com_15 + V008_R02_PR_com_16+V009_R02_PR_com_17 +
                              V010_R02_PR_com_18 + V011_R02_PR_com_19 + V012_R02_PR_com_20+V013_R02_PR_com_21 +
                              V014_R02_PR_com_22 + V015_R02_PR_com_23 + V016_R02_PR_com_24+V017_R02_PR_com_25 +
                              V018_R02_PR_com_26 + V019_R02_PR_com_27 + V020_R02_PR_com_28+V021_R02_PR_com_29) - 
                             (V094_R02_PR_alf_10_a_14 + V095_R02_PR_alf_15_a_19 + V096_R02_PR_alf_20_a_24 + 
                              V097_R02_PR_alf_25_a_29))/(V001_R02_Pes_Resp))*(100)

i_perc_PR_ate_30_nalf <- data.frame(i_perc_PR_ate_30_nalf)

#11 Porcentagem de Pessoas Responsaveis moradoras em DPP com Renda ate 3SM:
i_perc_pr_ren_ate_3_SM <- ((`V067_RR_PR_DPP_Rend_ate_1-2_SM`+
                              `V068_RR_PR_DPP_Rend_1-2_a_1_SM`+
                              V069_RR_PR_DPP_Rend_1_a_2_SM+
                              V070_RR_PR_DPP_Rend_2_a_3_SM+
                              V076_RR_PR_DPP_Sem_Rend)/(V001_Bas_Num_DPP))*(100)

i_perc_pr_ren_ate_3_SM <- data.frame(i_perc_pr_ren_ate_3_SM)

#12 Renda media das PR por DPP
i_Renda_Med_PR_DPP <- V005_Bas_Renda_Med_PR_DPP
i_Renda_Med_PR_DPP <- data.frame(i_Renda_Med_PR_DPP)

#13 Porcentagem de DPP sem coleta de lixo
i_perc_dpp_sem_col_lixo <- ((1)-(V035_D01_DPP_com_col_lixo/V001_Bas_Num_DPP))*(100)
i_perc_dpp_sem_col_lixo <- data.frame(i_perc_dpp_sem_col_lixo)


#14 Porcentagem de DPP sem ligacao a rede de Abastecimento de agua
i_perc_DPP_sem_ab_agua <- ((1)-(V012_D01_DPP_com_ab_agua/V001_Bas_Num_DPP))*(100)
i_perc_DPP_sem_ab_agua <- data.frame(i_perc_DPP_sem_ab_agua)

#15 Porcentagem de DPP sem Banheiros ou Sanitarios
i_perc_DPP_sem_ban_san <- (V023_D01_DPP_sem_ban_san/V001_Bas_Num_DPP)*(100)
i_perc_DPP_sem_ban_san <- data.frame(i_perc_DPP_sem_ban_san)

#16 Porcentagem de DPP sem ligacao de Rede de Esgoto ou Fossa Septica
i_perc_DPP_sem_esg_fos <- ((1)-((V017_D01_DPP_com_ban_esg_red+V018_D01_DPP_com_ban_esg_fos)/V016_D01_DPP_com_ban_san))*(100)
i_perc_DPP_sem_esg_fos <- data.frame(i_perc_DPP_sem_esg_fos)

#17 Numero Medio de Banheiros por Habitante de DPP
i_num_med_ban_hab <- (((V025_D01_DPPcom_1_ban*(1))+(V026_D01_DPP_com_2_ban*(2))+(V027_D01_DPP_com_3_ban*(3))+
                         (V028_D01_DPP_com_4_ban*(4))+(V029_D01_DPP_com_5_ban*(5))+(V030_D01_DPP_com_6_ban*(6))+
                         (V031_D01_DPP_com_7_ban*(7))+(V032_D01_DPP_com_8_ban*(8))+(V033_D01_DPP_com_9_ban*(9)))/(V024_D01_DPP_com_ban))
i_num_med_ban_hab <- data.frame(i_num_med_ban_hab)

#Banco de Dados de Indicadores
bancodedados_ind <- data.frame(Cod_Setor, Situacao_setor, Tipo_setor, i_num_dpp, i_num_mor_dpp, i_num_med_mor_por_dpp,
                               i_perc_pes_res_brancas, i_perc_PR_sexo_fem, i_num_dom_imp, 
                               i_perc_dpp_outra_ocup, i_perc_pr_ate_30, i_perc_PR_nalf, 
                               i_perc_PR_ate_30_nalf, i_perc_pr_ren_ate_3_SM, i_Renda_Med_PR_DPP,
                               i_perc_dpp_sem_col_lixo, i_perc_DPP_sem_ab_agua, i_perc_DPP_sem_ban_san, 
                               i_perc_DPP_sem_esg_fos, i_num_med_ban_hab)
#Salvar no PC
write.xlsx(bancodedados_ind, "D:/Metodologia_R/Resultado_final_LDA/bancodedados_ind_DF.xlsx")


#3. Estatistica Descritiva

#3.1 Numero de zeros em cada indicador 

#Quais sao os indicadores que apresentam maior numero de zeros?

sum(bancodedados_ind$i_num_dpp==0) #1- numero de zeros = 0
sum(bancodedados_ind$i_num_mor_dpp==0) #2- numero de zeros = 0
sum(bancodedados_ind$i_num_med_mor_por_dpp==0) #3- numero de zeros = 0
sum(bancodedados_ind$i_perc_pes_res_brancas==0) #4- numero de zeros = 0
sum(bancodedados_ind$i_perc_PR_sexo_fem==0) #5- numero de zeros = 0
sum(bancodedados_ind$i_num_dom_imp==0) #6- numero de zeros= 1550
sum(bancodedados_ind$i_perc_dpp_outra_ocup==0) #7- numero de zero s= 2966
sum(bancodedados_ind$i_perc_pr_ate_30==0) #8- numero de zeros = 14
sum(bancodedados_ind$i_perc_PR_nalf==0) #9 - numero de zeros = 895 
sum(bancodedados_ind$i_perc_PR_ate_30_nalf==0) #10 - numero de zeros = 3315
sum(bancodedados_ind$i_perc_pr_ren_ate_3_SM==0) #11 - numero de zeros = 3
sum(bancodedados_ind$i_Renda_Med_PR_DPP==0) #12 - numero de zeros = 0
sum(bancodedados_ind$i_perc_dpp_sem_col_lixo==0) #13 - numero de zeros = 
sum(bancodedados_ind$i_perc_DPP_sem_ab_agua==0) #14 - numero de zeros = 3621
sum(bancodedados_ind$i_perc_DPP_sem_ban_san==0) #15 - numero de zeros = 3762
sum(bancodedados_ind$i_perc_DPP_sem_esg_fos==0) #16 - numero de zeros = 2778
sum(bancodedados_ind$i_num_med_ban_hab==0) #17 - numero de zeros = 0

#3.2 Gerar a Estatistica Descritiva

#Divisao dos Setores
setor0 <- bancodedados_ind[bancodedados_ind$Tipo_setor==0,]
setor1 <- bancodedados_ind[bancodedados_ind$Tipo_setor==1,]
View(setor0)
View(setor1)

#Dados da Estatistica Descritiva
summary(setor0)
summary(setor1)


#3.3 Analise Grafica dos Indicadores: comparacao entre os Grupos (0 e 1)

#Avalia a frequencia/distribuicao dos valores de cada indicador por grupo.
#Ou seja, verifica a qualidade das variaveis ind. para explicar a Variavel Dependente (Tipo de setor).

ldahist(bancodedados_ind$i_num_dpp, g = bancodedados_ind$Tipo_setor, col = "#c00000") #1
ldahist(bancodedados_ind$i_num_mor_dpp, g = bancodedados_ind$Tipo_setor, col = "darkgoldenrod2") #2
ldahist(bancodedados_ind$i_num_med_mor_por_dpp, g = bancodedados_ind$Tipo_setor, col = "#c00000") #3
ldahist(bancodedados_ind$i_perc_pes_res_brancas, g= bancodedados_ind$Tipo_setor, col = "darkgoldenrod2") #4
ldahist(bancodedados_ind$i_perc_PR_sexo_fem, g = bancodedados_ind$Tipo_setor, col = "#c00000") #5
ldahist(bancodedados_ind$i_num_dom_imp, g = bancodedados_ind$Tipo_setor, col = "darkgoldenrod2") #6
ldahist(bancodedados_ind$i_perc_dpp_outra_ocup, g = bancodedados_ind$Tipo_setor, col = "#c00000") #7
ldahist(bancodedados_ind$i_perc_pr_ate_30, g = bancodedados_ind$Tipo_setor, col = "darkgoldenrod2") #8
ldahist(bancodedados_ind$i_perc_PR_nalf, g = bancodedados_ind$Tipo_setor, col = "#c00000") #9
ldahist(bancodedados_ind$i_perc_PR_ate_30_nalf, g = bancodedados_ind$Tipo_setor, col = "darkgoldenrod2") #10
ldahist(bancodedados_ind$i_perc_pr_ren_ate_3_SM, g = bancodedados_ind$Tipo_setor, col = "#c00000") #11
ldahist(bancodedados_ind$i_Renda_Med_PR_DPP, g = bancodedados_ind$Tipo_setor, col = "darkgoldenrod2") #12
ldahist(bancodedados_ind$i_perc_dpp_sem_col_lixo, g = bancodedados_ind$Tipo_setor, col = "#c00000") #13
ldahist(bancodedados_ind$i_perc_DPP_sem_ab_agua, g = bancodedados_ind$Tipo_setor, col = "darkgoldenrod2") #14
ldahist(bancodedados_ind$i_perc_DPP_sem_ban_san, g = bancodedados_ind$Tipo_setor, col = "#c00000") #15
ldahist(bancodedados_ind$i_perc_DPP_sem_esg_fos, g = bancodedados_ind$Tipo_setor, col = "darkgoldenrod2") #16
ldahist(bancodedados_ind$i_num_med_ban_hab, g = bancodedados_ind$Tipo_setor, col = "#c00000") #17

#4. Procedimento Stepwise - Selecao de variaveis

#Comando greedy.wilks do pacote klaR

stepwise <- greedy.wilks (Tipo_setor ~ i_num_dpp + i_num_mor_dpp + i_num_med_mor_por_dpp +
                            i_perc_pes_res_brancas + i_perc_PR_sexo_fem + i_num_dom_imp + 
                            i_perc_dpp_outra_ocup + i_perc_pr_ate_30 + i_perc_PR_nalf + 
                            i_perc_PR_ate_30_nalf + i_perc_pr_ren_ate_3_SM + i_Renda_Med_PR_DPP +
                            i_perc_dpp_sem_col_lixo + i_perc_DPP_sem_ab_agua + i_perc_DPP_sem_ban_san + 
                            i_perc_DPP_sem_esg_fos + i_num_med_ban_hab, niveau = 0.05, 
                            data = bancodedados_ind)

stepwise
stepwise$results

write.xlsx(stepwise$results, "D:/Metodologia_R/Resultado_final_LDA/stepwise_results.xlsx")

#5. Geracao Matrizes

#Criar Data Frame com os Indicadores que passaram na Stepwise


stepwise_ind <- data.frame(i_num_dpp,
                           i_num_mor_dpp,
                           i_num_dom_imp,
                           i_perc_pr_ate_30,
                           i_perc_PR_nalf,
                           i_perc_PR_ate_30_nalf,
                           i_perc_pr_ren_ate_3_SM,
                           i_Renda_Med_PR_DPP,
                           i_perc_dpp_sem_col_lixo,
                           i_perc_DPP_sem_ab_agua,
                           i_perc_DPP_sem_esg_fos,
                           i_num_med_ban_hab)


View(stepwise_ind)
attach(stepwise_ind)

write.xlsx(stepwise_ind, "D:/Metodologia_R/Resultado_final_LDA/stepwise_ind.xlsx")

#5.1 Matriz de Correlacao total - Para avaliar multicolinearidade

corr_total <- rcorr(as.matrix(stepwise_ind), type = "pearson")

View(corr_total$r)

write.xlsx(corr_total$r, "D:/Metodologia_R/Resultado_final_LDA/Matrizes/corr_total.xlsx")


#5.2 Geracao das demais Matrizes de Correlacao

statistics_corr <- statsBy(stepwise_ind, Tipo_setor, cors = TRUE, cor="cor", method="pearson", use="pairwise", 
                           poly=FALSE, na.rm=TRUE, alpha=.05)

print(statistics_corr, short=FALSE)


#5.2.1 Matriz de correlacao Pooled Within Groups

corr_within_groups <- statistics_corr$rwg

View(statistics_corr$rwg)

write.xlsx(statistics_corr$rwg, "D:/Metodologia_R/Resultado_final_LDA/Matrizes/corr_within_groups.xlsx")


#5.2.2 Matriz de correlacao Pooled Within Groups Weighted (Ponderada pelo tamanho amostral)

View(statistics_corr$pooled)

write.xlsx(statistics_corr$pooled, "D:/Metodologia_R/Resultado_final_LDA/Matrizes/corr_within_groups_weighted.xlsx")


#5.2.3 Matriz de correlacao Separate groups (Dentro de cada grupo)
View(statistics_corr$r)

View(statistics_corr$r[["0"]])
View(statistics_corr$r[["1"]])

write.xlsx(statistics_corr$r[["0"]], "D:/Metodologia_R/Resultado_final_LDA/Matrizes/corr_grupos_separados_0.xlsx")
write.xlsx(statistics_corr$r[["1"]], "D:/Metodologia_R/Resultado_final_LDA/Matrizes/corr_grupos_separados_1.xlsx")

#5.3 Geracao de matrizes de Covariancia (within groups, separate groups, ...)

statistics_cov <- statsBy(stepwise_ind, Tipo_setor, cors = FALSE, cor="cov", method="pearson", use="pairwise", 
                                             poly=FALSE, na.rm=TRUE, alpha=.05)

View(statistics_cov$within)

#5.3.1 Matriz de Covariancia Within Groups

cov_within_groups <- statistics_cov$rwg

View(statistics_cov$rwg)

write.xlsx(statistics_cov$rwg, "D:/Metodologia_R/Resultado_final_LDA/Matrizes/cov_within_groups.xlsx")

#Diferenca entre corr_within_groups e corr_total
#A primeira usa a media das covariancias do grupo a qual a observacao pertence
#e a segunda e uma correlacao binaria de variavel a variavel usando a covariancia em relacao a MEDIA TOTAL
##Usar o "desda" do DiscriMiner e o site da IBM para explicar o que e o within groups 

##Identificou-se multicolinearidade entre i_num_dpp e i_num_mor_dpp E i_perc_PR_renda_ate_3SM e i_Renda_media_dpp
#Assim, escolheremos um dos indicadores de cada par e criaremos a lda em seguida.
#Foi escolhido o i_num_mor_dpp e o i_perc_PR_renda_ate_3SM, pois possuem a maior estatistica F de cada par:

#ANOVA e Teste F

ind <- as.matrix(stepwise_ind)
tipodesetor <- as.factor(bancodedados_ind$Tipo_setor)

ind_anova <- aov(ind ~ tipodesetor)
ind_manova <- manova(ind ~ tipodesetor)

estF <- summary.aov(ind_manova, test = "Wilks")

estF_merge <- data.frame(estF$` Response i_num_dpp`, estF$` Response i_num_mor_dpp`,
                         estF$` Response i_num_dom_imp`, estF$` Response i_perc_pr_ate_30`,
                         estF$` Response i_perc_PR_nalf`, estF$` Response i_perc_PR_ate_30_nalf`,
                         estF$` Response i_perc_pr_ren_ate_3_SM`, estF$` Response i_Renda_Med_PR_DPP`,
                         estF$` Response i_perc_dpp_sem_col_lixo`, estF$` Response i_perc_DPP_sem_ab_agua`,
                         estF$` Response i_perc_DPP_sem_esg_fos`, estF$` Response i_num_med_ban_hab`)

write.xlsx(estF_merge, "D:/Metodologia_R/Resultado_final_LDA/estatisticaF_variaveis_stepwise.xlsx")

#Data Frame dos indicadores finais da ADL

lda_ind <- data.frame(i_num_mor_dpp,
                      i_num_dom_imp,
                      i_perc_pr_ate_30,
                      i_perc_PR_nalf,
                      i_perc_PR_ate_30_nalf,
                      i_perc_pr_ren_ate_3_SM,
                      i_perc_dpp_sem_col_lixo,
                      i_perc_DPP_sem_ab_agua,
                      i_perc_DPP_sem_esg_fos,
                      i_num_med_ban_hab)


lda_ind_ord <- data.frame(i_perc_DPP_sem_esg_fos, 
                          i_perc_dpp_sem_col_lixo,
                          i_perc_DPP_sem_ab_agua,
                          i_num_dom_imp,
                          i_perc_pr_ate_30,
                          i_num_mor_dpp,
                          i_perc_PR_nalf,
                          i_perc_pr_ren_ate_3_SM,
                          i_num_med_ban_hab,
                          i_perc_PR_ate_30_nalf)

#6. Testar a normalidade das variáveis:


tipo_setor0 <- bancodedados_ind[bancodedados_ind$Tipo_setor==0,]
tipo_setor1 <- bancodedados_ind[bancodedados_ind$Tipo_setor==1,]

shapiro_test_0 <- shapiro.test(t(tipo_setor0$i_num_mor_dpp +
                                 tipo_setor0$i_num_dom_imp +
                                 tipo_setor0$i_perc_pr_ate_30 +
                                 tipo_setor0$i_perc_PR_nalf +  
                                 tipo_setor0$i_perc_PR_ate_30_nalf +
                                 tipo_setor0$i_perc_pr_ren_ate_3_SM +
                                 tipo_setor0$i_perc_dpp_sem_col_lixo +
                                 tipo_setor0$i_perc_DPP_sem_ab_agua +
                                 tipo_setor0$i_perc_DPP_sem_esg_fos +
                                 tipo_setor0$i_num_med_ban_hab))

shapiro_test_1 <- shapiro.test(t(tipo_setor1$i_num_mor_dpp +
                                 tipo_setor1$i_num_dom_imp +
                                 tipo_setor1$i_perc_pr_ate_30 +
                                 tipo_setor1$i_perc_PR_nalf +  
                                 tipo_setor1$i_perc_PR_ate_30_nalf +
                                 tipo_setor1$i_perc_pr_ren_ate_3_SM +
                                 tipo_setor1$i_perc_dpp_sem_col_lixo +
                                 tipo_setor1$i_perc_DPP_sem_ab_agua +
                                 tipo_setor1$i_perc_DPP_sem_esg_fos +
                                 tipo_setor1$i_num_med_ban_hab))


shapiro_test_0
shapiro_test_1


#7. Teste BOX's M de Normalidade Multivariada e de Homogeneidade das Matrizes de Covariancia

box_m <- boxM(data.frame(i_num_mor_dpp,
                         i_num_dom_imp,
                         i_perc_pr_ate_30,
                         i_perc_PR_nalf,
                         i_perc_PR_ate_30_nalf,
                         i_perc_pr_ren_ate_3_SM,
                         i_perc_dpp_sem_col_lixo,
                         i_perc_DPP_sem_ab_agua,
                         i_perc_DPP_sem_esg_fos,
                         i_num_med_ban_hab), group = Tipo_setor)


#Pooled within groups matrix by Box's M Test
View(box_m$p.value)

#Results
View(box_m$statistic) 

box_m
   
#Rejeitou-se a hipotese nula de normalidade e homogeneidade das covariancias (dispersao) entre os grupos.
#Possivel causa: as observacoes apresentam inflacao de zeros, fazendo com que boa parte das medianas sejam ZERO.
#Porem, pode-se assumir normalidade assintotica pelo Teorema do Limite Central, pelo tamanho da amostra.

#ASSUMIU-SE NORMALIDADE ASSINTOTICA PELO TEOREMA DO LIMITE CENTRAL#.

#8. Construcao da Funcao Discriminante 

#Indicadores excluidos da analise: i_num_dpp e Renda_media.

library(MASS)

lda_function <- lda(Tipo_setor ~ i_num_mor_dpp +
                                 i_num_dom_imp +
                                 i_perc_pr_ate_30 +
                                 i_perc_PR_nalf +  
                                 i_perc_PR_ate_30_nalf +
                                 i_perc_pr_ren_ate_3_SM +
                                 i_perc_dpp_sem_col_lixo +
                                 i_perc_DPP_sem_ab_agua +
                                 i_perc_DPP_sem_esg_fos +
                                 i_num_med_ban_hab, data= bancodedados_ind,
                                 tol = 1.0e-4, method = "moment",
                                 CV = FALSE)
lda_function


lda_function_ord <- lda(Tipo_setor ~ i_perc_DPP_sem_esg_fos + 
                        i_perc_dpp_sem_col_lixo +
                        i_perc_DPP_sem_ab_agua +
                        i_num_dom_imp +
                        i_perc_pr_ate_30 +
                        i_num_mor_dpp +
                        i_perc_PR_nalf +
                        i_perc_pr_ren_ate_3_SM +
                        i_num_med_ban_hab +
                        i_perc_PR_ate_30_nalf, data = bancodedados_ind,
                    tol = 1.0e-4, method = "moment",
                    CV = FALSE)

lda_function_ord

#Salvar coeficientes
write.xlsx(lda_function$scaling, "D:/Metodologia_R/Resultado_final_LDA/coef_lda_function.xlsx")

write.xlsx(lda_function_ord$scaling, "D:/Metodologia_R/Resultado_final_LDA/coef_lda_function_ord.xlsx")

#Scores Discriminantes

lda_score <- predict(lda_function, dimen=1)$x
View(lda_score)

write.xlsx(lda_score, "D:/Metodologia_R/Resultado_final_LDA/lda_scores.xlsx")

#8.1. Calculo da constante da funcao discriminante da lda

#Coef*Medias

ax0 <- sum(lda_function$scaling*lda_function$means[1,])
ax0

ax1 <- sum(lda_function$scaling*lda_function$means[2,])
ax1

#8.3 Calculo do Ponto de Corte (limiar)
#ax0 + ax1/2 (Media Nao Ponderada dos Scores Medios)

limiar <- (ax0 + ax1) / 2
limiar

#Constante: media ponderada de ax1 e ax0 

constante <- (ax0*3894+ax1*175)/4069

#8.2 Calculo do Score medio

score_medio_0 <- ax0 - constante
score_medio_1 <- ax1 - constante

#O Score Medio entre os grupos (Ponderado) deve dar zero:

((score_medio_0* 3894) + (score_medio_1) * 175)/4069

#A media nao ponderada dos scores dos grupos representa o limiar.
#Limiar e o ponto de corte que define o que sera classificado como AS e como NE.
#A constante foi calculada no excel, por meio do lda_scores menos os coeficientes da funcao multiplicados
#pelas variaveis de um determinado setor. O valor resultante e igual a constante, sendo a mesma para todas
#as 4069 observacoes.

#8.3. Centroides da Funcao

centroides <- lda_function$means

centroides_ord <- lda_function_ord$means

write.xlsx(centroides, "D:/Metodologia_R/Resultado_final_LDA/centroides.xlsx")

write.xlsx(centroides_ord, "D:/Metodologia_R/Resultado_final_LDA/centroides_ord.xlsx")

#9. Matriz de Estrutura

#Correlacao entre cada indicador e seus respectivos scores da LDA

matriz_lda_scores <- (cbind(i_num_mor_dpp,
                     i_num_dom_imp,
                     i_perc_pr_ate_30,
                     i_perc_PR_nalf,
                     i_perc_PR_ate_30_nalf,
                     i_perc_pr_ren_ate_3_SM,
                     i_perc_dpp_sem_col_lixo,
                     i_perc_DPP_sem_ab_agua,
                     i_perc_DPP_sem_esg_fos,
                     i_num_med_ban_hab,
                     lda_score))

matriz_lda_scores_ord <- (cbind(i_perc_DPP_sem_esg_fos, 
                                 i_perc_dpp_sem_col_lixo,
                                 i_perc_DPP_sem_ab_agua,
                                 i_num_dom_imp,
                                 i_perc_pr_ate_30,
                                 i_num_mor_dpp,
                                 i_perc_PR_nalf,
                                 i_perc_pr_ren_ate_3_SM,
                                 i_num_med_ban_hab,
                                 i_perc_PR_ate_30_nalf, lda_score))

corr_total_lda_scores <- statsBy(matriz_lda_scores, Tipo_setor, 
                                 cors = FALSE, cor="cor", 
                                 method="pearson", use="pairwise", 
                                 poly=FALSE, na.rm=TRUE, alpha=.05)

corr_total_lda_scores_ord <- statsBy(matriz_lda_scores_ord, Tipo_setor, 
                                 cors = FALSE, cor="cor", 
                                 method="pearson", use="pairwise", 
                                 poly=FALSE, na.rm=TRUE, alpha=.05)

#A Matriz de estrutura sera a ultima coluna do corr_LD_i$rwg:

matriz_de_estrutura <- data.frame(corr_total_lda_scores$rwg)

matriz_de_estrutura_ord <- data.frame(corr_total_lda_scores_ord$rwg)

View(matriz_de_estrutura)

#Salvar

write.xlsx(matriz_de_estrutura[,11], "D:/Metodologia_R/Resultado_final_LDA/matrizdeestrutura.xlsx")

write.xlsx(matriz_de_estrutura_ord[,11], "D:/Metodologia_R/Resultado_final_LDA/Matrizes/matrizdeestrutura_ord.xlsx")

#10. Classificacao

attach(bancodedados_ind)

#10.1. Probabilidades a Priori

View(lda_function$prior)

#10.2. Predicoes do Modelo ADL

pred_modelo_adl <- predict(object = lda_function, newdata = bancodedados_ind, prior=lda_function$prior, type = "response")

View(pred_modelo_adl$class)

write.xlsx(pred_modelo_adl$x, "D:/Metodologia_R/Resultado_final_LDA/Predicoes/scores_lda.xlsx")

#10.3. Classificacoes do Modelo ADL para cada setor:

View(data.frame(as.factor(pred_modelo_adl$class), pred_modelo_adl$x, pred_modelo_adl$posterior))

View(pred_modelo_adl$x)

bancodedados_ind_predict_adl <- data.frame(Cod_Setor, Situacao_setor, pred_modelo_adl$class, pred_modelo_adl$x,
                                       i_num_dpp, i_num_mor_dpp, i_num_med_mor_por_dpp,
                                       i_perc_pes_res_brancas, i_perc_PR_sexo_fem, i_num_dom_imp, 
                                       i_perc_dpp_outra_ocup, i_perc_pr_ate_30, i_perc_PR_nalf, 
                                       i_perc_PR_ate_30_nalf, i_perc_pr_ren_ate_3_SM, i_Renda_Med_PR_DPP,
                                       i_perc_dpp_sem_col_lixo, i_perc_DPP_sem_ab_agua, i_perc_DPP_sem_ban_san, 
                                       i_perc_DPP_sem_esg_fos, i_num_med_ban_hab)

write.xlsx(bancodedados_ind_predict, "D:/Metodologia_R/Resultado_final_LDA/Predicoes/bancodedados_ind_predict.xlsx")

#11. Matriz de Confusao

matrizdeconfusao_lda <- confusionMatrix(table(pred_modelo_adl$class, bancodedados_ind$Tipo_setor))

matrizdeconfusao_lda$table

matrizdeconfusao_lda

write.xlsx(matrizdeconfusao_lda$table, "D:/Metodologia_R/Resultado_final_LDA/Matrizes/matrizdeconfusao.xlsx")

#12. Avaliacao do modelo LDA

#12.1 Curva ROC

View(pred_modelo_adl$x)

roc_lda <- plot.roc(Tipo_setor, pred_modelo_adl$x)

plot(roc_lda,
     print.auc=TRUE, 
     auc.polygon=TRUE, 
     grud=c(0.1,0.2),
     grid.col=c("#c00000","darkgoldenrod2"), 
     max.auc.polygon=TRUE, 
     auc.polygon.col="#c00000", 
     print.thres=TRUE)

#12.2 Acerto Global do Modelo ADL

BD_orig_adl <- cbind(bancodedados_ind, pred_modelo_adl)

BD_orig_adl %>%
  count (Tipo_setor, class)

BD_orig_adl %>% 
  summarize(score = mean(class == Tipo_setor))

#0.9437208

################## MEXER A PARTIR DAQUI, MARCIO ###############

#13. VISUALIZACAO DOS RESULTADOS

library(lattice)
library(ggplot2)

#13.1 Grafico da LDA

plot(lda_function, asp=1, col = "brown4")

#13.1 Graficos de densidade 

#Com as classes originais
#o comando lwd controla a espessura da linha do gráfico.

densityplot(~pred_modelo_adl$x, groups= Tipo_setor, bw=1,
            xlab="Score Discriminante",ylab="Densidade",
            lwd=5,
            col=c("brown4","gold"),
            group = bancodedados_ind$Tipo_setor,
            plot.points = "jitter",main = "Densidade de observações por Tipo de Setor",
            auto.key = list(points = T, lines = F, space = 'right', col=c("brown4","gold")))

#Com as classes preditas
densityplot(~pred_modelo_adl$x, groups= bancodedados_ind_predict_adl$pred_modelo_adl.class,
            bw=1,xlab="Score Discriminante",ylab="Densidade",
            lwd=5,
            col=c("brown4","gold"),
            group = bancodedados_ind_predict_adl$pred_modelo_adl.class,
            plot.points = "jitter", main = "Densidade de observações por Tipo de Setor - Preditos",
            auto.key = list(points = T, lines = F, space = 'right', col=c("brown4","gold")))


#13.2 Histogramas

#Para fazer o histograma dos scores discriminantes vs. Tipo de setor,
#precisa criar um data frame unindo esses dados:

#Criar banco de dados original adicionando uma coluna com os scores da adl

bancodedados_ind_sc <- data.frame(bancodedados_ind, pred_modelo_adl$x)

fix(bancodedados_ind_sc)

write.xlsx(bancodedados_ind_sc, "D:/Metodologia_R/Resultado_final_LDA/Predicoes/bancodedados_ind_sc.xlsx")

#13.2.1. Histograma - Antes da predicao - dados originais

View(bancodedados_ind_sc)
Setor <- data.frame(ifelse(bancodedados_ind_sc$Tipo_setor==1,"SBN","NE"))
bancodedados_ind_sc["Setor"] <- Setor

pl_pred1 <- ggplot(bancodedados_ind_sc, aes(x = LD1, color = as.factor(Setor)), breaks = (0.5))

pl_pred <- pl_pred1 + geom_histogram(data = subset(bancodedados_ind_sc,
                                    bancodedados_ind_sc$Tipo_setor == '0'),
                                    binwidth=0.5, alpha = 0.5,
                                    position = 'dodge', fill="brown4") +
                     geom_histogram(data = subset(bancodedados_ind_sc,
                                    bancodedados_ind_sc$Tipo_setor == '1'),
                                    binwidth=0.5, alpha = 0.5,
                                    position = 'dodge', fill="darkgoldenrod1") +
                    labs(title= "Função Discriminante - Classes Originais",
                    x = "Escore Discriminante", y = "Num. de observações",
                    col = "Tipo de Setor")

pl_pred


#13.2.2. Histograma - Depois da predicao - dados preditos

View(bancodedados_ind_predict_adl)
Setor_p <- data.frame(ifelse(bancodedados_ind_predict_adl$pred_modelo_adl.class==1,"SBN","NE"))
bancodedados_ind_predict_adl["Setor_p"] <- Setor_p

pl_pred2 <- ggplot(bancodedados_ind_predict_adl, aes(x = LD1, col = as.factor(Setor_p)), breaks = (0.5))

pl_pred_adl <- pl_pred2 + geom_histogram(data = subset(bancodedados_ind_predict_adl,
                                                      bancodedados_ind_predict_adl$pred_modelo_adl.class == '0'),
                                        binwidth=0.5, alpha = 0.5,
                                        position = 'identity', fill="brown4") +
                         geom_histogram(data = subset(bancodedados_ind_predict_adl,
                                                      bancodedados_ind_predict_adl$pred_modelo_adl.class == '1'),
                                        binwidth=0.5, alpha = 0.5,
                                        position = 'identity',fill="darkgoldenrod1") +
                         labs(title= "Função Discriminante - Classes Preditas pelo Modelo",
                          x = "Escore Discriminante", y = "Num. de observações",
                          col = "Tipo de Setor")

pl_pred_adl


##Marcio!!!
#PORQUE O SEGUNDO GRAFICO (pl_pred_adl) FICA COM OS SETORES 0 OU 1 E O PRIMEIRO FICA
#COMO SE FOSSE CONTINUO (0, 0.25, 0.5, 1)? Queria os dois igual ao segundo!!
#É que no primeiro o ggplot não identificou Tipo_setor como um fator. Coloquei isso.

##Tentei usar o fix e nao consegui.
##Mudar tambem as cores.

#13.3 Graficos Variavel x Variavel

#Com o banco de dados original 

plot(bancodedados_ind[,19], bancodedados_ind[,15], las = 1,
     col = ifelse(bancodedados_ind$Tipo_setor==0,"brown4","gold"),
     pch = 20, xlab="% DPP sem esgoto ou fossa sept",ylab="Renda Media")
legend(x=85, y = 104, c("NE","SbN"), pch = 20, col=c("brown4","gold"), bty ="n")


#Com o banco de dados predito

plot(bancodedados_ind_predict_adl[,20], bancodedados_ind_predict_adl[,16], las = 1,
     col = ifelse(bancodedados_ind_predict_adl$pred_modelo_adl.class==0,"brown4","gold"),
     pch = 20, xlab="% DPP sem esgoto ou fossa sept",ylab="Renda Media")
legend(x=85, y = 104, c("NE","SbN"), pch = 20, col=c("brown","gold"), bty ="n")

#Quero replicar o grafico acima no ggplot:
#Nao consegui!

ggplot(bancodedados_ind_predict_adl, aes(x=i_perc_DPP_sem_esg_fos, y= i_Renda_Med_PR_DPP,col=pred_modelo_adl.class))+
  geom_point(aes(x=i_perc_DPP_sem_esg_fos)) +
  scale_color_manual(values = c('brown4','darkgoldenrod1')) +
  theme(legend.position="none") +
  geom_smooth(method = "lda", se = T)+
  labs(x="DPP sem Esgoto",
       y="Renda Media",
       title="DPP sem Esgoto",
       caption="Fonte: CENSO IBGE 2010")


#13.4 Grafico das Predicoes da Funcao Discriminante
plot(lda_function)

#Para melhorar, vou usar esse, com normalidade:

graf_adl <- plot(lda_function, dimen = 1, type = "b", col="brown4")

#Queria alterar a cor dele.
