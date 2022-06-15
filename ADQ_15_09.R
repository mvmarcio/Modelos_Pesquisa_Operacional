#ANALISE DISCRIMINANTE QUADRATICA#

library(MASS)
library(klaR)
library(caret)
library(ROCR)
library(pROC)
library(earth)
library(dplyr)

#9. Construcao da Funcao Discriminante Quadratica

qda_function_ord <-  qda(Tipo_setor ~
                           i_perc_DPP_sem_esg_fos + 
                           i_perc_dpp_sem_col_lixo +
                           i_perc_DPP_sem_ab_agua +
                           i_num_dom_imp +
                           i_perc_pr_ate_30 +
                           i_num_mor_dpp +
                           i_perc_PR_nalf +
                           i_perc_pr_ren_ate_3_SM +
                           i_num_med_ban_hab +
                           i_perc_PR_ate_30_nalf,
                         data= bancodedados_ind,
                         tol = 1.0e-4, method = "moment",
                         CV = FALSE)


qda_function <-  qda(Tipo_setor ~ i_num_mor_dpp +
                                  i_num_dom_imp +
                                  i_perc_pr_ate_30 +
                                  i_perc_PR_nalf +  
                                  i_perc_PR_ate_30_nalf +
                                  i_perc_pr_ren_ate_3_SM +
                                  i_perc_dpp_sem_col_lixo +
                                  i_perc_DPP_sem_ab_agua +
                                  i_perc_DPP_sem_esg_fos +
                                  i_num_med_ban_hab,
                     data= bancodedados_ind, tol = 1.0e-4, method = "moment",
                     CV = FALSE)

write.xlsx(qda_function$scaling, "D:/Metodologia_R/Resultado_final_QDA/coef_qda_function.xlsx")
write.xlsx(qda_function_ord$scaling, "D:/Metodologia_R/Resultado_final_QDA/coef_qda_function_ord.xlsx")

#10. Classificacao - ADQ

attach(bancodedados_ind)

#10.1 Probabilidades a Priori da ADQ

qda_function$prior

#10.2. Predicoes do modelo ADQ

pred_modelo_adq <- predict(object = qda_function, newdata = bancodedados_ind, prior=qda_function$prior, type = "response")

View(pred_modelo_adq$class)

View(pred_modelo_adq$posterior)

write.xlsx(pred_modelo_adq$class, "D:/Metodologia_R/Resultado_final_QDA/Predicoes/prob_posterior_qda.xlsx")

#10.3. Classificacoes do Modelo ADL para cada setor:

View(data.frame(as.factor(pred_modelo_adq$class), pred_modelo_adq$posterior))

bancodedados_ind_predict_adq <- data.frame(Cod_Setor, Situacao_setor, pred_modelo_adq$class, pred_modelo_adq$posterior,
                                           i_num_dpp, i_num_mor_dpp, i_num_med_mor_por_dpp,
                                           i_perc_pes_res_brancas, i_perc_PR_sexo_fem, i_num_dom_imp, 
                                           i_perc_dpp_outra_ocup, i_perc_pr_ate_30, i_perc_PR_nalf, 
                                           i_perc_PR_ate_30_nalf, i_perc_pr_ren_ate_3_SM, i_Renda_Med_PR_DPP,
                                           i_perc_dpp_sem_col_lixo, i_perc_DPP_sem_ab_agua, i_perc_DPP_sem_ban_san, 
                                           i_perc_DPP_sem_esg_fos, i_num_med_ban_hab)

write.xlsx(bancodedados_ind_predict_adq, "D:/Metodologia_R/Resultado_final_QDA/Predicoes/bancodedados_ind_predict_qda.xlsx")

#11. Matriz de Confusao

matrizdeconfusao_qda <- confusionMatrix(table(pred_modelo_adq$class, bancodedados_ind$Tipo_setor))

matrizdeconfusao_qda$table
matrizdeconfusao_qda

write.xlsx(matrizdeconfusao_qda$table, "D:/Metodologia_R/Resultado_final_QDA/Matrizes/matrizdeconfusao.xlsx")

######### MEXER A PARTIR DAQUI, MARCIO ################

#12. Avaliacao do Modelo QDA

#12.1 Curva ROC 
#Preciso trocar os nomes dos eixos para portugues

roc_qda <- plot.roc(Tipo_setor, pred_modelo_adq$posterior[,2], xlab = "Especificidade",
                    ylab="Sensitividade")

plot(roc_qda,
     print.auc=TRUE, 
     auc.polygon=TRUE, 
     grud=c(0.1,0.2),
     grid.col=c("brown4","gold"), 
     max.auc.polygon=TRUE, 
     auc.polygon.col="brown", 
     print.thres=TRUE, xlab = "Especificidade",
     ylab="Sensitividade")

#12.1 Percentual de Acerto Global

BD_orig_adq <- cbind(bancodedados_ind, pred_modelo_adq)

BD_orig_adq %>%
  count (Tipo_setor, class)

BD_orig_adq %>% 
  summarize(score = mean(class == Tipo_setor))

#0.9368395

#13. VISUALIZACAO DOS RESULTADOS

#13.1 Essa foi a melhor forma que encontrei de visualizar os resultados.

View(data.frame(pred_modelo_adq$class))
par(mfrow=c(0,1), mfcol=c(1,0))
plot(pred_modelo_adq$posterior[,2], pred_modelo_adq$class, col=as.factor(bancodedados_ind$Tipo_setor+10),
     xlab = "Probabilidades a posteriori", ylab = "Classes preditas", xlim=c(0,1), ylim=c(0.2,1),
     pch = 19)

#Outra alternativa que criei:
par(mfrow=c(0,1), mfcol=c(1,0))
plot(pred_modelo_adq$posterior[,2], as.factor(pred_modelo_adq$class), col=c("brown4","gold"),
     xlab = "Probabilidades a posteriori", ylab = "Classes preditas", xlim=c(0,1), ylim=c(0.2,1),
     pch = 19)



#Porem, gostaria de tentar plotar a funcao, talvez um parecido com o que tu plotou da RL !!
#Precisa ajustar o eixo y "classes preditas" para 0 e 1
#E precisa mudar as cores.


#13.2 Tentativa de fazer um grafico com o limite da decisao (decision boundary)
#Nao deu certo, mas ficou o grafico do Esgoto x Renda:

# Criar Grid
L <- 4069 #Tamanho da grid
X1 <- seq(min(bancodedados_ind[,14]) -1, max(bancodedados_ind[,14]) +1, length = L)
X2 <- seq(min(bancodedados_ind[,19]) -1, max(bancodedados_ind[,19]) +1, length = L) 
grid_set <- expand.grid(X1, X2)

# Adapt the variable names
colnames(grid_set) = c('% Renda ate 3SM', '% sem Esgoto')

# Plotar '% Renda ate 3SM' ~ '% sem Esgoto'
plot(bancodedados_ind[, c("i_perc_pr_ren_ate_3_SM", "i_perc_DPP_sem_esg_fos")],
     main = 'Resultados ADQ',
     xlab = '% Renda ate 3SM', ylab = '% sem Esgoto',
     col = c("brown4", "gold"),
     xlim = range(X1), ylim = range(X2))
legend(x=X1, y=X2,legend=c("% Renda ate 3SM", "% sem Esgoto"))

# Preencher os pontos
points(bancodedados_ind[, c("i_perc_pr_ren_ate_3_SM", "i_perc_DPP_sem_esg_fos")], pch = 21, bg = ifelse(bancodedados_ind[, 3] == 1, 'gold', 'red3'))

# Predicoes
adqclass <- pred_modelo_adq$class

# Separate the predictions by a contour
contour(X1, X2, matrix(as.numeric(adqclass), length(X1), length(X2)), add = TRUE, drawlabels=FALSE)

#Referencias:
#https://github.com/mmarouen/The-Elements-Of-Statistical-Learning/blob/master/ESL_CH_IV_Linear%20Classification/fig4_1%265%266_DecisionBoundaries.R
#https://www.mghassany.com/MLcourse/pw-4.html#lda-from-scratch

#13.3 Grafico Pacote Earth

attach(bancodedados_ind)

plot(qda_function, hist = TRUE, type = "class", labels = TRUE)

#13.3 Graficos KLAR
#Queria mudar as cores! (nao sei como fazer isso), esse grafico gerado pelo partimat eh muito ESCANDALOSO! kkkk

dev.new(width=10, height=5, noRStudioGD=TRUE)
partimat(as.factor(Tipo_setor) ~ i_perc_DPP_sem_esg_fos + 
           i_perc_DPP_sem_ab_agua +
           i_perc_dpp_sem_col_lixo +
           i_perc_pr_ren_ate_3_SM,
         data= bancodedados_ind, method="qda",col.correct='gold',col.wrong='brown4',
         col.mean=1, image.colors = c("white","white"))


partimat(as.factor(Tipo_setor) ~ i_perc_pr_ren_ate_3_SM + i_perc_DPP_sem_esg_fos,
         data= bancodedados_ind, method = "qda", plot.matrix = TRUE,
         col.correct='gold',image.colors = c("white","white"), col.wrong='brown4')

#__________________________________________________________________________________________________________#

#Banco de dados final com as classes preditas pela ADQ

bd_ind_final_qda <- data.frame(Cod_Setor, Situacao_setor, pred_modelo_adq$class, i_num_mor_dpp,
                               i_num_dom_imp, i_perc_pr_ate_30, i_perc_PR_nalf,
                               i_perc_PR_ate_30_nalf, i_perc_pr_ren_ate_3_SM,
                               i_perc_dpp_sem_col_lixo, i_perc_DPP_sem_ab_agua,
                               i_perc_DPP_sem_esg_fos, i_num_med_ban_hab)

