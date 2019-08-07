if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, glmulti, mfx, ROCR, broom, purrr, margins, stringr, sjPlot, rmarkdown)


MatConf <- function(m, d, y){
  
  
  #El tomar 0,5 como límite para determinar la probabilidad de una valoración alta es una decisión estándar pero
  #arbitraria. Existen maneras de determinar si esta tasa es la adecuada empleando varios criterios, a continuación
  #mostramos como varía la precisión del modelo con distintos limites.
  
  variableTarget <- d[, y]
  
  prediccion <- ROCR::prediction(fitted(m), variableTarget) 
  precision <- performance(prediccion,'acc')
  ac.val = max(unlist(precision@y.values))
  th0 = unlist(precision@x.values)[unlist(precision@y.values) == ac.val]
  
  th = th0[1]
  
  #R2
  
  pseudoR2 <- 1 - m$deviance/m$null.deviance
  
  #matriz.confusion
  
  matriz.confusion <-  table(prediccion = predict(m, newdata = d, type='response')>th,
                             observado =variableTarget)
  
  #Calculamos la precisión, especificidad y sensibilidad 
  
  A1 = matriz.confusion[1,1] / sum(matriz.confusion[,1])
  A2 = matriz.confusion[2,2] / sum(matriz.confusion[,2])
  A3 <- sum(diag(matriz.confusion)) / sum(matriz.confusion)
  
  
  #AUC
  auc.c2 = performance(prediccion, "auc")
  auc.c2 = unlist(auc.c2@y.values)
  
  list(matriz = matriz.confusion, R2 = pseudoR2, especificidad = A1,
       sensibilidad = A2, precisión = A3, AUC = auc.c2)
  
}




selecModelo <-  function(d, y, x, C=TRUE){
  
  #Se estiman los modelos
  
  set.seed(123)
  
  glmulti.logistic.out <-
    glmulti(y,
            x,
            data = d,
            intercept = C,           # Incluir constante
            level = 1,               # No interaction considered
            method = "h",            # Exhaustive approach
            crit = "aic",            # AIC as criteria
            confsetsize = 100,       # Keep n best models
            plotty = F, report = T,  # No plot or interim reports
            fitfunction = "glm",     # glm function
            family = binomial(link = 'logit'))       # binomial family for logistic regression
  
  #Graficos del glmulti
  jpeg(file = 'grafglmutl.jpeg',  width = 480, height = 550)
  par(mfrow=c(2,1))
  plot(glmulti.logistic.out, type = "s")
  plot(glmulti.logistic.out, type = "p")
  dev.off()
  
  #Se construyen las tablas
  
  
  #Se inicializan las tablas como listas vacias
  modelo1 <- list()
  modelo2 <- list()
  evaluacion <- list()
  
  #Se rellenan las tablas con los 3 mejores modelos
  
  for (i in 1:3) {
    
    set.seed(123)
    
    modeloBest <- paste("Target ~",
                        as.character(glmulti.logistic.out@formulas[[i]][3]))
    
    
    modeloLogit <-  glm(modeloBest, 
                        family = binomial(link = 'logit'),
                        data = d)
    
    #Se elimina el -1 en la formula del modelo para que lo pueda leer la funcion logitmfx
    ifelse(C == FALSE, modeloBest <-  str_replace_all(modeloBest, '.-1 .', ''), modeloBest)
    
    EfectMarg <- logitmfx(modeloBest
                          , data = encuesta.logit.03,
                          atmean = FALSE)
    
    EfectMarg2 <- margins(modeloLogit)
    
    MargGraf <- summary(EfectMarg2)
    
    #Gráfico efectos marginales
    
    jpeg(file = paste0('grafprob', i, '.jpeg'))
    print(plot_model(modeloLogit, vline.color = "blue", sort.est = TRUE, show.values = TRUE, value.offset = .3))
    dev.off()
    
    ggplot(data = MargGraf) +
      geom_point(aes(factor, AME)) +
      geom_errorbar(aes(x = factor, ymin = lower, ymax = upper)) +
      geom_hline(yintercept = 0) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45))
    ggsave(paste0('grafmarg', i, '.jpeg'), width=6, height=6)
    
    message(paste0("creando tabla ", i))
    
    #Tabla modelos logit
    
    modelo1[[i]] <- tidy(modeloLogit) %>% 
      dplyr::select(term, estimate, p.value) %>% 
      dplyr::rename(variable = term, coeficiente = estimate, pvalor = p.value) 
    
    #Tabla efectos marginales
    
    modelo2[[i]] <- EfectMarg$mfxest %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      dplyr::rename(marginal = `dF/dx`, pvalor = `P>|z|`, variable = rowname) %>%
      dplyr::select(variable, marginal, pvalor) 
    
    #Evaluacion
    
    evalTemp <- MatConf(modeloLogit, encuesta.logit.03, "Target")
    evaluacion[[i]] <-  evalTemp[c(2:6)] %>%
      unlist()
    
  }
  
  
  #Tabla de coeficientes
  modelo_Coef <-  modelo1 %>% 
    reduce(full_join, by = "variable")
  
  for (i in seq(3,7,2)) {
    modelo_Coef[,i] <- modelo_Coef[,i] %>%
      map(function(x) round(x,5)) %>%
      unlist()  
  }
  
  for (i in seq(2,6,2)) {
    colnames(modelo_Coef)[i] <- 'Coeficientes'
  }
  
  for (i in seq(3,7,2)) {
    colnames(modelo_Coef)[i] <- 'pValor'
  }
  
  
  #Tabla de efectos marginales
  modelo_Marg <-  modelo2 %>% 
    reduce(full_join, by = "variable")
  
  for (i in seq(3,7,2)) {
    modelo_Marg[,i] <- modelo_Marg[,i] %>%
      map(function(x) round(x,5)) %>%
      unlist()  
  }
  
  for (i in seq(2,6,2)) {
    colnames(modelo_Marg)[i] <- 'Coeficientes'
  }
  
  for (i in seq(3,7,2)) {
    colnames(modelo_Marg)[i] <- 'pValor'
  }
  
  #Tabla de Evaluacion
  Modelo_Eval <- reduce(evaluacion, rbind)
  rownames(Modelo_Eval) <- c('Modelo 1', 'Modelo 2', 'Modelo 3')
  
  
  render("cuadros.rmd", output_file = 'cuadros.pdf')
  
  list(coeficientes = modelo_Coef,
       marginales = modelo_Marg,
       evaluacionM = Modelo_Eval)
  
}
