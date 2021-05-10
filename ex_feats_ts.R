rm(list = ls())

setwd("~/Documentos/MESTRADO - PO/APRENDIZADO DE MÁQUINA")

library(magrittr)
library(BatchGetSymbols)
library(tsfeatures)

# funções auxiliares ----
movMeas <- function(x,f,n,...){
  N <- length(x)
  
  out <- rep(NA,n-1)
  for (i in n:N) {
    v <- matrix(x[(i-n+1):i],nrow = 1)
    out <- c(out,
             apply(v,1,f,...))
  }
  out
}

attrNA <- function(x){
  for (i in 2:length(x)) {
    if (is.na(x[i])) {
      x[i] <- x[i-1]
    }
  }
  return(x)
}

genMatPrice <- function(prices_list){
  num_tick <- length(prices_list)
  tickers <- names(prices_list)
  
  # Sincronização ----
  datas <- prices_list[[1]][,1] %>% 
    as.character
  for (i in 2:num_tick) {
    datas <- c(datas,
               prices_list[[i]][,1] %>% 
                 as.character) %>% 
      unique
  }
  datas <- datas %>% 
    as.Date %>% 
    sort %>% 
    as.character
  
  # Merging ----
  mat_prices_list <- NULL
  for (i in 1:num_tick) {
    mat_prices_list <- cbind(mat_prices_list,
                             ifelse(datas %in% (prices_list[[i]][,1] %>% 
                                                  as.character),
                                    prices_list[[i]][datas,2],
                                    NA))
  }
  
  rownames(mat_prices_list) <- datas
  colnames(mat_prices_list) <- tickers
  
  # Atribuição de valores faltantes ----
  mat_prices_list <- mat_prices_list %>% apply(2,attrNA)
  mat_prices_list <- mat_prices_list %>% na.omit
  
  datas <- rownames(mat_prices_list)
  
  mat_prices_list
}

genMatRend <- function(mat_prices_list,lag_DC){
  datas <- rownames(mat_prices_list)
  
  # Matriz de Rendimentos ----
  fcs <- datas %>% 
    as.Date %>% 
    diff %>% 
    mean %>% 
    as.numeric
  
  n <- round(lag_DC/fcs)
  
  rend <- mat_prices_list %>%
    ROC(n=n)
  
  rend
}

# setup ----
lag_DC <- 180

# exemplo de geração features ----

tickers <- c("b3sa3.sa","bova11.sa","petr4.sa","vale3.sa")
batch <- BatchGetSymbols(tickers,
                         first.date = "2006-01-01",
                         last.date = Sys.Date()-1,
                         thresh.bad.data = 0.5,
                         be.quiet = T)

precos <- batch$df.tickers
rm(batch)

feats <- list()
for (i in 1:length(tickers)) {
  df <- subset(precos,ticker==tickers[i])
  y <- df$price.close
  N <- length(y)
  fcs <- df$ref.date %>% 
    diff %>% as.numeric %>% mean
  
  lag <- round(lag_DC/fcs)
  
  int <- seq(1,N,lag)
  
  lista_precos <- list()
  for (j in 1:(length(int)-1)) {
    lista_precos[[j]] <- y[int[j]:(int[j+1]-1)]
  }
  
  feats[[i]] <- tsfeatures(lista_precos)
  corrplot::corrplot(feats[[i]][,-c(1:3)] %>% cor(method = "s"))
}