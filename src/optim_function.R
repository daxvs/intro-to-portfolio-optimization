optim <- function(data_referencia, dados_completo, restricao = FALSE) {
  data_referencia <- as.Date(data_referencia)
  
  # 1. Definir as datas da janela de treino (2 anos anteriores)
  fim_treino <- data_referencia - days(1)
  inicio_treino <- data_referencia - years(2)
  
  # 2. Filtrar os dados e converter para timeSeries (exigência do fPortfolio)
  dados_treino <- dados_completo |> 
    filter(between(date, left = inicio_treino, right = fim_treino)) |> # inicio_treino <= date <= fim_treino
    select(-date) |> 
    as.timeSeries()
  
  # 3. Executar otimização
  if(restricao) {
    constraint.10pct <- c(
      paste0("minW[1:", ncol(dados_treino), "]=0.00"),
      paste0("maxW[1:", ncol(dados_treino), "]=0.10"),
      paste0("sumW[1:", ncol(dados_treino), "]=1")
    )
    spec <- fPortfolio::portfolioSpec()
    opt <- tryCatch(
      expr = { # é a expressão (expr) que você quer que o R tente executar.
        fPortfolio::minvariancePortfolio(
          data = dados_treino,
          spec = spec,
          constraints = constraint.10pct
        )
      },
      error = function(e) {return(NULL)} # Este é o argumento que define o que fazer se o bloco principal falhar.
      # e: representa o objeto de erro. O R passa automaticamente a mensagem de erro técnica para essa variável e.
    )
  } else {
    spec <- fPortfolio::portfolioSpec()
    opt <- tryCatch(
      expr = {
        fPortfolio::minvariancePortfolio(
          data = dados_treino,
          spec = spec,
          constraints = "LongOnly"
        )
      },
      error = function(e) {return(NULL)}
    )
  }
  
  # caso haja algum erro durante a otimização a função 'optim' será encerrada, retornando NULL
  if(is.null(opt)) return(NULL) 
  
  # 4. Não havendo erro durante o processo de otimização, nossa função 'optim' irá retoranr um tibble com a data de referência em cada linha e os respectivos pesos em cada coluna
  pesos <- getWeights(opt)
  bind_cols(
    tibble(
      date_ref = data_referencia
    ),
    as_tibble(t(pesos))
  )
  
}