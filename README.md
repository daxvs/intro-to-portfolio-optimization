# intro-to-portfolio-optimization

Aplicação do critério de média variância para obtenção do portfólio de variância mínima através do pacote `fPortfolio`. Obtemos um portfólio com rebalanceamento mensal que supera o retorno acumulado do CDI durante o período de 5 anos.

O arquivo `quarto_portfolio_financas.html` possui uma simples explicação sobre a teoria do critério de média variância, algumas referências de material sobre otimização de portfólios e o código em R utilizado para otimização e visualização dos resultados. O documento Quarto `quarto_portfolio_financas.qmd` apresenta o cógio utilizado para gerar o arquivo html. Em `scripts` e `src` temos o código em R completo e a função `optim()` criada para realizar a iteração mensal de rebalanceamento.
