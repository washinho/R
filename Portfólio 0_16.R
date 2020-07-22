#https://campus.datacamp.com/courses/intermediate-portfolio-analysis-in-r/application?ex=10
#Compute benchmark returns
#In this exercise, we will create a benchmark to evaluate the performance of the optimization models in later exercises. An equal weight benchmark is a simple weighting scheme to construct the benchmark portfolio. The intuition of an equal weight approach is that there is no preference for any given asset. We are setting this up to answer the question, "Can optimization outperform a simple weighting scheme to construct a portfolio?"


# Load the package
library(PortfolioAnalytics)
library(readxl)
library(dplyr)
library(tidyquant)
library(tidyverse)

#Lê a planilha com os ativos que está nos meus documentos
ativos <- read_excel("C:/Users/servo/OneDrive/Documentos/Ibov_wash.xlsx", sheet=1, col_names = TRUE)

#Coloca ".SA" após os tickers para poderem ser lidos no Yahoo Finance
ativos$codigo <- paste(ativos$`Ativos`,".SA",sep="")

#Transforma os tickers numa lista
lista <- ativos$codigo

#automatiza a busca das datas
data_sistema <- Sys.Date()
ontem_menos3anos <- as.POSIXlt(as.Date(data_sistema - 1))
ontem_menos3anos$year <- ontem_menos3anos$year - 3
ontem <- data_sistema - 1

#Busca o retorno das ações
log_Retornos_diarios <-   lista %>%
  tq_get(get  = "stock.prices",
         from = "2015-01-02",
         to   = ontem) %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly",
               type = "log",
               col_rename = "Ra")

#Ativos em colunas únicas
dia <- c(log_Retornos_diarios$date)
retorno <- c(log_Retornos_diarios$Ra)
ativo <- c(log_Retornos_diarios$symbol)
df1 <- data.frame(dia, retorno, ativo)
df2 <- xtabs(retorno ~ dia + ativo, data = df1)
#Para remover o primeiro valor zerado
df2 <- df2[(-1),]

#Traz todos os ativos para a mesma df
df3 <- df2 %>%
  as.data.frame() %>%
  mutate(dia = as.Date(dia),
         dia = as.yearmon(dia)) %>%
  group_by(dia, ativo) %>%
  summarise(Freq = sum(Freq)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = dia,
    names_from = ativo,
    values_from = Freq
  )

#Transformar a df3 em xts pra poder fazer o abaixo
df3_xts <- xts(df3, order.by = as.Date(df3$dia), "%b %Y")
storage.mode(df3_xts) <- "numeric"
df3_xts$dia <- NULL

# Create a vector of equal weights
equal_weights <- rep(1 / ncol(df3_xts), ncol(df3_xts))

# Compute the benchmark returns
r_benchmark <- Return.portfolio(R = df3_xts, weights = equal_weights, rebalance_on = "quarters")
colnames(r_benchmark) <- "benchmark"

# Plot the benchmark returns
plot(r_benchmark)


#Define the portfolio optimization problem
#We define the portfolio optimization problem to minimize portfolio standard deviation subject to full investment and long only constraints. In this problem, we will set up the portfolio specification based on the defined problem. The following exercises in this chapter will build on the initial portfolio specification set up here.

# Create the portfolio specification
port_spec <- portfolio.spec(colnames(df3_xts))

# Add a full investment constraint such that the weights sum to 1
port_spec <- add.constraint(portfolio = port_spec, type = "box", min_sum=0.01, max_sum=0.6)

# Add a long only constraint such that the weight of an asset is between 0 and 1
port_spec <- add.constraint(portfolio = port_spec, type = "long_only")

# Add an objective to minimize portfolio standard deviation
port_spec <- add.objective(portfolio = port_spec, type = "risk" , name = "StdDev")

# Print the portfolio specification
print(port_spec)


#Backtest with periodic rebalancing
#Now we will run the backtest using the portfolio specification created in the last exercise with quarterly rebalancing to evaluate out-of-sample performance. The other backtest parameters we need to set are the training period and rolling window. The training period sets the number of data points to use for the initial optimization. The rolling window sets the number of periods to use in the window. This problem can be solved with a quadratic programming solver so we will use "ROI" for the optimization method.


# Run the optimization
opt_rebal_base <- optimize.portfolio.rebalancing(R = df3_xts, 
                                                 portfolio = port_spec, 
                                                 optimize_method = "ROI", 
                                                 rebalance_on = "quarters", 
                                                 training_period = 60,
                                                 rolling_window = 60)

# Print the results
print(opt_rebal_base)

# Chart the weights
chart.Weights(opt_rebal_base)

# Compute the portfolio returns
returns_base <- Return.portfolio(R = df3_xts, weights = extractWeights(opt_rebal_base))
colnames(returns_base) <- "base"


# Add a risk budge objective
port_spec <- add.objective(portfolio = port_spec, 
                           type = "risk_budget", 
                           name = "StdDev", 
                           min_prisk = 0.05, 
                           max_prisk = 0.1)

# Run the optimization
opt_rebal_rb <- optimize.portfolio.rebalancing(R = df3_xts, 
                                               portfolio = port_spec, 
                                               optimize_method = "random",
                                               trace = TRUE,
                                               rebalance_on = "quarters", 
                                               training_period = 60,
                                               rolling_window = 60)

# Chart the weights
chart.Weights(opt_rebal_rb)

# Chart the percentage contribution to risk
chart.RiskBudget(opt_rebal_rb, match.col = "StdDev", risk.type = "percentage")

# Compute the portfolio returns
returns_rb <- Return.portfolio(R = df3_xts, weights = extractWeights(opt_rebal_rb))
colnames(returns_rb) <- "risk_budget"


#Do improved estimates lead to improved performance?
#Let us hypothesize that using a robust estimate of the variance-covariance matrix will outperform the sample variance covariance matrix. In theory, better estimates should lead to better results. We will use the moments_robust() function that was defined in chapter 3 and the portfolio specification from the last exercise.

# Run the optimization
opt_rebal_rb_robust <- optimize.portfolio.rebalancing(R = df3_xts, 
                                                      momentFUN = "moments_robust",
                                                      portfolio = port_spec, 
                                                      optimize_method = "random",
                                                      trace = TRUE,
                                                      rebalance_on = "quarters", 
                                                      training_period = 60,
                                                      rolling_window = 60)

# Chart the weights
chart.Weights(opt_rebal_rb_robust)

# Chart the percentage contribution to risk
chart.RiskBudget(opt_rebal_rb_robust, match.col = "StdDev", risk.type = "percentage")

# Compute the portfolio returns
returns_rb_robust <- Return.portfolio(R = df3_xts, weights = extractWeights(opt_rebal_rb_robust))
colnames(returns_rb_robust) <- "rb_robust"


#Analyze results and compare to benchmark
#In the previous exercises of the chapter, we created an equal weight benchmark r_benchmark and ran the following optimizations:
  
  #Minimize portfolio standard deviation with sample estimates (returns stored in returns_base).
#Minimize portfolio standard deviation with percentage contribution to risk using sample estimates (returns stored in returns_rb).
#Minimize portfolio standard deviation with percentage contribution to risk using robust estimates (returns stored in returns_rb_robust).
#Now we wish to analyze the performance of the optimization backtests and compare with the benchmark.

# Combine the returns
ret <- cbind(r_benchmark, returns_base, returns_rb, returns_rb_robust)

# Compute annualized returns
table.AnnualizedReturns(R = ret)

# Chart the performance summary
charts.PerformanceSummary(R = ret)
