
library(quantmod)

# ETFs' dictionary
etf_names <- c(
  "SGLD.AS" = "Gold", "SPDM.L" = "Palladium", "ETLF.DE" = "Commodities", "EUNW.DE" = "High Yield €",
  "IWDA.AS" = "World", "SXRQ.DE" = "€ Govt Bond 7-10yr", "IBCL.DE" = "€ Govt Bond 15-30yr",
  "IHYA.L" = "High Yield $", "IEAA.MI" = "Corp Bonds €", "VGIT" = "$ Intermediate Treasuries",
  "TLT" = "$ 20+ Year Treasury Bond", "TIP" = "TIPS $", "XAD3.MI" = "Platinum", "SPY2.DE" = "Global Real Estate",
  "XAD6.MI" = "Silver", "VUSA.DE" = "S&P 500", "LYP6.DE" = "Eurostoxx 600", "ZPRA.DE" = "Asian Dividend Aristocrats",
  "IBCI.DE" = "TIPS €", "EXI5.DE" = "European Real Estate", "UKPH.DE" = "UK Real Estate", "AYEP.DE" = "Asian Real Estate",
  "BTC-EUR" = "BTC/€", "BTC-USD" = "BTC/$", "ETH-EUR" = "ETH/€", "ETH-USD" = "ETH/$", "QDVP.DE" = "US MBS", "NUCL.L" = "Nuclear & Uranium",
  "DFEN.DE" = "Defense", "QNTM.L" = "Quantum Computing", "JEDI.L" = "Space Innovators", "ESPO.L" = "Videogaming & eSports",
  "REMX.L" = "Rare Earth & Stategic Metals", "HDRO.L" = "H2", "KRW.PA" = "Korea", "6C=F" = "Canadian Dollar",
   "6A=F" = "Australian Dollar", "6N=F" = "New Zealand Dollar", "6M=F" = "Mexican Peso", "IS0E.DE" = "Gold producers"
)

# Categoríes
bonds <- c("SXRQ.DE", "IBCL.DE", "IBCI.DE", "IEAA.MI", "EUNW.DE", "VGIT", "TLT", "TIP", "IHYA.L", "QDVP.DE")
equities <- c("IWDA.AS", "VUSA.DE", "LYP6.DE", "ZPRA.DE", "KRW.PA")
equities_sector <- c("NUCL.L", "DFEN.DE", "QNTM.L", "JEDI.L", "ESPO.L", "REMX.L", "HDRO.L", "IS0E.DE")
commodity_currencies <- c("6C=F", "6A=F", "6N=F", "6M=F")
commodities <- c("ETLF.DE", "SGLD.AS", "XAD6.MI", "XAD3.MI", "SPDM.L")
real_estate <- c("SPY2.DE", "EXI5.DE", "UKPH.DE", "AYEP.DE")
crypto <- c("BTC-EUR", "BTC-USD", "ETH-EUR", "ETH-USD")

categories <- list(
  Bonds = bonds,
  Equities = equities,
  Equities_Sectors = equities_sector,
  Commodities = commodities,
  Commodity_Currencies = commodity_currencies,
  Real_Estate = real_estate,
  Cryptocurrencies = crypto
)


# dates
end_date <- Sys.Date()
start_date <- end_date - 100

# Data frame para guardar resultados
results <- data.frame(
  Ticker = character(),
  Nombre = character(),
  Categoria = character(),
  Diferencia = numeric(),
  stringsAsFactors = FALSE
)

# Main function
for (cat_name in names(categories)) {
  cat("\n", cat_name, "\n")
  tickers <- categories[[cat_name]]
  
  for (ticker in tickers) {
    tryCatch({
      data <- getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
      close_prices <- Cl(data)
      
      if (NROW(close_prices) < 2) {
        cat("Datos insuficientes para", etf_names[[ticker]], "(", ticker, ")\n")
        next
      }
      
      price_start <- as.numeric(close_prices[1])
      price_end <- as.numeric(close_prices[NROW(close_prices)])
      diff_pct <- (price_end - price_start) / price_start * 100
      
      cat(sprintf("Diferencia de %s: %.2f%%\n", etf_names[[ticker]], diff_pct))
      
      # Guardar resultado
      results <- rbind(results, data.frame(
        Ticker = ticker,
        Nombre = etf_names[[ticker]],
        Categoria = cat_name,
        Diferencia = diff_pct
      ))
      
    }, error = function(e) {
      cat("Error with", ticker, ":", conditionMessage(e), "\n")
    })
  }
#  Sys.sleep(10)  # Espera para evitar bloqueo
}

# Shows 5 values with largest absolute change
if (nrow(results) > 0) {
  cat("\nTop 5 cambios absolutos (positivos o negativos):\n")
  top5 <- results[order(-abs(results$Diferencia)), ][1:5, ]
  print(top5, row.names = FALSE)
} else {
  cat("\nNo se obtuvieron datos suficientes para calcular variaciones.\n")
}
