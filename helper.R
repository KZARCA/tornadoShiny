library(purrr)
library(dplyr)
currencies = c("AED", "AFN", "ALL", "AMD", "ANG", "AOA", "ARP", "ARS", "AUD", "AWG", "AZN", "BAM", "BBD", "BDT", "BGN", "BHD", "BIF", "BMD", "BND", 
               "BOB", "BOV", "BRL", "BSD", "BTN", "BWP", "BYB", "BZD", "CAD", "CDF", "CHF", "CLF", "CLP", "CNY", "COP", "COU", "CRC", "CUC", "CUP", 
               "CVE", "CYP", "CZK", "DJF", "DKK", "DOP", "DZD", "ECS", "ECV", "EGP", "ERN", "ETB", "EUR", "FJD", "FKP", "GBP", "GEL", "GHS", "GIP", 
               "GMD", "GNF", "GTQ", "GWP", "GYD", "HKD", "HNL", "HRK", "HTG", "HUF", "IDR", "ILS", "INR", "IQD", "IRR", "ISK", "JMD", "JOD", "JPY", 
               "KES", "KGS", "KHR", "KMF", "KPW", "KRW", "KZT", "KWD", "KYD", "KYD", "LAK", "LBP", "LKR", "LRD", "LSL", "LTL", "LVL", "LYD", "MAD", 
               "MDL", "MGA", "MKD", "MMK", "MNT", "MOP", "MRO", "MUR", "MVR", "MWK", "MXN", "MXV", "MYR", "MZN", "NAD", "NGN", "NHF", "NIO", "NOK", 
               "NPR", "NZD", "OMR", "PAB", "PEN", "PGK", "PHP", "PKR", "PLN", "PYG", "QAR", "RON", "RSD", "RUB", "RWF", "SAR", "SBD", "SCR", "SDG", 
               "SEK", "SGD", "SHP", "SLL", "SOS", "SRD", "SSP", "STD", "SVC", "SYP", "SZL", "THB", "TJS", "TMT", "TND", "TOP", "TRY", "TTD", "TWD", 
               "TZS", "UAH", "UGX", "USD", "USN", "USS", "UYU", "UZS", "VEB", "VEF", "VND", "VUV", "WST", "XAF", "XAG", "XAU", "XCD", "XDR", "XFO", 
               "XFU", "XOF", "XPD", "XPF", "XPT", "YER", "ZAR", "ZMK", "ZWL", "Lek", "؋", "$", "ƒ", "$", "ман", "$", "$", "p.", "BZ$", "$", "$b", 
               "KM", "P", "лв", "R$", "$", "៛", "$", "$", "$", "¥", "$", "₡", "kn", "₱", "Kč", "kr", "RD$", "$", "£", "$", "€", "£", "$", "¢", 
               "£", "Q", "£", "$", "L", "$", "Ft", "kr", "Rp", "﷼", "£", "₪", "J$", "¥", "£", "лв", "₩", "₩", "лв", "₭", "£", "$", "ден", "RM"
               , "₨", "$", "₮", "MT", "$", "₨", "ƒ", "$", "C$", "₦", "₩", "kr", "﷼", "₨", "B/.", "Gs", "S/.", "₱", "zł", "﷼", "lei", "руб",
               "£", "﷼", "Дин.", "₨", "$", "$", "S", "R", "₩", "₨", "kr", "CHF", "$", "£", "NT$", "฿", "TT$", "$", "₴", "£", "$", "$U", "лв",
               "Bs", "₫", "﷼", "Z$")

formatE <- function(x, big.mark, decimal.mark) {
  tmp <- vector()
  for (elem in x){
    conv <- suppressWarnings(as.numeric(as.character(elem)))
    if (!is.na(conv))
      tmp <- append(tmp, format(conv, big.mark = big.mark, decimal.mark = decimal.mark, drop0trailing = TRUE, scientific=FALSE, trim=T))
    else
      tmp <- append(tmp, elem)
  }
  formatted <- tmp
  sub(" +$", "", formatted)
}
percent <- function(x, format = "f", ...) {
  map_chr(x, function(y){
    y <- as.numeric(as.character(y))
    digits <- if (y < 0.1) 1 else 0
    paste0(formatC(100 * y, format = format, digits = digits, ...), "%") #prints warnings if the vector t$valuevar contains characters
  })
}

currency.format <- function(x, symbol, currency.sep, currency.pos, big.mark, decimal.mark) {
  if (currency.pos == "before")
    paste(symbol, formatE(x, big.mark = big.mark, decimal.mark=decimal.mark), sep = currency.sep) 
  else 
    paste(formatE(x, big.mark=big.mark, decimal.mark=decimal.mark), sep = currency.sep, symbol) 
}