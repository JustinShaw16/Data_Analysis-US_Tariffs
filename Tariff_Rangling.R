library(ggplot2)
library(reshape2)

#Trade Imbalances Chart
trade_data <- read.csv("TradeImbalances.csv", stringsAsFactors = FALSE)

trade_data$Country <- factor(trade_data$Country, levels = trade_data$Country[order(trade_data$TradeBalance)])
ggplot(trade_data, aes(x = TradeBalance, y = Country)) + geom_bar(stat = "identity", fill = "steelblue") + labs(title = "Bilateral Trade Balances (in Billion USD)", x = "Trade Balance (Billion USD)", y = "Country") + theme_minimal()

#------------------------------------------------------------------------------
#Trade Specific Goods

trade_goods <- read.csv("TradeSpecificGoods.csv", stringsAsFactors = FALSE)

trade_goods$TradeType <- factor(trade_goods$TradeType, levels = c("Export", "Import"))

#U.S. / China Chart (Specific Goods)
china_data <- subset(trade_goods, Country == "China")
ggplot(china_data, aes(x = Good, y = Value, fill = TradeType)) + geom_bar(stat = "identity", position = "dodge") + scale_fill_manual(values = c("Export" = "steelblue", "Import" = "darkorange")) + labs(title = "U.S. Trade with China: Specific Goods", x = "Goods Category", y = "Trade Value (Billion USD)", fill = "Trade Type") + theme_minimal()

#U.S. / European Union Chart (Specific Goods)
eu_data <- subset(trade_goods, Country == "European Union")
ggplot(eu_data, aes(x = Good, y = Value, fill = TradeType)) + geom_bar(stat = "identity", position = "dodge") + scale_fill_manual(values = c("Export" = "steelblue", "Import" = "darkorange")) + labs(title = "U.S. Trade with European Union: Specific Goods", x = "Goods Category", y = "Trade Value (Billion USD)", fill = "Trade Type") + theme_minimal()

#U.S. / Switzerland Chart (Specific Goods)
swiss_data <- subset(trade_goods, Country == "Switzerland")
ggplot(swiss_data, aes(x = Good, y = Value, fill = TradeType)) + geom_bar(stat = "identity", position = "dodge") + scale_fill_manual(values = c("Export" = "steelblue", "Import" = "darkorange")) + labs(title = "U.S. Trade with Switzerland: Specific Goods", x = "Goods Category", y = "Trade Value (Billion USD)", fill = "Trade Type") + theme_minimal()

#------------------------------------------------------------------------------
#Policy Data

policy_data <- read.csv("TariffData.csv", stringsAsFactors = FALSE)
policy_data$Direction <- paste(policy_data$Country, "->", policy_data$Destination)
policy_long <- melt(policy_data, id.vars = c("Country", "Destination", "Good", "Direction"), measure.vars = c("TariffRate", "NTLC"), variable.name = "Measure", value.name = "Value")

#U.S. / China Chart (Policy)

china_policy_data <- subset(policy_long, (Country == "United States" & Destination == "China") | (Country == "China" & Destination == "United States"))

ggplot(china_policy_data, aes(x = Good, y= Value, fill = Direction)) + geom_bar(stat = "identity", position = "dodge") + facet_wrap(~Measure, scales = "free_y") + scale_fill_manual(values = c("United States -> China" = "steelblue", "China -> United States" = "darkorange")) + labs(title = "Trade Policy Measures: U.S. vs. China", x = "Goods Category", y = "Value", fill = "Direction") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#U.S. / European Union Chart (Policy)

eu_policy_data <- subset(policy_long, (Country == "United States" & Destination == "European Union") | (Country == "European Union" & Destination == "United States"))

ggplot(eu_policy_data, aes(x = Good, y= Value, fill = Direction)) + geom_bar(stat = "identity", position = "dodge") + facet_wrap(~Measure, scales = "free_y") + scale_fill_manual(values = c("United States -> European Union" = "steelblue", "European Union -> United States" = "darkorange")) + labs(title = "Trade Policy Measures: U.S. vs. European Union", x = "Goods Category", y = "Value", fill = "Direction") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#U.S. / Switzerland (Policy)

swiss_policy_data <- subset(policy_long, (Country == "United States" & Destination == "Switzerland") | (Country == "Switzerland" & Destination == "United States"))

ggplot(swiss_policy_data, aes(x = Good, y= Value, fill = Direction)) + geom_bar(stat = "identity", position = "dodge") + facet_wrap(~Measure, scales = "free_y") + scale_fill_manual(values = c("United States -> Switzerland" = "steelblue", "Switzerland -> United States" = "darkorange")) + labs(title = "Trade Policy Measures: U.S. vs. Switzerland", x = "Goods Category", y = "Value", fill = "Direction") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

