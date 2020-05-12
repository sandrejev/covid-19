library(dplyr)
library(ggplot2)
library(prophet)


download.file("https://op.mos.ru/EHDWSREST/catalog/export/get?id=814656", "civil_registry")
months = data.frame(MonthNumber=1:12, RussianName=c("Январь", "Февраль", "Март", "Апрель", "Май", "Июнь", "Июль", "Август", "Сентябрь", "Октябрь", "Ноябрь", "Декабрь"), stringsAsFactors=F)
data.civil_registry = readr::read_delim(unz("civil_registry", "data-6267-2020-05-08.csv"), delim=";", locale=readr::locale(encoding='PT154')) %>%
  dplyr::inner_join(months, by=c("Month"="RussianName")) %>%
  dplyr::mutate(Date=as.Date(sprintf("%i-%02i-01", Year, MonthNumber), "%Y-%m-%d")) %>%
  dplyr::select(Date, StateRegistrationOfDeath) %>%
  dplyr::mutate(Year=as.numeric(format(Date, "%Y")), Month=as.numeric(format(Date, "%m")))
# ggplot(data.civil_registry) +
#   geom_line(aes(x=Date, y=StateRegistrationOfDeath))

model.civil_registry = prophet::prophet(data.civil_registry %>% dplyr::slice(1:dplyr::last(which(data.civil_registry$Month==3))) %>% dplyr::select(ds=Date, y=StateRegistrationOfDeath), yearly.seasonalit=T, weekly.seasonality=F, daily.seasonalit=F)
forecast.civil_registry = prophet::make_future_dataframe(model.civil_registry, periods=12-data.civil_registry$Month[nrow(data.civil_registry)], freq="month")
forecast.civil_registry = predict(model.civil_registry, forecast.civil_registry) %>% dplyr::mutate(Date=as.Date(ds))
forecast.civil_registry = forecast.civil_registry %>% 
  dplyr::mutate(Date=as.Date(Date)) %>%
  dplyr::left_join(data.civil_registry, by=c("Date")) %>%
  dplyr::mutate(Year=as.numeric(format(Date, "%Y")), Month=as.numeric(format(Date, "%m")))
# plot(model.civil_registry, forecast.civil_registry)
# prophet::prophet_plot_components(model.civil_registry, forecast.civil_registry)


ggplot(forecast.civil_registry) +
  geom_line(aes(x=Date, y=StateRegistrationOfDeath, color="Measured")) +
  geom_line(aes(x=Date, y=yhat, color="Predicted")) +
  geom_ribbon(aes(x=Date, ymin=yhat_lower, ymax=yhat_upper, fill="Predicted"), data=forecast.civil_registry, alpha=0.2) +
  scale_color_manual(values=c("Measured"="#E31A1C", "Predicted"="#1F78B4")) +
  scale_fill_manual(values=c("Measured"="#E31A1C", "Predicted"="#1F78B4")) +
  facet_wrap(~Year, scales="free")
