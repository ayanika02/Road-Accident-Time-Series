library(fpp3)
library(zoo)
library(forecast)
library(feasts)

summary(global_economy)
view(global_economy)
global_economy

algeria <- global_economy |> 
  filter(Country=="Algeria")
class(algeria)  #gives tsibble
typeof(algeria)  #gives list
fit <- algeria |>
  model(ETS(Exports ~ error("A")
                  + trend("N") + season("N")))
 components(fit) |> autoplot()

report(fit)
autoplot(algeria,Growth)
autoplot(algeria)

fit <- algeria |>
  model(
    ses = ETS(Exports ~ error("A") + trend("N") + season("N")),
    holt = ETS(Exports ~ error("A") + trend("A") + season("N")),
    damped = ETS(Exports ~ error("A") + trend("Ad") + season("N"))
  )

tidy(fit)
view(accuracy(fit))

fit2 <- global_economy |>
  mutate(Pop = Population/1e6) |>
  model(ets = ETS(Pop))
fit2

forecast(fit2)
residuals(fit2,type="response")
