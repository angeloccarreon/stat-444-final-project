library(mgcv)
# housing_raw = read.csv(file="house-prices-advanced-regression-techniques/train.csv")
# housing = read.csv(file="house-prices-advanced-regression-techniques/train.csv")

data$Neighborhood = as.numeric(factor(data$Neighborhood))
data$House.Style = as.numeric(factor(data$House.Style))
data$Exterior.1st = as.numeric(factor(data$Exterior.1st))
data$Exterior.2nd = as.numeric(factor(data$Exterior.2nd))
data$Foundation = as.numeric(factor(data$Foundation))
data$Bsmt.Qual = as.numeric(factor(data$Bsmt.Qual))
data$BsmtFin.Type.1 = as.numeric(factor(data$BsmtFin.Type.1))
data$Kitchen.Qual = as.numeric(factor(data$Kitchen.Qual))
data$Fireplace.Qu = as.numeric(factor(data$Fireplace.Qu))
data$Garage.Finish = as.numeric(factor(data$Garage.Finish))

test_all = gam(SalePrice ~
                 s(data$Neighborhood, k=25) +
                 s(data$House.Style, k=8) +
                 s(data$Overall.Qual, k=10) +
                 s(data$Year.Built) +
                 s(data$Exterior.1st, k=15) +
                 s(data$Exterior.2nd, k=16) +
                 s(data$Mas.Vnr.Area) +
                 s(data$Foundation, k=6) +
                 s(data$Bsmt.Qual, k=6) +
                 s(data$BsmtFin.SF.1) +
                 s(data$Total.Bsmt.SF) +
                 s(data$Gr.Liv.Area) +
                 s(data$Full.Bath, k=5) +
                 s(data$Garage.Cars, k=7) +
                 s(data$Kitchen.Qual, k=4),
               data=data)
par(mfrow=c(4, 4))

for (i in 1:14) {
  plot(test_all, shift=mean(data$SalePrice))
  points(data$Overall.Qual, data$SalePrice)
}

test_num = gam(SalePrice ~
                 s(housing$Overall.Qual, k=10) +
                 s(housing$Year.Built) +
                 s(housing$Mas.Vnr.Area) +
                 s(housing$BsmtFin.SF.1) +
                 s(housing$Total.Bsmt.SF) +
                 s(housing$Gr.Liv.Area) +
                 s(housing$Full.Bath, k=5) +
                 s(housing$Garage.Cars, k=7) +
                 s(housing$Garage.Area),
               data=housing)
par(mfrow = c(3, 3))
plot(test_num, shift=mean(housing$SalePrice), ylim=c(0,500000))

test_cat = gam(SalePrice ~
                 s(housing$Neighborhood, k=25) +
                 s(housing$House.Style, k=8) +
                 s(housing$Exterior.1st, k=15) +
                 s(housing$Exterior.2nd, k=16) +
                 s(housing$Foundation, k=6) +
                 s(housing$Kitchen.Qual, k=4),
               data=housing)
par(mfrow=c(3,2))
plot(test_gam, shift=mean(housing$SalePrice))

points(housing$Neighborhood, housing$SalePrice)

points(housing$MSZoning, housing$SalePrice - mean(housing$SalePrice))

# Making predictions
pred = predict(test_gam)

# Now they match up
plot(housing$MSZoning, housing$SalePrice)
lines(sort(housing$MSZoning), pred, col="red")
