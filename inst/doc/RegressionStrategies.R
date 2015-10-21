### R code from vignette source 'RegressionStrategies.Rnw'

###################################################
### code chunk number 1: RegressionStrategies.Rnw:32-51
###################################################
# Load the smwrBase package
library(smwrBase)
# Construct the synthetic data
DF <- structure(list(Date = structure(c(14885, 14922, 14960, 
    14998, 15036, 15074, 15112, 15150, 15188, 15226, 15263, 
    15301, 15339, 15377, 15415, 15453, 15491, 15529, 15567, 
    15605), class = "Date"),
  Flow = c(409, 509, 221, 2180, 4010, 2380, 6650, 3960, 3860, 
    558, 776, 1130, 1390, 1310, 2280, 2820, 1900, 1070, 483, 192), 
  Y1 = c(0.55, 0.65, 0.55, 1.18, 1.56, 1.17, 1.97, 1.38, 1.64, 
    0.74, 0.7, 0.88, 0.85, 0.96, 1.03, 1.26, 1.09, 0.8, 0.71, 0.52),
  Y2 = c(0.52, 0.5, 0.49, 2.15, 2.5, 1.59, 2.34, 1.64, 1.9, 0.4, 
    0.77, 0.88, 1.81, 1.11, 1.69, 1.52, 1.28, 0.74, 0.55, 0.45),
  Y3 = c(0.56, 0.65, 2.4, 2.54, 3.34, 0.75, 0.78, 0.35, 0.5, 0.31, 
    0.51, 2.23, 2.03, 2.41, 1.77, 1.19, 0.53, 0.35, 0.56, 0.33)),
  .Names = c("Date", "Flow", "Y1", "Y2", "Y3"), 
  row.names = c(NA, -20L), class = "data.frame")
# Print the first few rows
head(DF)


###################################################
### code chunk number 2: RegressionStrategies.Rnw:59-61
###################################################
with(DF, plot(Flow, Y1, type='p', pch=16, tck=.02, log="xy", las=1))
abline(-1.1828,       0.3728)


###################################################
### code chunk number 3: RegressionStrategies.Rnw:70-72
###################################################
# The model with a second-order polynomial term
lm(log10(Y1) ~ quadratic(log10(Flow)), data=DF)


###################################################
### code chunk number 4: RegressionStrategies.Rnw:75-79
###################################################
with(DF, plot(Flow, Y1, type='p', pch=16, tck=.02, log="xy", las=1))
curve(10^(-0.07486 + 0.37284*(log10(x) - 3.03898) + 
        0.13653*(log10(x) - 3.03898)^2),
      166, 7700, add=TRUE)


###################################################
### code chunk number 5: RegressionStrategies.Rnw:92-98
###################################################
# Try models with ladder of powers between 0 and 1
deviance(lm(log10(Y1) ~ boxCox(Flow, 0.), data=DF))
deviance(lm(log10(Y1) ~ boxCox(Flow, 0.25), data=DF))
deviance(lm(log10(Y1) ~ boxCox(Flow, 0.50), data=DF))
deviance(lm(log10(Y1) ~ boxCox(Flow, 0.75), data=DF))
deviance(lm(log10(Y1) ~ boxCox(Flow, 1.), data=DF))


###################################################
### code chunk number 6: RegressionStrategies.Rnw:103-105
###################################################
# The model with fourth-root transform
lm(log10(Y1) ~ boxCox(Flow, 0.25), data=DF)


###################################################
### code chunk number 7: RegressionStrategies.Rnw:108-111
###################################################
with(DF, plot(Flow, Y1, type='p', pch=16, tck=.02, log="xy", las=1))
curve(10^(-0.5956165 + 0.0001308 * boxCox(x, .25, GM=1262.052)),
      166, 7700, add=TRUE)


###################################################
### code chunk number 8: RegressionStrategies.Rnw:120-125
###################################################
# Try models with hyperbolic factors from 0 to -3
deviance(lm(log10(Y1) ~ hyperbolic(Flow, factor=0), data=DF))
deviance(lm(log10(Y1) ~ hyperbolic(Flow, factor=-1), data=DF))
deviance(lm(log10(Y1) ~ hyperbolic(Flow, factor=-2), data=DF))
deviance(lm(log10(Y1) ~ hyperbolic(Flow, factor=-3), data=DF))


###################################################
### code chunk number 9: RegressionStrategies.Rnw:128-133
###################################################
# Try models with hyperbolic factors from 0 to -.75 (negtive 1 can be excluded)
deviance(lm(log10(Y1) ~ hyperbolic(Flow, factor=0), data=DF))
deviance(lm(log10(Y1) ~ hyperbolic(Flow, factor=-.25), data=DF))
deviance(lm(log10(Y1) ~ hyperbolic(Flow, factor=-.50), data=DF))
deviance(lm(log10(Y1) ~ hyperbolic(Flow, factor=-.75), data=DF))


###################################################
### code chunk number 10: RegressionStrategies.Rnw:138-140
###################################################
# The model with the hyperbolic transform
lm(log10(Y1) ~ hyperbolic(Flow, factor=-.25), data=DF)


###################################################
### code chunk number 11: RegressionStrategies.Rnw:143-146
###################################################
with(DF, plot(Flow, Y1, type='p', pch=16, tck=.02, log="xy", las=1))
curve(10^(-0.3071 + 0.000239 * hyperbolic(x, factor=-.25, scale=1904.4)),
      166, 7700, add=TRUE)


###################################################
### code chunk number 12: RegressionStrategies.Rnw:160-163
###################################################
with(DF, plot(Flow, Y2, type='p', pch=16, tck=.02, log="xy", las=1))
with(DF, loess.smooth(log(Flow), log(Y2), span=.5)) -> DF.smo
with(DF.smo, lines(exp(x), exp(y)))


###################################################
### code chunk number 13: RegressionStrategies.Rnw:170-172
###################################################
# The model with the hyperbolic transform
lm(log10(Y2) ~ sCurve(log10(Flow), location=log10(1200)), data=DF)


###################################################
### code chunk number 14: RegressionStrategies.Rnw:175-178
###################################################
with(DF, plot(Flow, Y2, type='p', pch=16, tck=.02, log="xy", las=1))
curve(10^(0.00196 + 0.90982 * sCurve(log10(x), location=log10(1200))),
      166, 7700, add=TRUE)


###################################################
### code chunk number 15: RegressionStrategies.Rnw:185-187
###################################################
# The model with the sCurve transform
lm(log10(Y2) ~ sCurve(log10(Flow), location=log10(1200), scale=5), data=DF)


###################################################
### code chunk number 16: RegressionStrategies.Rnw:190-194
###################################################
with(DF, plot(Flow, Y2, type='p', pch=16, tck=.02, log="xy", las=1))
curve(10^( -0.004898 + 0.420611 * sCurve(log10(x), location=log10(1200),
                                    scale=5, shape=1)),
      166, 7700, add=TRUE)


###################################################
### code chunk number 17: RegressionStrategies.Rnw:208-210
###################################################
# The model with fourier transform
lm(log10(Y3) ~ fourier(Date), data=DF)


###################################################
### code chunk number 18: RegressionStrategies.Rnw:213-217
###################################################
with(DF, plot(Date, Y3, type='p', pch=16, tck=.02, log="y", las=1))
curve(10^(-0.04152 +0.23159*sin((x-14975)/365*2*pi ) +
            0.38081*cos((x-14975)/365*2*pi )),
      14857, 15633, add=TRUE)


