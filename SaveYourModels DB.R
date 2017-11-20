
# Save best regression model 
outPath = file.path("U:/Midterm/")
modelPart2 = modelA1
save(modelPart2,file=file.path(outPath,"modelPart2.RData"))


outPath = file.path("U:/Midterm/")
modelPart3 = glm.fits2
optThreshPart3 = threshA2
save(modelPart3,optThreshPart3,file=file.path(outPath,"modelPart3.RData"))