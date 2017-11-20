########################################
## PREDICT 422
## Charity Project - Part 4 (The Mailing List Problem)
##
## SaveYourModels.R
########################################

# I'm going with the second approach. 

# Save Part 2 Model: 
#   0. For the sake of discussion, let's use modelC1 from SampleCodePart2.R for this
#      part. While modelC1 wasn't the top model on the Part 2 test data, it was close,
#      and it will be more illustrative for me to use a Lasso model here than a
#      regression model.
#   1. Go to your Part 2 code and execute the commands that you need so that your
#      chosen model is in memory (or you can execute ALL of your code from Part 2).
#      You want the model to be trained on the Part 2 training set, as before, so you
#      don't need to make any changes to your Part 2 code.
#   2. While your chosen Part 2 model is in memory, execute the following commands
#      (modified for your particular situation):
#         
#outPath = file.path("/Users","JLW","Documents","Northwestern MSPA","PREDICT 422","Project - Charity Mailing","Project Data Files")
modelPart2 = regfit.fwd
save(modelPart2,file=("modelPart2.RData"))

# Save Part 3 Model: 

#   2. While your chosen Part 3 model is in memory, execute the following commands
#      (modified for your particular situation):
#         outPath = file.path("/Users","JLW","Documents","Northwestern MSPA","PREDICT 422","Project - Charity Mailing","Project Data Files")
modelPart3 = modelC1
optThreshPart3 = threshC1
save(modelPart3,optThreshPart3,file=("modelPart3.RData"))
#


# Specify the file path where your .RData files are located
modelPath = file.path("/Users","JLW","Documents","Northwestern MSPA","PREDICT 422",
                      "Project - Charity Mailing","Project Data Files")

# Use the load command to load objects from your .RData files.
load(file.path(modelPath,"modelPart2.RData"))
load(file.path(modelPath,"modelPart3.RData"))

# You should now see modelPart2 and modelPart3 (plus any hyper-parameters that you
# saved) in your R Environment (i.e. in memory).