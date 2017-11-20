########################################
## PREDICT 422
## Charity Project - Part 4 (The Mailing List Problem)
##
## SaveYourModels.R
########################################

# In order to use your chosen models from Part 2 and Part 3 of the project to predict
# values for the Validation data and Test data for Part 4 of the project, you need a
# means to bring your chosen models into the R Environment. There are several ways to 
# accomplish this (a few possibilities listed here). Note that you may want to 
# alter the model naming convention to distinguish between Part 2 (Regression Problem)
# models and Part 3 (Classification Problem) models.
#   - Run code from Part 2 and Part 3 in the current R session. If you leave the R
#     session running for your Part 4 code, then those models will still be in memory 
#     as your work on Part 4.
#   - Save your chosen models from Part 2 and Part 3 to .RData files. This is a means
#     of saving R objects to a file that is able to be opened again in R.
#
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
#         outPath = file.path("/Users","JLW","Documents","Northwestern MSPA","PREDICT 422","Project - Charity Mailing","Project Data Files")
#         modelPart2 = modelC1
#         save(modelPart2,file=file.path(outPath,"modelPart2.RData"))
#
#      - The first command defines the file path where you want to save the file. It
#        can be set the same as your inPath or not. You need to define a valid file
#        path for your file system.
#      - The second command renames my modelC1 to be called modelPart2. I do this only
#        for the reason that my chosen Part 3 model might also be modelC1. Renaming
#        the model distinguishes my Part 2 model from my Part 3 model. Also, note that
#        you would specify your chosen model here in place of "modelC1".
#      - The third command performs the saving action, specifying which R objects to
#        save (modelPart2 in this case) and the file name (with file path) to save to.
#        You can save more than one object in this command (as you will see in the
#        Part 3 example). For example, the Lasso model for modelC1 was built for the 
#        single value cvLasso$lambda.min. If modelC1 had been trained over a range of
#        lambda values, then we would want to save the chosen optimal value of 
#        cvLasso$lambda.min along with the model.
#   3. You now have your chosen Part 2 model saved to your computer. You can close or
#      terminate your R session, and you will still be able to come back later and 
#      load the model from the .RData file. You can go to the file location that you
#      specified above, and you should see a file named modelPart2.RData.

# Save Part 3 Model: 
#   0. I'll use modelA1 from SampleCodePart3.R for this part. Based on the results
#      obtained in Part 3, modelA1 had the highest TP rate on the Test set. 
#      While the "accuracy" for modelA1 wasn't as high as for some of the other models, 
#      it is really the TP rate that will drive the mailing list selection.
#   1. Go to your Part 3 code and execute the commands that you need so that your
#      chosen model is in memory. It is a good idea to start with a fresh R session
#      here. That way you don't have any unintentional carry-over from your Part 2 
#      code.
#   2. While your chosen Part 3 model is in memory, execute the following commands
#      (modified for your particular situation):
#         outPath = file.path("/Users","JLW","Documents","Northwestern MSPA","PREDICT 422","Project - Charity Mailing","Project Data Files")
#         modelPart3 = modelA1
#          optThreshPart3 = threshA1
#         save(modelPart3,optThreshPart3,file=file.path(outPath,"modelPart3.RData"))
#
#      - When saving to a .RData file, you can list as many objects and values as
#        you want. Follow those objects with the file specification. R will bundle 
#        everything up and save it to one .RData file. For example, you could save
#        multiple models and the data used to train the models as follows:
#           save(classData2,modelA1,optThreshA1,modelB1,optThreshB1,file=file.path(outPath,"exampleSave.RData"))
#   3. You now have your chosen Part 3 model saved to your computer. You can close or
#      terminate your R session, and you will still be able to come back later and 
#      load the model from the .RData file. You can go to the file location that you
#      specified above, and you should see a file named modelPart3.RData.

# Once you have saved your Part 2 and Part 3 models, I recommend you start with a 
# fresh R session again. To load the models from Part 2 and Part 3, we have the 
# following commands:

# Specify the file path where your .RData files are located
modelPath = file.path("/Users","JLW","Documents","Northwestern MSPA","PREDICT 422",
                      "Project - Charity Mailing","Project Data Files")

# Use the load command to load objects from your .RData files.
load(file.path(modelPath,"modelPart2.RData"))
load(file.path(modelPath,"modelPart3.RData"))

# You should now see modelPart2 and modelPart3 (plus any hyper-parameters that you
# saved) in your R Environment (i.e. in memory).