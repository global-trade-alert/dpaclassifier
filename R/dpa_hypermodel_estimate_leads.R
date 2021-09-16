dpa_hypermodel_estimate_leads = function(leads.core, conf.cutoff = 0.2){
  
  
  #list relevant files
  current.wd = getwd()
  wd.pref = str_extract(getwd(), ".+GTA data team Dropbox")
  dpa.data.path = paste0(wd.pref, "/Bastiat/data/dpa classifier/")
  classifiers = paste0(dpa.data.path, list.files(path = dpa.data.path))
  
  #load model
  dpa.hypermodel.list = classifiers[grepl("hypermodel", classifiers)]
  dpa.hypermodel.fname = dpa.hypermodel.list[length(dpa.hypermodel.list)]
  load(dpa.hypermodel.fname)
  
  #x.predict = dpa_generate_col_probs(leads.core)
  x.predict = dpa_gen_d2v_conc(leads.core)
  
  #deal with NAs, assuming they are from as-yet-unknown inputs, e.g. new AA
  #this means you need to retrain the model, but of course some training data is needed!
  #therefore assume a new AA is relevant = 1, meaning it should be shown to a user 
  #and therefore labelled.
  x.predict[is.na(x.predict)] = 1
  
  
  #control which libraries are loaded so we are not loading a billion ML libraries each time
  #feel free to add more
  if(dpa.hypermodel$hypermodel.method == "RFstandard"){
    library(randomForest)
  }
  
  if(dpa.hypermodel$hypermodel.method == "RFgrid"){
    library(randomForest)
    library(caret)
  }
  
  #xgb needs its special matrices UwU
  if(dpa.hypermodel$hypermodel.method == "XGB"){
    library(caret)
    library(xgboost)
    x.predict = xgb.DMatrix(as.matrix(x.predict))
  }
  
  if(dpa.hypermodel$hypermodel.method %in% c("svmlinear", "rvm")){
    library(caret)
    library(kernlab)
  }
  
  
  #do prediction
  predict.result = predict(dpa.hypermodel$hypermodel, newdata=x.predict, type = "prob", verbose = T) %>% 
    as.data.frame()
  
  #get results ready
  
  binary.prediction.result = predict.result$yes > conf.cutoff
  
  return(
    list(
      binary.prediction.result = binary.prediction.result,
      raw.score = predict.result$yes
    )
  )
  
  
  
  
}