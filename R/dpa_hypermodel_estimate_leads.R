dpa_hypermodel_estimate_leads = function(leads.core, conf.cutoff = 0.2){


  #rename cols to match db
  #because classifier was built with db colnames
  colname.conversion = data.frame(
    db.name = c("hint.title", "hint.description"),
    rs.name = c("act.title.en", "act.description.en"),
    stringsAsFactors = F
  )


  for(col in colnames(leads.core)){
    if(col %in% colname.conversion$rs.name){
      colnames(leads.core)[colnames(leads.core)==col] = colname.conversion$db.name[colname.conversion$rs.name==col]
    }
  }

  #remove any that are too few chars

  #set criteria
  to.classify = (paste(leads.core$hint.title, leads.core$hint.description) %>%
    nchar() > 5) & !is.na(leads.core$hint.description)

  if(sum(to.classify)==0){

    print("No leads eligible for classification because description nchar < 5 or is NA. NA will be returned.")

    return(list(binary.prediction.result = rep(NA, nrow(leads.core)),
                raw.score = rep(NA, nrow(leads.core)))
    )

  } else if(sum(to.classify)<nrow(leads.core)){
    print(paste0((nrow(leads.core)-sum(to.classify)), " leads not eligible for classification as description nchar < 5 or is NA."))
  }else if(sum(to.classify)==nrow(leads.core)){
    print("Classifying all input leads.")
  }
  leads.core.classify = leads.core[to.classify,]

  #list relevant files
  current.wd = getwd()
  wd.pref = str_extract(getwd(), ".+GTA data team Dropbox")
  dpa.data.path = paste0(wd.pref, "/Bastiat/data/dpa classifier/")
  classifiers = paste0(dpa.data.path, list.files(path = dpa.data.path))

  #load model
  dpa.hypermodel.list = classifiers[grepl("dpa_hypermodel\\.Rdata", classifiers)]
  dpa.hypermodel.fname = dpa.hypermodel.list[length(dpa.hypermodel.list)]
  load(dpa.hypermodel.fname)

  #x.predict = dpa_generate_col_probs(leads.core.classify)
  x.predict = dpa_gen_d2v_conc(leads.core.classify)

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

  binary.prediction.result = data.frame(bid = leads.core.classify$bid,
                                        predict.result$yes > conf.cutoff,
                                        stringsAsFactors = F)


  raw.score = data.frame(leads.core.classify$bid,
                         predict.result$yes,
                         stringsAsFactors = F)



  return(
    list(
      binary.prediction.result = binary.prediction.result,
      raw.score = raw.score
    )
  )




}
