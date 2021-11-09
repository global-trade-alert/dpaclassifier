
dpa_train_model = function(training.testing.split = 0.82,
                           training.cols = c("hint.title",
                                             "hint.description",
                                             "acting.agency"),
                           hypermodel.method = "XGB" #TODO remove default param
                           ){

  if(training.testing.split != 1){
    message("Training/testing split <1; performance metrics will be generated automatically.")
  }

  dpa_classifier_init()




  if(! hypermodel.method %in% c("RFstandard", "RFgrid", "XGB")){
    stop("please provide the desired model type from \"RFstandard\", \"RFgrid\", \"XGB\".")
  }



  print("Getting training data...")
  training.dpa.full = dpa_generate_training_data()
  print("Training data received.")
  print(paste("Total docs:", nrow(training.dpa.full)))
  print(paste("Relevant docs:", sum(training.dpa.full$relevant==1)))
  print(paste("Irelevant docs:", sum(training.dpa.full$relevant==0)))

  #if(show.text.summary){
  print("Summary of training corpus titles word counts:")
  training.dpa.full$hint.title %>%
    strsplit(" ") %>%
    sapply(length) %>%
    summary() %>%
    print()

  print("Summary of training corpus texts word counts:")
  training.dpa.full$hint.description %>%
    strsplit(" ") %>%
    sapply(length) %>%
    summary() %>%
    print()
  #}

  # Preprocess text ---------------------------------------------------------

  orig.rows=nrow(training.dpa.full)
  orig.bids = training.dpa.full$bid

  print("preprocessing text...")
  for(col in training.cols){
    print(col)

    col.idx = match(col, colnames(training.dpa.full))

    training.dpa.full[,col.idx] = dpa_text_preprocess(training.dpa.full[,col.idx])

    training.dpa.full = subset(training.dpa.full, grepl(pattern = "[a-z]", training.dpa.full[,col.idx]))

    rm(col.idx)
  }

  rem.bids = which(! orig.bids %in% training.dpa.full$bid) %>% paste(collapse = ", ")
  warning(paste(orig.rows-nrow(training.dpa.full), "rows removed during preprocessing! probably because they consisted of only bad chars ([^A-z]) which cannot be classified! The BIDs for the removed rows were:", rem.bids))



  # train test split --------------------------------------------------------

  training.id = sample.int(nrow(training.dpa.full), size = nrow(training.dpa.full)*training.testing.split)
  training.dpa = training.dpa.full[training.id,]
  testing.dpa = training.dpa.full[-training.id,]
  rm(training.id)







  # W2V/D2V EMBEDDINGS ------------------------------------------------------


  #functionised to enable easy adjustment of on which cols to train the model

  #this only needs to be done as part of training function so I leave it in here

  generate_w2v_embeddings = function(text.name, text){

    if(is.null(text.name)){
      stop("you must provide names so the model can be saved properly!")
    }

    set.seed(221)

    print(paste(text.name, "- preparing word vector embeddings, may take a while..."))

    model.w2v = word2vec(x = text,
                         type = "skip-gram",
                         dim = 100,
                         iter = 20,
                         min_count = 1)

    this.emb.fname = paste0("data/dpa classifier/", format(Sys.Date(), "%Y-%m-%d"), "_", text.name,"_w2v_embedding.bin")

    print(paste(text.name, "w2v embeddings created! saving to",this.emb.fname))
    write.word2vec(model.w2v, file = this.emb.fname)


  }

  for(col in training.cols){

    col.idx = match(col, colnames(training.dpa))
    train.w2v = training.dpa[,col.idx]
    generate_w2v_embeddings(text.name = col, text = train.w2v)
    rm(col.idx)

  }





  # d2v concatenated hyperclassifier (DCHC) ---------------------------------


  #this will save which cols were used to generate embeddings which are
  #concatenated later and used to train the concatenated hypermodel
  hypermodel_emb_cols_fname = paste0("data/dpa classifier/", format(Sys.Date(), "%Y-%m-%d"), "_dpa_conc_hypermodel_training_cols.rda")
  save(training.cols, file = hypermodel_emb_cols_fname)


  set.seed(221)




  #x.train$evaluation = as.factor(training.dpa$relevant)

  hc.relevant = ifelse(test = training.dpa$relevant==1,
                       yes = "yes",
                       no = "no")

  col.predictions = dpa_gen_d2v_conc(training.dpa)

  col.predictions$relevant=as.factor(hc.relevant)

  rownames(col.predictions) = training.dpa$bid

  #use this to tune the sample size... it can't be larger than this number of course
  class.freq.min = min(table(col.predictions$relevant))


  # 1. Simple RandomForest with no grid search etc - this is the fastest to train (and not bad performance!)
  if(hypermodel.method == "RFstandard"){

    library(randomForest)
    print(paste(col, "Creating new hypermodel... (non-grid search)"))

    hypermodel = randomForest(relevant ~ .,
                              data=col.predictions)


    #2. RandomForest with grid search and other tuning params - slower (prelim acc = 58%)
  }else if(hypermodel.method == "RFgrid"){
    library(caret)
    library(randomForest)

    print(paste("Creating new hypermodel... (RF w/ grid search, may take a while)"))

    #with grid search - prepare your RAM

    # K-fold cross-validation from caret
    # Define the control
    # took about 40-45 mins to train on my machine
    rf.trControl <- trainControl(method = "repeatedcv",
                                 number = 10,
                                 repeats = 3,
                                 search = "grid",
                                 sampling = "up")

    # normal cv, much faster
    # rf.trControl(method = "cv",
    #              number = 5,
    #              search = "grid")

    mtry <- sqrt(ncol(col.predictions %>% select(-relevant)))

    rf.tunegrid <- expand.grid(.mtry=mtry)

    hypermodel = train(relevant ~ .,
                       data=col.predictions,
                       method='rf',
                       metric="Accuracy",
                       #mtry=33,
                       tunegrid = rf.tunegrid,
                       trControl = rf.trControl)


    # 3. XGBoost - probably no good for this dataset as quite small
    # (prelim tests had accuracy of 43%)
  }else if(hypermodel.method == "XGB"){

    library(caret)
    print(paste("Creating new hypermodel... (XGB w/ grid search, may take a while)"))
    library(xgboost)
    library(ROSE)

    #xgb likes to use dmatrices - IIRC this is required in python but not in R,
    #either way I do it here
    x.train = xgb.DMatrix(as.matrix(col.predictions %>% select(-relevant)))
    y.train = as.factor(col.predictions$relevant)

    # cross-validation method and number of folds + enable parallel computation
    xgb.trControl = trainControl(
      method = "cv",
      number = 10,
      allowParallel = TRUE,
      verboseIter = FALSE,
      returnData = FALSE
    )

    #grid space to search for the best hyperparameters
    xgbGrid <- expand.grid(nrounds = c(100,200),
                           max_depth = c(10, 15, 20, 25),
                           colsample_bytree = seq(0.5, 0.9, length.out = 5),
                           # The values below are default values in the sklearn-api.
                           eta = 0.1,
                           gamma=0,
                           min_child_weight = 1,
                           subsample = 1
    )

    hypermodel = train(
      x.train, y.train,
      trControl = xgb.trControl,
      tuneGrid = xgbGrid,
      method = "xgbTree"
    )

    # Support Vector Machines - Linear
    # prelim.acc ~50%
    # unfortunately errors out when used on unseen data, which is apparently common to this implementation of SVM
    # nothing I can do to fix except trying random seed values, which sounds problematic
  }else if(hypermodel.method == "svmLinear"){

    library(caret)
    library(kernlab)
    library(ROSE)#for sampling

    #these params are the best result I got after many tests
    #including a 1000 fold cv
    svm.trControl <- trainControl(method = "repeatedcv",
                                  number = 10,
                                  repeats = 3,
                                  sampling = "up")
    #classProbs = T)

    hypermodel <-
      train(
        relevant ~ .,
        data = col.predictions,
        method = "svmLinear",
        trControl = svm.trControl,
        preProcess = c("center", "scale"),
        tuneGrid = expand.grid(C = seq(0.5, 2, length = 20))
      )




    #Relevance Vector Machines (RVM)
    #currently not supported for classification in R :(
    #I leave it here in case it is one day
  }else if(hypermodel.method == "rvm"){

    rvm.trControl <- trainControl(method = "repeatedcv",
                                  number = 10,
                                  repeats = 3)

    hypermodel <- train(relevant ~.,
                        data = col.predictions,
                        method = "rvmLinear",
                        trControl=rvm.trControl,
                        preProcess = c("center", "scale"),
                        tuneLength = 10)

    #standard Naive Bayes
  }else if(hypermodel.method == "NBstandard"){

    library(caret)
    library(klaR)
    library(e1071) #this may not be required here

    nb.trControl <- trainControl(method = "repeatedcv",
                                 number = 10,
                                 repeats = 3,
                                 search = "grid")

    #fL = laplace correction - during testing this appeared to make zero difference,
    #may be due to the specific nature of the training data used

    # accuracy seems lower than RF or XGB
    nb.tuneGrid <- data.frame(fL = seq(1, length = 4),
                              usekernel = T,
                              adjust = seq(0.25, 1.5, length = 4))


    hypermodel <- train(relevant ~.,
                        data = col.predictions,
                        method = "nb",
                        trControl=nb.trControl,
                        preProcess = c("center", "scale"),
                        tuneGrid = nb.tuneGrid)




  }

  dpa.hypermodel = list(hypermodel = hypermodel,
                        hypermodel.method = hypermodel.method)


  dpa.hypermodel.fname = paste0("data/dpa classifier/", format(Sys.Date(), "%Y-%m-%d"), "_dpa_hypermodel.Rdata")

  print(paste("New DPA hypermodel created!"))
  print(paste("Saving to:", dpa.hypermodel.fname))

  save(dpa.hypermodel, file = dpa.hypermodel.fname)








  # TESTING -----------------------------------------------------------------

  ##### TESTING NEW CLASSIFIER #####

  # if called for

  if(training.testing.split !=1){

    print(paste("The retain quantile used for testing is", 1-training.testing.split))

    estimation=dpa_hypermodel_estimate_leads(testing.dpa)

    #original col called "relevance" was used for training
    #dpa_hypermodel_estimate_leads() also creates col called "relevance" to allow merging with bt_leads_core_update()
    #in this case, we need to compare the two "relevances" so rename the new one
    colnames(estimation$binary.prediction.result) = c("bid", "hypermodel.relevance")

    testing.dpa.metrics = testing.dpa
    testing.dpa.metrics = merge(testing.dpa.metrics, estimation$raw.score, all.x = T)
    testing.dpa.metrics = merge(testing.dpa.metrics, estimation$binary.prediction.result, all.x = T)
    testing.dpa.metrics$correct = testing.dpa.metrics$hypermodel.relevance == testing.dpa.metrics$relevant

    pr.metrics=dpa_generate_pr_metrics(model.prediction = testing.dpa.metrics$hypermodel.relevance,
                                       real.label = testing.dpa.metrics$relevant,
                                       model.name = hypermodel.method)



    # R AT K SCORING
    testing.dpa.rak = testing.dpa.metrics[order(-testing.dpa.metrics$relevance.probability),]

    tot.rlv = sum(testing.dpa.rak$relevant)

    r.at.k = data.frame(results.percentile=seq(10, 100, 10),
                        score = numeric(10L))

    colnames(r.at.k)[2] = paste0(hypermodel.method, ".score")
    for(grp in seq(.1, 1, .1)){

      grp.count = grp*10

      grp.rows = nrow(testing.dpa.rak)/10

      this.grp = ceiling(grp.count*grp.rows)

      results.grp = testing.dpa.rak[c(1:this.grp),]

      grp.score = sum(results.grp$relevant)/tot.rlv

      r.at.k[grp.count,2] = grp.score

    }

    #use this if testing several models
    #r.at.k.all = merge(r.at.k.all, r.at.k)

    library(reshape2)

    plotdata = melt(r.at.k, id = "results.percentile")

    ggplot(data=plotdata,
           aes(x=results.percentile, y=value, colour=variable)) +
      geom_line()







    #bind metrics together when testing different models
    #pr.all = rbind(pr.all, pr.metrics)

    # used with JF's DS model comparison script
    # result2 = data.frame(bid = testing.dpa$bid,
    #                      contender = "Name",
    #                      pred = testing.dpa$`TRUE`,
    #                      evaluation = testing.dpa$evaluation)

    # to generate the df in full use this:
    # tb2 = leads.core.dpa
    #
    # setnames(leads.core.dpa, "hint.title", "act.title.en")
    # setnames(leads.core.dpa, "hint.description", "act.description.en")
    #
    # res = bt_estimate_news_leads(leads.core.news = tb2,
    #                              binary.prediction = F,
    #                              return.both = T,
    #                              conf.cutoff = 0.3)
    #
    # mrs.h.w2v = data.frame(bid = tb2$bid,
    #                        contender = "mrs h d2v",
    #                        pred = res$raw.score,
    #                        evaluation = tb2$hint.state.id %in% c(5,6,7))
    #
    print("The precision, recall, etc, metrics will be returned for the model's performance on the test set.")
    return(pr.metrics)

  }



}
