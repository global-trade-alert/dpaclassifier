dpa_gen_d2v_conc = function(leads.core){
  
  
  #list relevant files
  current.wd = getwd()
  wd.pref = str_extract(getwd(), ".+GTA data team Dropbox")
  dpa.data.path = paste0(wd.pref, "/Bastiat/data/dpa classifier/")
  classifiers = paste0(dpa.data.path, list.files(path = dpa.data.path))
  
  #get cols used to generate embeddings
  dpa.chc.embs = classifiers[grepl("dpa_conc_hypermodel_embeddings", classifiers)]
  most.recent.dpa.chc.embs = dpa.chc.embs[length(dpa.chc.embs)]
  load(most.recent.dpa.chc.embs)
  
  dpa.col.vectors = list()
  
  for(col in training.cols){
    
    col.idx = match(col, colnames(leads.core))
    
    x.train = dpa_d2v_preprocess(col, doc_id = leads.core$bid, text = leads.core[,col.idx])
    
    dpa.col.vectors[[col]] = x.train
    
  }
  
  #conc them all into a big aß matrix
  dpa.conc.vectors = do.call(cbind, dpa.col.vectors) %>% 
    as.data.frame()
  
}