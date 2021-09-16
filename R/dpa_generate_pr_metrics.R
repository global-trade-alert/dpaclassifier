#' Generates precision, and recall metrics for a given IR model. Takes in
#' vectors of the model's predictions, the real label for the documents and an
#' optional model name.
#'
#' @param model.prediction The prediction of your model, boolean.
#' @param real.label The real label of the document, boolean.
#' @param model.name The name of the model (optional)
#'
#' @return data.frame with the scores
#' @references www.globaltradealert.org
#' @Author Callum Campbell for Global Trade Alert.
#'

dpa_generate_pr_metrics = function(model.prediction,
                                   real.label,
                                   model.name = "model"){
  
  model.prediction=as.numeric(model.prediction)
  real.label=as.numeric(real.label)
  
  pred.correct = model.prediction == real.label
  
  accuracy = (length(pred.correct[pred.correct]) / length(pred.correct)) #crude measure of percentage correctly classified
  
  #precision: are the results mostly relevant?
  precision = (length(model.prediction[model.prediction==1 & real.label==1]) / #relevant returned
                 sum(model.prediction == 1)) #n returned
  
  #recall: did the model return all the available relevant docs?
  recall = (length(model.prediction[model.prediction==1 & real.label==1]) /
              sum(real.label == 1)) #n relevant
  
  #F1 measure: harmonic mean of precision and recall. Punishes poor score in either.
  f1 = (2*(precision*recall)) / (precision + recall)
  
  #retain rate: the total percentage of results that are kept
  retain = (length(model.prediction[model.prediction==1])) /
    length(model.prediction)
  
  return(data.frame(model.name=model.name,
                    accuracy = accuracy,
                    precision = precision,
                    recall = recall,
                    f1 = f1,
                    retain = retain))
  
}
