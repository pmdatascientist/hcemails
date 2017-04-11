
loadpos<- function(){
  pos = scan("positive-words.txt", what='character')
  poswords=c(pos)
  return(poswords)
}

loadneg<- function(){
  neg = scan("negative-words.txt", what='character')
  negwords=c(neg)
  return(negwords)
}

scoring<- function(sentence,poswords,negwords){
  
  sentence= gsub('[[:punct:]]','',sentence)
  sentence= gsub('[[:cntrl:]]','',sentence)
  sentence= gsub('\\d+','',sentence)

  sentence=tolower(sentence)
  
  word.list= str_split(sentence,'\\s+')
  words= unlist(word.list)
  pos.matches= match(words,poswords)
  neg.matches= match(words,negwords)
  
  pos.matches= !is.na(pos.matches)
  neg.matches= !is.na(neg.matches)
  
  score= sum(pos.matches) - sum(neg.matches)
  
  return(score)
  
}

posscoring<- function(sentence,poswords,negwords){
  
  sentence= gsub('[[:punct:]]','',sentence)
  sentence= gsub('[[:cntrl:]]','',sentence)
  sentence= gsub('\\d+','',sentence)
  
  sentence=tolower(sentence)
  
  word.list= str_split(sentence,'\\s+')
  words= unlist(word.list)
  pos.matches= match(words,poswords)
  neg.matches= match(words,negwords)
  
  pos.matches= !is.na(pos.matches)
  neg.matches= !is.na(neg.matches)
  
  score= sum(pos.matches) 
  
  return(score)
  
}


negscoring<- function(sentence,poswords,negwords){
  
  sentence= gsub('[[:punct:]]','',sentence)
  sentence= gsub('[[:cntrl:]]','',sentence)
  sentence= gsub('\\d+','',sentence)
  
  sentence=tolower(sentence)
  
  word.list= str_split(sentence,'\\s+')
  words= unlist(word.list)
  pos.matches= match(words,poswords)
  neg.matches= match(words,negwords)
  
  pos.matches= !is.na(pos.matches)
  neg.matches= !is.na(neg.matches)
  
  score= sum(neg.matches) 
  
  return(score)
  
}

totalwords<- function(sentence,poswords,negwords){
  
  sentence= gsub('[[:punct:]]','',sentence)
  sentence= gsub('[[:cntrl:]]','',sentence)
  sentence= gsub('\\d+','',sentence)
  
  sentence=tolower(sentence)
  
  word.list= str_split(sentence,'\\s+')
  words= unlist(word.list)
  pos.matches= match(words,poswords)
  neg.matches= match(words,negwords)
  
  pos.matches= !is.na(pos.matches)
  neg.matches= !is.na(neg.matches)
  
  score= sum(neg.matches) +sum(pos.matches)
  
  return(score)
  
}

