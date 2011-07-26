mBias=function(expr, data){
#expr is real model
#bigMod is lm(yvar~., data)
#make a new population based on model to sample from
#newdata is dataset with box1, box2 box3 box4... for each term and if it should be included in the newdata
  #construct CIs based on new data using all terms. 
  #Plot all terms coefs on number lines, 0 if not in the new model/old model.
  
  
  
  
  
  
  
  n=slider(5, 500, step=1, label="n points to sample")
  seed=slider(#later)
  
}