predict_lme_results<-function(model,newdata,factors){

    
 newdata$pred<-predict(model, 
                         newdata=newdata,level=0)
grouz <- ref_grid(model, 
                  cov.keep= factors)

emmrouz <- data.frame(emmeans(grouz, 
                              spec= factors, 
                              level= 0.95),
                      site="Rouzic")

return(emmrouz)
}


