prices = function(symbols,type = c("open","high","low","close","volume","adjustedClose"),from="2004-01-01",to=Sys.Date(),
                  period=c("days","weeks","months","quarters","years"),returns = FALSE){

  library(quantmod)
  # Building Price Structure
  # ==============================================================
  data_env = new.env()
  getSymbols(symbols,env=data_env,from=from,to=to,auto.assign = T)
  if (type == "open"){
    prices_list = lapply(data_env,Op)
    prices = do.call(merge,prices_list)
  }

  else if (type == "high"){
    prices_list = lapply(data_env,Hi)
    prices = do.call(merge,prices_list)
  }

  else if (type == "low"){
    prices_list = lapply(data_env,Lo)
    prices = do.call(merge,prices_list)
  }

  else if (type == "close"){
    prices_list = lapply(data_env,Cl)
    prices = do.call(merge,prices_list)
  }

  else if (type == "volume"){
    prices_list = lapply(data_env,Vo)
    prices = do.call(merge,prices_list)
  }

  else if (type == "adjustedClose"){
    prices_list = lapply(data_env,Ad)
    prices = do.call(merge,prices_list)
  }

  else {
    print("type not detected")
  }

  # Building Periodic Price Structure
  # =================================================
  if (period == "days"){
    prices = prices
  }
  
  else if (period == "weeks"){
    prices = prices[endpoints(prices, on="weeks",k=1),]
  }
  
  else if (period == "months"){
    prices = prices[endpoints(prices,on="months",k=1),]
  }
  
  else if (period == "quarters"){
    prices = prices[endpoints(prices,on="quarters",k=1),]
  }
  
  else if (period == "years"){
    prices = prices[endpoints(prices,on="years",k=1),]
  }
  
  else {
    print("period not detected")
  }
  
  if (returns){
    prices = diff(prices)/prices[-length(prices)]
    prices = prices[-1,]
  }

}
