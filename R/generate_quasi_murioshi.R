#' Generating quasi murioshi of popn music
#' 
#' @export
generate_quasi_murioshi <- function(){
  
  source("http://aoki2.si.gunma-u.ac.jp/R/src/plot.R", encoding="euc-jp")
  
  generate_popkun <- function(popnum,high){
    popkun_color <- c("antiquewhite2","yellow","green","blue","red","blue","green","yellow","antiquewhite2")
    aaa <- 67
    bbb <- 67
    popkun_yoko <- c(-2*aaa-2*bbb,-2*aaa-bbb,-aaa-bbb,-aaa,0,aaa,aaa+bbb,2*aaa+bbb,2*aaa+2*bbb)
    
    color <- popkun_color[popnum]
    
    if(popnum %% 2 == 0){
      popkun_size <- 30
      line_size <- 27
      eye <- 5
    }else{
      popkun_size <- 35
      line_size <- 32
      eye <- 0
    }
    
    yoko <- popkun_yoko[popnum]
    plot.ellipsef(200 + yoko,    200 + high,  popkun_size, 23, 0, col=color) #輪郭
    plot.ellipsef(200-15 + yoko + eye, 200+10 + high,  7, 7, 0, col="white") # 左目
    plot.ellipsef(200+15 + yoko - eye, 200+10 + high,  7, 7, 0, col="white") # 右目
    plot.ellipsef(200-13 + yoko + eye, 200+10 + high,  5, 5, 0, col="black") # 左目
    plot.ellipsef(200+13 + yoko - eye, 200+10 + high,  5, 5, 0, col="black") # 右目
    plot.ellipse( 200 + yoko,    200 + high, line_size, 0, 0, 180, 360, col="white", lwd=5) # ライン
  }
  
  plot.start(asp=1)
  
  waku_yoko <- c(-3*aaa-2*bbb,popkun_yoko) + (aaa / 2)
  for(i in 1:10){
    plot.ellipse( 200 + waku_yoko[i],    200, 1000, 0, 90, 180, 360, col="black", lwd=3)
  }
  
  
  lll <- sample(7:9, 1)
  
  kkk0 <- sample((1:9),lll)
  
  kkk <- list()
  
  kazu <- sample(2:9, 1)
  
  for(i in 1:(kazu-1)){
    mmm <- sample(1:(length(kkk0)-1),1)
    kkk[[i]] <- sample(kkk0,mmm)
    kkk0 <- kkk0[ -which(kkk0 %in% kkk[[i]]) ]
    if(length(kkk0) == 1){
      kazu <- i
      break
    }
  }
  if(kazu != i){
    kkk[[kazu]] <- kkk0
  }
  
  for(j in 1:10000){
    murioshi <- 0
    
    for(i in 1:kazu){
      if(sum((kkk[[i]] <= max(kkk[[i]])-3) & (kkk[[i]] >= min(kkk[[i]])+3) ) != 0){
        
        mmm <- sample(1:(length(kkk[[i]])-1),1)
        kkk[[kazu+1]] <- sample(kkk[[i]],mmm)
        kkk[[i]] <- kkk[[i]][ -which(kkk[[i]] %in% kkk[[kazu+1]]) ]
        
        kazu <- kazu + 1
        murioshi <- murioshi + 1
      }
    }
    
    if(murioshi == 0){
      break
    }
    
  }
  
  high <- -100
  for(i in 1:kazu){
    
    for(l in 1:length(kkk[[i]])){
      generate_popkun(kkk[[i]][l],high)
    }
    
    high <- high + runif(1,10,20)
    
  }
  
}
