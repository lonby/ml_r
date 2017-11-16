machine_learning <- function() {
  ml_cho<-menu(c("knn 알고리즘", "나이브 베이즈","의사 결정트리","회귀 분석"), title="Do you want this?")
  if (ml_cho == 1){
    #knn_fun()
  }
  else if (ml_cho ==2 ) {
    #naviebayes()
  }
  else if (ml_cho ==3 ) {
    
    cho3<-menu(c("정보획득량을 출력하려면 1",
                 "의사결정 트리로 시각화하려면 2",
                 "규칙 기반의 리퍼 알고리즘을 사용하려면 3"))
    if (cho3 == 1){
      information_fun()
    }
    else if (cho3 ==2 ) {
      decision_fun()
    }
    else if (cho3 ==3 ) {
      riper_fun()
    }
    #naviebayes()
  }
  else if (ml_cho == 4){
    cho3<-menu(c("단순 선형 회귀 분석",
                 "다중 선형 회귀 분석",
                 "회귀트리 그래프"))
    if (cho3 == 1){
      multi_reg()
    }
    else if (cho3 ==2 ) {
      multi_reg()
    }
    else if (cho3 ==3 ) {
      reg_tree()
    }
    
  }
  
}






# 3-1. 정보획득량을 출력하려면 1

information_fun <- function(){
  packages <- c("FSelector")
  
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))
  }
  
  library(FSelector)
  
  input_table <- readline('csv파일을 입력하세요. ex) emp.csv : ')
  table_name <- read.csv(input_table, stringsAsFactors = F, header=T)
  
  print(colnames(table_name))
  cat('\n','Data Summary','\n')
  print(head(table_name)[1:3,])
  
  input_header <- readline('컬럼명이 있습니까? ex)T OR F : ')
  input_label <- readline('라벨 컬럼을 입력하세요. ')
  input_rm_num <- readline('배제할 컬럼이 있다면 컬럼 위치번호를 입력하세요. ex) n,n,n ..., 없을 시 0 : ')
  
  input_label_num <- as.integer(input_label_num)
  
  ### 헤더 유무 검사
  if (input_header == 'T'){
    table_name <- read.csv(input_table, stringsAsFactors = T, header=T)
  }else {
    table_name <- read.csv(input_table, stringsAsFactors = T, header=F)
  }
  
  ### label number 지정
  input_label_num <- which(colnames(table_name) == input_label)
  
  ### na값 제거
  table_name <- na.omit(table_name)
  
  ## 라벨 값 추출
  table_label_col<-table_name[, input_label_num]
  
  ### 제거할 컬럼 제거
  split_num<-strsplit(input_rm_num, ',')
  split_num <- sort(as.integer(c(unlist(split_num), input_label_num)), decreasing = T)
  if (0 %in% split_num ){
    table_name <- table_name[,-as.integer(input_label_num)]
  }else{
    
    for(i in split_num){
      table_name <- table_name[,-as.integer(i)]
    }
  }
  
  table2 <- cbind(table_name, lb=factor(table_label_col))
  
  ## 결과
  weights <- information.gain( lb ~ ., table2)
  print(weights)
}



# 3-2. 의사결정 트리로 시각화하려면 2
decision_fun <- function(){
  packages <- c("rattle", "rpart")
  
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))
  }
  
  library(rattle)
  library(rpart.plot)
  
  input_table <- readline('csv파일을 입력하세요. ex) emp.csv : ')
  table_name <- read.csv(input_table, stringsAsFactors = F, header=T)
  
  print(colnames(table_name))
  cat('\n','Data Summary','\n')
  print(head(table_name)[1:3,])
  
  input_header <- readline('컬럼명이 있습니까? ex)T OR F : ')
  input_label <- readline('라벨 컬럼을 입력하세요. ')
  input_rm_num <- readline('배제할 컬럼이 있다면 컬럼 위치번호를 입력하세요. ex) n,n,n ..., 없을 시 0 : ')
  
  
  ### 헤더 유무 검사
  if (input_header == 'T'){
    table_name <- read.csv(input_table, stringsAsFactors = T, header=T)
  }else {
    table_name <- read.csv(input_table, stringsAsFactors = T, header=F)
  }
  
  ### label number 지정
  input_label_num <- which(colnames(table_name) == input_label)
  
  ### na값 제거
  table_name <- na.omit(table_name)
  
  ## 라벨 값 추출
  table_label_col<-table_name[, input_label_num]
  
  ### 제거할 컬럼 제거
  split_num<-strsplit(input_rm_num, ',')
  split_num <- sort(as.integer(c(unlist(split_num), input_label_num)), decreasing = T)
  if (0 %in% split_num ){
    table_name <- table_name[,-as.integer(input_label_num)]
  }else{
    
    for(i in split_num){
      table_name <- table_name[,-as.integer(i)]
    }
  }
  
  table2 <- cbind(table_name, lb=factor(table_label_col))
  
  ## 결과
  tree <- rpart(lb~., data=table2, control=rpart.control(minsplit=2))
  fancyRpartPlot(tree)
  
}



# 3.3 riper


riper_fun <- function(){
  packages <- c("RWeka", "gmodels", "data.table")
  
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))
  }
  
  library(RWeka)
  library(gmodels)
  library(data.table)
  
  input_table <- readline('csv파일을 입력하세요. ex) emp.csv : ')
  table_name <- read.csv(input_table, stringsAsFactors = F, header=T)
  
  print(colnames(table_name))
  cat('\n','Data Summary','\n')
  print(head(table_name)[1:3,])
  
  input_header <- readline('컬럼명이 있습니까? ex)T OR F : ')
  input_label <- readline('라벨 컬럼을 입력하세요. ')
  input_rm_num <- readline('배제할 컬럼이 있다면 컬럼 위치번호를 입력하세요. ex) n,n,n ..., 없을 시 0 : ')
  
  
  ### 헤더 유무 검사
  if (input_header == 'T'){
    table_name <- read.csv(input_table, stringsAsFactors = T, header=T)
  }else {
    table_name <- read.csv(input_table, stringsAsFactors = T, header=F)
  }
  
  ### label 위치
  input_label_num <- which(colnames(table_name) == input_label)
  
  ### na값 제거
  table_name <- na.omit(table_name)
  
  ## 라벨 값 추출
  table_label_col<-table_name[, input_label_num]
  
  ### 제거할 컬럼 제거
  split_num<-strsplit(input_rm_num, ',')
  split_num <- sort(as.integer(c(unlist(split_num), input_label_num)), decreasing = T)
  if (0 %in% split_num ){
    table_name <- table_name[,-as.integer(input_label_num)]
  }else{
    
    for(i in split_num){
      table_name <- table_name[,-as.integer(i)]
    }
  }
  
  table2 <- cbind(table_name, lb=factor(table_label_col))
  ### label 위치 재지정
  input_label_num <- which(colnames(table2) == 'lb')
  
  set.seed(1)
  dim(table2)
  train_cnt <- round(0.75*dim(table2)[1])
  train_index <- sample(1:dim(table2)[1],train_cnt,
                        replace=F)
  train <- table2[train_index, ] 
  test  <- table2[-train_index, ]
  
  
  ## 결과
  model <- JRip(lb~., train, na.action=na.pass)
  result <- predict(model, test[, -input_label_num])
  CrossTable(result, test[,input_label_num])
}



multi_reg <- function(){
  
  graphics.off()
  
  
  
  input_table <- readline('csv파일을 입력하세요. ex) emp.csv : ')
  
  table_name <- read.csv(input_table, stringsAsFactors = F, header=T)
  
  
  
  print(colnames(table_name))
  
  cat('\n','Data Summary','\n')
  
  print(head(table_name)[1:3,])
  
  
  
  input_header <- readline('컬럼명이 있습니까? ex)T OR F : ')
  
  input_y_num <- readline('종속변수의 위치번호를 입력하세요. ex)N (N>=1) : ') 
  
  input_y_num <- as.integer(input_y_num)
  
  
  
  input_x_num <- readline('독립변수의 위치번호를 입력하세요. ex)n OR n,n,n ... (n>=1) : ')
  
  
  
  ### 헤더 유무 검사
  
  if (input_header == 'T'){
    
    table_name <- read.csv(input_table, stringsAsFactors = F, header=T)  
    
  }else {
    
    table_name <- read.csv(input_table, stringsAsFactors = F, header=F)
    
  }
  
  
  
  ### 종속변수 값 추출
  
  table_y_col<-table_name[, input_y_num]
  
  
  
  
  
  ### 독립변수 값 추출
  
  split_num<-data.frame(strsplit(input_x_num, ','))
  
  names(split_num) <- 'num'
  
  split_num <- as.integer(as.character(split_num$num))
  
  tmp_table <- data.frame(table_name[,split_num])
  
  
  
  final_table <- cbind(lb = table_y_col, tmp_table)
  
  
  
  model <- lm(lb~., data=final_table)
  
  
  
  split_num <- data.table(num=split_num)
  
  if(nrow(split_num) == 1){
    
    names(final_table) <- c('lb','xv')
    
    
    
    input_x_num <- as.integer(input_x_num)
    
    yname <- colnames(table_name[input_y_num])
    
    xname <- colnames(table_name[input_x_num])
    
    
    
    tmp<-round(coef(model),2)
    
    title <- paste('y = ', tmp[2],'* x + ',tmp[1])
    
    
    
    plot(lb~xv, data=final_table, xlab = xname , ylab = yname, col='blue', main=title)
    
    abline(model,col='red')
    
  }
  
  print(model)
  
}



### 회귀트리
reg_tree <- function(){
  
  packages <- c("rattle", "rpart")
  
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))
  }
  
  library(rpart)
  library(rpart.plot)
  
  input_table <- readline('csv파일을 입력하세요. ex) emp.csv : ')
  table_name <- read.csv(input_table, stringsAsFactors = F, header=T)
  
  print(colnames(table_name))
  cat('\n','Data Summary','\n')
  print(head(table_name)[1:3,])
  
  input_header <- readline('컬럼명이 있습니까? ex)T OR F : ')
  input_label <- readline('라벨 컬럼을 입력하세요. ')
  input_x_num <- readline('독립변수의 위치번호를 입력하세요. ex)n OR n,n,n ... (n>=1) : ')
  
  
  ### 헤더 유무 검사
  if (input_header == 'T'){
    table_name <- read.csv(input_table, stringsAsFactors = T, header=T)
  }else {
    table_name <- read.csv(input_table, stringsAsFactors = T, header=F)
  }
  
  ### 종속변수 값 추출
  input_y_num <- which(colnames(table_name) == input_label)
  table_y_col<-table_name[, input_y_num]
  
  ### 독립변수 값 추출
  
  split_num<-data.frame(strsplit(input_x_num, ','))
  names(split_num) <- 'num'
  split_num <- as.integer(as.character(split_num$num))
  tmp_table <- data.frame(table_name[,split_num])
  
  final_table <- cbind(y = table_y_col, tmp_table)
  
  train <- final_table[1:round(0.75*nrow(final_table)), ]
  test <- final_table[round(0.75*nrow(final_table))+1:nrow(final_table), ]
  model <- rpart(y ~., data=train)
  rpart.plot(model, digits=3, fallen.leaves=T, type=2, extra=101)
}



