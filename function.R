machine_learning <- function() {
  ml_cho<-menu(c("knn �˰�����", "���̺� ������","�ǻ� ����Ʈ��","ȸ�� �м�"), title="Do you want this?")
  if (ml_cho == 1){
    #knn_fun()
  }
  else if (ml_cho ==2 ) {
    #naviebayes()
  }
  else if (ml_cho ==3 ) {
    
    cho3<-menu(c("����ȹ�淮�� ����Ϸ��� 1",
                 "�ǻ���� Ʈ���� �ð�ȭ�Ϸ��� 2",
                 "��Ģ ����� ���� �˰������� ����Ϸ��� 3"))
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
    cho3<-menu(c("�ܼ� ���� ȸ�� �м�",
                 "���� ���� ȸ�� �м�",
                 "ȸ��Ʈ�� �׷���"))
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






# 3-1. ����ȹ�淮�� ����Ϸ��� 1

information_fun <- function(){
  packages <- c("FSelector")
  
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))
  }
  
  library(FSelector)
  
  input_table <- readline('csv������ �Է��ϼ���. ex) emp.csv : ')
  table_name <- read.csv(input_table, stringsAsFactors = F, header=T)
  
  print(colnames(table_name))
  cat('\n','Data Summary','\n')
  print(head(table_name)[1:3,])
  
  input_header <- readline('�÷����� �ֽ��ϱ�? ex)T OR F : ')
  input_label <- readline('�� �÷��� �Է��ϼ���. ')
  input_rm_num <- readline('������ �÷��� �ִٸ� �÷� ��ġ��ȣ�� �Է��ϼ���. ex) n,n,n ..., ���� �� 0 : ')
  
  input_label_num <- as.integer(input_label_num)
  
  ### ��� ���� �˻�
  if (input_header == 'T'){
    table_name <- read.csv(input_table, stringsAsFactors = T, header=T)
  }else {
    table_name <- read.csv(input_table, stringsAsFactors = T, header=F)
  }
  
  ### label number ����
  input_label_num <- which(colnames(table_name) == input_label)
  
  ### na�� ����
  table_name <- na.omit(table_name)
  
  ## �� �� ����
  table_label_col<-table_name[, input_label_num]
  
  ### ������ �÷� ����
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
  
  ## ���
  weights <- information.gain( lb ~ ., table2)
  print(weights)
}



# 3-2. �ǻ���� Ʈ���� �ð�ȭ�Ϸ��� 2
decision_fun <- function(){
  packages <- c("rattle", "rpart")
  
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))
  }
  
  library(rattle)
  library(rpart.plot)
  
  input_table <- readline('csv������ �Է��ϼ���. ex) emp.csv : ')
  table_name <- read.csv(input_table, stringsAsFactors = F, header=T)
  
  print(colnames(table_name))
  cat('\n','Data Summary','\n')
  print(head(table_name)[1:3,])
  
  input_header <- readline('�÷����� �ֽ��ϱ�? ex)T OR F : ')
  input_label <- readline('�� �÷��� �Է��ϼ���. ')
  input_rm_num <- readline('������ �÷��� �ִٸ� �÷� ��ġ��ȣ�� �Է��ϼ���. ex) n,n,n ..., ���� �� 0 : ')
  
  
  ### ��� ���� �˻�
  if (input_header == 'T'){
    table_name <- read.csv(input_table, stringsAsFactors = T, header=T)
  }else {
    table_name <- read.csv(input_table, stringsAsFactors = T, header=F)
  }
  
  ### label number ����
  input_label_num <- which(colnames(table_name) == input_label)
  
  ### na�� ����
  table_name <- na.omit(table_name)
  
  ## �� �� ����
  table_label_col<-table_name[, input_label_num]
  
  ### ������ �÷� ����
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
  
  ## ���
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
  
  input_table <- readline('csv������ �Է��ϼ���. ex) emp.csv : ')
  table_name <- read.csv(input_table, stringsAsFactors = F, header=T)
  
  print(colnames(table_name))
  cat('\n','Data Summary','\n')
  print(head(table_name)[1:3,])
  
  input_header <- readline('�÷����� �ֽ��ϱ�? ex)T OR F : ')
  input_label <- readline('�� �÷��� �Է��ϼ���. ')
  input_rm_num <- readline('������ �÷��� �ִٸ� �÷� ��ġ��ȣ�� �Է��ϼ���. ex) n,n,n ..., ���� �� 0 : ')
  
  
  ### ��� ���� �˻�
  if (input_header == 'T'){
    table_name <- read.csv(input_table, stringsAsFactors = T, header=T)
  }else {
    table_name <- read.csv(input_table, stringsAsFactors = T, header=F)
  }
  
  ### label ��ġ
  input_label_num <- which(colnames(table_name) == input_label)
  
  ### na�� ����
  table_name <- na.omit(table_name)
  
  ## �� �� ����
  table_label_col<-table_name[, input_label_num]
  
  ### ������ �÷� ����
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
  ### label ��ġ ������
  input_label_num <- which(colnames(table2) == 'lb')
  
  set.seed(1)
  dim(table2)
  train_cnt <- round(0.75*dim(table2)[1])
  train_index <- sample(1:dim(table2)[1],train_cnt,
                        replace=F)
  train <- table2[train_index, ] 
  test  <- table2[-train_index, ]
  
  
  ## ���
  model <- JRip(lb~., train, na.action=na.pass)
  result <- predict(model, test[, -input_label_num])
  CrossTable(result, test[,input_label_num])
}



multi_reg <- function(){
  
  graphics.off()
  
  
  
  input_table <- readline('csv������ �Է��ϼ���. ex) emp.csv : ')
  
  table_name <- read.csv(input_table, stringsAsFactors = F, header=T)
  
  
  
  print(colnames(table_name))
  
  cat('\n','Data Summary','\n')
  
  print(head(table_name)[1:3,])
  
  
  
  input_header <- readline('�÷����� �ֽ��ϱ�? ex)T OR F : ')
  
  input_y_num <- readline('���Ӻ����� ��ġ��ȣ�� �Է��ϼ���. ex)N (N>=1) : ') 
  
  input_y_num <- as.integer(input_y_num)
  
  
  
  input_x_num <- readline('���������� ��ġ��ȣ�� �Է��ϼ���. ex)n OR n,n,n ... (n>=1) : ')
  
  
  
  ### ��� ���� �˻�
  
  if (input_header == 'T'){
    
    table_name <- read.csv(input_table, stringsAsFactors = F, header=T)  
    
  }else {
    
    table_name <- read.csv(input_table, stringsAsFactors = F, header=F)
    
  }
  
  
  
  ### ���Ӻ��� �� ����
  
  table_y_col<-table_name[, input_y_num]
  
  
  
  
  
  ### �������� �� ����
  
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



### ȸ��Ʈ��
reg_tree <- function(){
  
  packages <- c("rattle", "rpart")
  
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))
  }
  
  library(rpart)
  library(rpart.plot)
  
  input_table <- readline('csv������ �Է��ϼ���. ex) emp.csv : ')
  table_name <- read.csv(input_table, stringsAsFactors = F, header=T)
  
  print(colnames(table_name))
  cat('\n','Data Summary','\n')
  print(head(table_name)[1:3,])
  
  input_header <- readline('�÷����� �ֽ��ϱ�? ex)T OR F : ')
  input_label <- readline('�� �÷��� �Է��ϼ���. ')
  input_x_num <- readline('���������� ��ġ��ȣ�� �Է��ϼ���. ex)n OR n,n,n ... (n>=1) : ')
  
  
  ### ��� ���� �˻�
  if (input_header == 'T'){
    table_name <- read.csv(input_table, stringsAsFactors = T, header=T)
  }else {
    table_name <- read.csv(input_table, stringsAsFactors = T, header=F)
  }
  
  ### ���Ӻ��� �� ����
  input_y_num <- which(colnames(table_name) == input_label)
  table_y_col<-table_name[, input_y_num]
  
  ### �������� �� ����
  
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


