?tibble::rownames_to_column()

#subset
iris |subset( subset=(Sepal.Length)>7)
iris |subset( select=c(1,2))
iris |subset( select=c("Sepal.Length", "Sepal.Width")) |head()


mtcars$mpg
mtcars[[1]]
mtcars[1] |head()
mtcars["mpg"] |head()
mtcars["mpg" |

mtcars[,"mpg",  drop=FALSE] 

mtcars[mtcars$mpg 30 ,]

mtcars[mtcars$cyl == 6,]


mtcars[,1:5] |subset( subset=(mpg) 30)
mtcars |subset( subset=(mpg) 30, select=c(1:5))
mtcars |subset( select = c(1,2)) |head()
mtcars |subset( select=c("mpg", "cyl")) |head()


sample(1:7, size = 10, replace = TRUE)


cut(mtcars$mpg, breaks = 2)
cut(mtcars$mpg, breaks = 3 , labels=c("low","mid","high"))
cut(mtcars$mpg, breaks = c(0,10,30,50) , labels=c("low","mid","high"))

cut()
data(mtcars)
Mtcars <- mtcars
# Engine (0 = V-shaped, 1 = straight)
Mtcars |transform(vs = factor(vs, levels=c(0,1), labels=c("V_shaped"," straight")))


mtcars |summary()

#수식작성 
# https://velog.io/@d2h10s/LaTex-Markdown-%EC%88%98%EC%8B%9D-%EC%9E%91%EC%84%B1%EB%B2%95
&



library(tidyverse)

table1
table3

?transform
"transform is a generic function, which—at least currently—only does anything useful with data frames. transform.default converts its first argument to a data frame if possible and calls transform.data.frame." |jjstat::kakaoi()
"Daily air quality measurements in New York, May to September 1973." |jjstat::kakaoi()




mtcars |
   transform(cyl_reverse = 12- cyl) |>
   subset(select = c(cyl, cyl_reverse)) |# 변수를 선택: 원래변수와 역코딩된 변수만 선택
   head(n= 10)  

airquality |head()
?airquality

transform(airquality, new = -Ozone, New_Temp = (Temp-32)/1.8) |head()

jjstat::comma(1234569)



library(reshape2)
names(airquality) <- tolower(names(airquality))
airquality |head(20)
aqm <- melt(airquality, id=c("month", "day"), na.rm=TRUE)
aqm  |>head(20)



acast(aqm, day ~ month ~ variable)
acast(aqm, month ~ variable, mean)
acast(aqm, month ~ variable, mean, margins = TRUE)
dcast(aqm, month ~ variable, mean, margins = c("month", "variable"))

#Chick weight example
ChickWeight 
ChickWeight |head(20)
names(ChickWeight) <- tolower(names(ChickWeight))

# weight를 ongformat으로 변경 
chick_m <- melt(ChickWeight, id=2:4, na.rm=TRUE)
chick_m |head(10)
dcast(chick_m, time+ chick + diet ~ variable) 

# dcast(
#   data,
#   formula,
#   fun.aggregate = NULL,
#   ...,
#   margins = NULL,
#   subset = NULL,
#   fill = NULL,
#   drop = TRUE,
#   value.var = guess_value(data)
# )


dcast(chick_m, time ~ variable, mean) # average effect of time
dcast(chick_m, diet ~ variable, mean) # average effect of diet
acast(chick_m, diet ~ time, mean) # average effect of diet & time

acast(chick_m, time ~ variable, mean) # average effect of time
acast(chick_m, diet ~ variable, mean) # average effect of diet
acast(chick_m, diet ~ time, mean) # average effect of diet & time

# How many chicks at each time? - checking for balance
acast(chick_m, time ~ diet, length)
acast(chick_m, chick ~ time, mean)
acast(chick_m, chick ~ time, mean, subset = .(time < 10 & chick < 20))
acast(chick_m, time ~ diet, length)
dcast(chick_m, diet + chick ~ time)
acast(chick_m, diet + chick ~ time)
acast(chick_m, chick ~ time ~ diet)
acast(chick_m, diet + chick ~ time, length, margins="diet")
acast(chick_m, diet + chick ~ time, length, drop = FALSE)



ChickWeight 
ChickWeight |str()
names(ChickWeight) <- tolower(names(ChickWeight))

# weight를 ongformat으로 변경 
chick_m <- melt(ChickWeight, id=2:4, na.rm=TRUE)
chick_m |head(10)
dcast(chick_m, time+ chick + diet ~ variable) 

# dcast(

?dcast

set.seed(20240226)
# df <- data.frame(id = 1:12,
#                  time = rep(1:3,4, each=TRUE),
#                  type = rep(c("mid","fin"), 6, each=TRUE),
#                  score = sample(1:7, 12, replace=TRUE)
#                  )

# df


set.seed(20240226)
df <- data.frame(id = 1:12,
                 type = rep(c("mid","fin"), 6, each=TRUE),
                 y1 = sample(1:7, 12, replace=TRUE),
                 y2 = sample(1:7, 12, replace=TRUE),
                 y3 = sample(1:7, 12, replace=TRUE)
                 )

df


?reshape2::melt
## S3 method for class 'data.frame'
# melt(
#   data,
#   id.vars,
#   measure.vars,
#   variable.name = "variable",
#   ...,
#   na.rm = FALSE,
#   value.name = "value",
#   factorsAsStrings = TRUE
# )


##############################################################################
reshape2::melt(df, id.vars = "id")
reshape2::melt(df, id.vars = c("id","type"))

reshape2::melt(df, id.vars = c("id","type"), variable.name="Var", value.name="score") 

df_long <- reshape2::melt(df, id.vars = c("id","type")) 

df_long

dcast(df_long, id + type ~ variable)
dcast(df_long, id + type ~ variable, value.var = "value")

df_long




####
# numeric변수를 찾아서 round처리 
jjstat::mysummaryBy(value ~ type, data = df_long )[,1:2] 
#   mutate_if(is.numeric, round, 2)

# jjstat::mysummaryBy(value ~ type, data = df_long )[,1:2] |>
#   sapply(function(x){if(is.numeric(x)){round(x, 2)}else{x}}) |>data.frame()

# jjstat::mysummaryBy(value ~ type, data = df_long )[,1:2] |>
#   map_dfr(~ if(is.numeric(.x)) round(.x, 2))


# ddf = jjstat::mysummaryBy(value ~ type, data = df_long )[,1:2]
# ddf
# ddf|dplyr::mutate_if(is.numeric, round, 2)

# ddf|sapply(function(x){if(is.numeric(x)){round(x, 2)}else{x}}) |>
#   data.frame()

# ddf|>
#   map_dfr(~ if(is.numeric(.x)) round(.x, 2))

# for(i in colnames(ddf)){
#     if(is.numeric(ddf[[i]])){
#         ddf[[i]] <- round(ddf[[i]], 2)
#     }
# }
# ddf

#집계함수 
aggregate(value ~ type, data = df_long, mean)


dcast(df_long, type ~ 1, mean)
dcast(df_long, variable ~ 1, mean)



dcast(df_long, variable ~ type, mean) |round(2)
#####################################################
dcast(df_long, variable ~ type, mean) |
dplyr::mutate_if(is.numeric, round, 2)

dcast(df_long, variable ~ type, mean) |
 sapply(function(x){if(is.numeric(x)){round(x, 2)}else{x}}) |>
  data.frame()

dcast(df_long, variable ~ type, mean) |
   purrr::map_dfr(~ if(is.numeric(.x)) round(.x, 2) else(.x))

ddf= dcast(df_long, variable ~ type, mean)
for(i in colnames(ddf)){
    if(is.numeric(ddf[[i]])){
        ddf[[i]] <- round(ddf[[i]], 2)
    }
}
ddf

?map_dfr



dcast(df_long, type ~ variable, mean)


dcast(df_long, value ~ type, mean)
dcast(df_long, type ~ value, mean)

sapply(df_long$value, mean)


print(df_wide)

# 결과
#   id time value
# 1   1    1   10
# 2   2    2   20
# 3   3    3   30


setwd("F:/Rwork/lecture3/Rmluti")
# save(df, file = "df.RData")
load(file = "df.RData")

##################################################################
set.seed(20240226)
df <- data.frame(id = 1:12,
                 type = rep(c("mid","fin"), 6, each=TRUE),
                 y1 = sample(1:7, 12, replace=TRUE),
                 y2 = sample(1:7, 12, replace=TRUE),
                 y3 = sample(1:7, 12, replace=TRUE)
                 )

df

df_long
df_long <- reshape2::melt(df, id.vars = c("id","type")) 
reshape2::melt(df, id.vars = c("id","type"))
# ?reshape
# reshape(df, idvar = c("id","time"), 
#         v.name = "variable", 
#         direction ="long")

#stats::reshape에서 만든 자료가 아닌 경우  간단한 저적용이 안된다. 
reshape(df_long, 
        direction ="wide" )


reshape(df_long1, 
        idvar=c("id","type"), #유지될 데이터 
        timevar="variable", #column이 되는 변수 
        direction ="wide" )

reshape(df_long1, idvar=c("id","type"), #유지될 데이터 
        timevar="variable",#column이 되는 변수 
        varying=list(c("y1","y2","y3")), #column의 정확한 변수명
        direction ="wide" )

#stats::reshape를 이용한 방법 
df
df_long1 = reshape(df, 
        idvar=c("id", "type"), #유지할 데이터
        varying = list(3:5), # value가 될 변수 
        timevar = "variable",  #value가 된 column
        v.name = "score", # value의 명칭
        direction ='long')     
df_long1

reshape(df_long1, 
        direction ="wide" )


?reshape

df
?gather

gather(df,
         key = "variable", 
         value= "score", y1, y2, y3) 
gather(df,
         key = "variable", 
         value= "score", -id, -type) 

?spread

# spread(df_long, key="variable", value ="value")

tidyr::spread(df_long1, 
        key = "variable", #열(column)으로 돌아갈 값들 
        value = "score")  #각 열의 값이 되어야 하는 부분

tidyr::spread(df_long, 
        key = "variable", #열(column)으로 돌아갈 값들 
        value = "value")  #각 열의 값이 되어야 하는 부분

table4a


## times need not be numeric
dfdf <- data.frame(id = rep(1:4, rep(2,4)),
                 visit = I(rep(c("Before","After"), 4)),
                 x = rnorm(4), y = runif(4))
dfdf
reshape(dfdf, idvar=c("id","visit"), varying =list(3:4),
        v.name="score",direction ="long")|>
        reshape(direction="wide")



?pivot_longer
df_long3 = pivot_longer(df, 
            cols = y1:y3,
            names_to="variable", 
            values_to="score")

df_long3
?pivot_wider
pivot_wider(df_long3, 
            names_from="variable", #열이 될 이름 
            values_from="score")  #열변수의 값들 


# 데이터 생성
set.seed(123)
data <- rnorm(100) 
data
# 샤피로 윌크 검정 수행
shapiro.test(data) |tidy()

shapirotest_df = function(data){

   for(i in ncol(data)){

   } 
}

mtcars[,c("mpg","disp","hp","drat","wt","qsec")]

##########################copilot 
# Custom function to perform Shapiro-Wilk test on all columns of a data frame
shapiro_test_all<- function(data, digits=3) {
  # Initialize an empty data frame to store results
  result_df <- data.frame(Column = character(0), 
                    W = numeric(0), 
                    p_value = numeric(0), 
                    stringsAsFactors = FALSE)
  
  # Loop through each column in the data frame
  for (col in colnames(data)) {
    # Perform Shapiro-Wilk test
    test_result <- shapiro.test(data[[col]])
    
    # Extract test statistics
    W <- test_result$statistic
    p_value <- test_result$p.value
    
    # Append results to the data frame
    result_df <- rbind(result_df, 
                    data.frame(
                    variable = col, 
                    W = W, 
                    p.value = p_value))|>
                    dplyr::mutate_if(is.numeric, round, digits)
  }

 
 result_df<- result_df|
            transform(hypotheses= ifelse(p.value >0.05, 
                                "Normality * ", "reject")) 
  result_df
}
# Apply the custom function
shapiro_test_all(data)


# data.frame(Column = character(0), 
#                     W = numeric(0), 
#                     p_value = numeric(0), 
#                     stringsAsFactors = FALSE)
# Example usage with the given data
data <- data.frame(
  mpg = c(21.0, 21.0, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8, 19.2),
  disp = c(160.0, 160.0, 108.0, 258.0, 360.0, 225.0, 360.0, 146.7, 140.8, 167.6),
  hp = c(110, 110, 93, 110, 175, 105, 245, 62, 95, 123),
  drat = c(3.90, 3.90, 3.85, 3.08, 3.15, 2.76, 3.21, 3.69, 3.92, 3.92),
  wt = c(2.620, 2.875, 2.320, 3.215, 3.440, 3.460, 3.570, 3.190, 3.150, 3.440),
  qsec = c(16.46, 17.02, 18.61, 19.44, 17.02, 20.22, 15.84, 20.00, 22.90, 18.30)
)
data



###########################
library(tidyverse)

?rbindlist

shapiro_test_df <- function(data, digits = 3) {
    # Calculate Shapiro-Wilke test results for all columns
  # Shapiro-Wilke test results for each column
  results <- lapply(data, shapiro.test)
  
  variable = names(data)
  df = rbindlist(results)[,1:2]
  res = cbind(variable, df)|>
                    dplyr::mutate_if(is.numeric, round, digits)
                    
 res = res|transform(hypotheses= ifelse(p.value >0.05, "Normality * ", "reject")) 
  # 
  return(res)
}
data <- mtcars[,c("mpg","disp","hp","drat","wt","qsec")]
# Shapiro-Wilke Test Results for All Columns
 jjstat::shapiro_test_df(data)

mtcars[,c("mpg","disp","hp","drat","wt","qsec")]|>
   jjstat::shapiro_test_df()
  
  
  jjstat::p_mark_sig("p.value")
# 예시 데이터



 ToothGrowth%>%group_by(dose) %>%
  rstatix::t_test(data =., len ~ supp)%>%
  jjstat::p_mark_sig("p")
# 결과 출력
print(results)
data <- mtcars[,c("mpg","disp","hp","drat","wt","qsec")]

lapply(data, shapiro.test)|data.table::rbindlist()
(lapply(data, shapiro.test)|do.call(what=rbind) |>data.frame())[,1:2]

(do.call(rbind, lapply(data, shapiro.test))|>data.frame())[,1:2]

lapply(data, shapiro.test) |sapply(rbind)

?do.call



library(data.table)
detach(package:data.table)




# 예제 데이터 (임의로 생성)
ksdata<- c(6, 1, -4, 8, -2, 5, 0)
mean(ksdata); sd(ksdata)

# Kolmogorov-Smirnov 검정 수행
ks.test(ksdata, "pnorm", mean = mean(ksdata), sd = sd(ksdata))
ks.test(ksdata, "pnorm")


ks.test(as.numeric(mtcars$mpg), "norm")
ks.test(mtcars$mpg, "pnorm")
# ks.test(mtcars$mpg, "pnorm", mean(mtcars$mpg), sd(mtcars$mpg))
ks.test(mtcars$mpg, y)

x <- rnorm(50)
y <- runif(30)
# Do x and y come from the same distribution?
ks.test(x, y)
ks.test(x, "pnorm")

# install.packages("nortest")
library(nortest)
nortest::ad.test(mtcars$mpg)
nortest::ad.test(ksdata)
nortest::ad.test(c(6, 1, -4, 8, -2, 5,1,3))
x11()
hist(c(6, 1, -4, 8, -2, 5, 1, 3))
# dentisy(c(6, 1, -4, 8, -2, 5, 1, 3))


# 예제 데이터 (임의로 생성)
data <- c(6, 1, -4, 8, -2, 5, 0)

# Q-Q Plot 그리기
qqnorm(ksdata)  # 점 형태로 그리기
qqline(ksdata, col = 2)  # 근사선 그리기

####################
women



women | psych::alpha()
women |jjstat::alpha_table(variable="element")


# data()
library(tidyverse)

# https://psyteachr.github.io/index.html#category=dataviz

disgust <- read_csv("https://psyteachr.github.io/msc-data-skills/data/disgust.csv")
disgust
disgust |str()

disgust %>%
  select(moral1:moral7) %>%
  psych::alpha(title = "moral")

load("F:/Rwork/SEM212/SEM212_new/data/stat_onl.rda")
jjstat::stat_onl|>str()


data(jjstat::stat_onl)




stat_onl |>
  dplyr::select(upgrade, satisfy, fatigue) |     
        psych::alpha()|print(digits = 3)
stat_onl |>
  dplyr::select(upgrade, satisfy, fatigue) |     
        psych::alpha()|>
        summary() 

stat_onl |>
  dplyr::select(upgrade, satisfy, fatigue) |     
        psych::alpha(check.keys=TRUE)|>
        summary()
#check.keys=TRUE을 적용한 경우 
jjstat::stat_onl |>
  dplyr::select(upgrade, satisfy, fatigue) |     
        psych::alpha(check.keys=TRUE)

#fatiguefmf 제거(drop)한 후 분석
jjstat::stat_onl |>
  dplyr::select(upgrade, satisfy) |     
        psych::alpha(check.keys=TRUE) |>
          summary() 



jjstat::stat_onl |>
  dplyr::select(upgrade, satisfy, fatigue_inverse)|     
        psych::alpha( )|>print(digits=3)

stat_onl |>
  dplyr::select(upgrade, satisfy, fatigue_inverse)|
  jjstat::alpha_table() |knitr::kable("markdown")


  jjstat::markdown_table("신뢰도분석")



#역코딩 함수 #############################
rev_coding = function(vec, 
                     n = 6, 
                     check = FALSE){
if(is.vector(vec)){
  if(max(vec) < n){
     vec_r = n - vec
     }else{
      vec_r = "Please check the value again. It does not match the value to be decoded. Please check the scale again. For example, enter n=6 for 1~5 Likert and n=8 for 1~7 Likert"
         }
     }else{
       vec_r = "You can only input vectors for inverse coding. If you have entered a data frame, please find the variable and enter it correctly."
     }
if(check){
   res = data.frame(input= vec, direct="->",reverse= vec_r)
       }else{
        res = vec_r  
           }    
      res
      }
## error  :Error message output when data frames are entered     
rev_coding(mtcars)


jjstat::rev_coding(mtcars$gear)
jjstat::rev_coding(mtcars$gear, check=TRUE)
rev_coding(mtcars$cyl)

rvc

data(mtcars)
mtcars
mtcars  |is.vector()


  # mtcars$gear
# max(mtcars$gear)
# max(mtcars$cyl)

##################

# Custom function to perform reverse coding for multiple variables
revcode_one <- function(data, n, ...) {
  # Arguments:
  #   data: Data frame containing the variables
  #   n: Maximum scale value (e.g., 8 for Likert 7-point scale)
  #   ...: Variable names to be reverse coded
  
  # Loop through each variable
  for (var in c(...)) {
    # Reverse code the variable
    data[[paste0(var, "_r")]] <- n - data[[var]]
  }
  
  return(data)
}

# Example usage:
# Suppose 'data' is your data frame containing 'y1', 'y2', ..., 'y5'
# result_data <- revcode(data, n = 8, y1, y2, y3, y4, y5)

revcode(stat_onl,8, "upgrade", "satisfy", "fatigue" )|>
select(upgrade, satisfy, fatigue, upgrade_r, satisfy_r, fatigue_r)


seq_along(c("upgrade", "satisfy", "fatigue"))

## jjstat package에서 사용하는 방법 
# Custom function to perform reverse coding for multiple variables with custom n values
revcode <- function(data, n = 8, ..., postfix="_r") {
  # Arguments:
  #   data: Data frame containing the variables
  #   n: Vector of maximum scale values (one for each variable)
  #   ...: Variable names to be reverse coded
  if(length(n)==1){
    # Loop through each variable
    for (var in c(...)) {
      # Reverse code the variable
      data[[paste0(var, postfix)]] <- n - data[[var]]
    }
  }else{
    # Loop through each variable
    for (i in seq_along(c(...))) {
      var <- c(...)[i]
      n_val <- n[i]

      # Reverse code the variable
      data[[paste0(var, postfix)]] <- n_val - data[[var]]
    }

  }
  return(data)
}


# Example usage:
# Suppose 'data' is your data frame containing 'y1', 'y2', ..., 'y5'
# n_values <- c(8, 7, 8, 8, 7)  # Custom n values for each variable
# result_data_custom <- revcode(data, n = n_values, y1, y2, y3, y4, y5)

mtcars
revcode(stat_onl, c(8,8,8), "upgrade", "satisfy", "fatigue" )|>
select(upgrade, satisfy, fatigue, upgrade_r, satisfy_r, fatigue_r)

library(jjstat)
library(knitr)
stat_onl|str()
stat_onl|head()
revcode(stat_onl, 8, "upgrade", "satisfy", "fatigue","S_Add_learn")|>
select(upgrade, satisfy, fatigue, upgrade_r, satisfy_r, fatigue_r, S_Add_learn_r)|>
head()


revcode(mtcars[,c("cyl", "gear","carb")],12,"cyl")

jjstat::revcode(mtcars[,c("am","vs")],n = 1,"vs","am")

revcode(mtcars[,c("cyl", "gear","carb")],c(12, 6, 9),"cyl","gear","carb")


jjstat::revcode(mtcars[,c("cyl", "gear","carb")],c(12, 6, 9),"cyl","gear","carb")

stat_onl|>colnames()
?jjstat::alpha_table


dplyr::bind_rows(
stat_onl %>% select(S_Review, S_Add_learn, S_Feedback) %>%
  jjstat::alpha_table(show="data", variable = "A" ),
stat_onl %>% select(SE_Place, SE_Time) %>%
  jjstat::alpha_table(show="data", variable = "B"),
stat_onl %>% select(On_Satisfy, On_Joy, On_Easy, On_Engage) %>%
  jjstat::alpha_table(show="data", variable = "C"),
stat_onl %>% select(upgrade,satisfy,fatigue_inverse) %>%
 jjstat::alpha_table(show="data", variable = "D")
 ) |markdown_table(digits=3, font_size = 12,
                     caption= "total Cronbach alpha")

list(
stat_onl %>% select(S_Review, S_Add_learn, S_Feedback) %>%
  jjstat::alpha_table(show="data", variable = "A" ),
stat_onl %>% select(SE_Place, SE_Time) %>%
  jjstat::alpha_table(show="data", variable = "B"),
stat_onl %>% select(On_Satisfy, On_Joy, On_Easy, On_Engage) %>%
  jjstat::alpha_table(show="data", variable = "C"),
stat_onl %>% select(upgrade,satisfy,fatigue_inverse) %>%
 jjstat::alpha_table(show="data", variable = "D")
 ) | data.table::rbindlist()

load("F:/Rwork/SEM212/SEM212_new/data/stat_onl.rda")
stat_onl <- jjstat::stat_onl
data(jjstat::stat_onl)



bind_alpha_table = function(data, ...){
  # Arguments:
  #   data: Data frame containing the variables
  #   ...: Variable names to be included in the alpha table
 form = list(...)

  if(length(form)==1){
    result = jjstat::alpha_table( subset(data, select = form[[1]]),
    #  show="data", variable=paste("factor_",1 ) )
     show="data", variable = letters[1] )
      
  }else if(length(form) 1 ){
     result = jjstat::alpha_table(subset(data,select = form[[1]]),
    #  show="data" ,variable=paste("factor_",1 ))
     show="data" ,variable = letters[1])
    for (i in 2:length(form)) {
      result <- rbind(result,
               jjstat::alpha_table(subset(data, select = form[[i]]),
               show="data",
              #  variable = paste("factor_", i ) ))
               variable = letters[i] ))
                              }
                  } 

result
}

stat_onl <- jjstat::stat_onl

## select variable 
jjstat::bind_alpha_table(data = stat_onl, c("S_Review", "S_Add_learn", "S_Feedback"))
#error 
bind_alpha_table(data = stat_onl, c(S_Review, S_Add_learn, S_Feedback))

#select col number 
bind_alpha_table(data = stat_onl, c(9:11))

# multivariate data
jjstat::bind_alpha_table( stat_onl, 
        c("S_Review", "S_Add_learn", "S_Feedback"),
        c("SE_Place", "SE_Time"),
        c("On_Satisfy", "On_Joy", "On_Easy", "On_Engage"),
        c("upgrade","satisfy","fatigue_inverse")
)

letters
# Multivariate data, and the column number and variable name are mixed
jjstat::bind_alpha_table( stat_onl, 
        c("S_Review", "S_Add_learn", "S_Feedback"),
        c("SE_Place", "SE_Time"),
        c("On_Satisfy", "On_Joy", "On_Easy", "On_Engage"),
        c(5:6, 21)
)|>kable("markdown")

# stat_onl |>ncol()
# subset(stat_onl, select = c("S_Review"))
# subset(stat_onl, select = 9)
# subset(stat_onl, select = c(S_Review, S_Add_learn))
jjstat::bind_alpha_table( stat_onl, 
        c("S_Review", "S_Add_learn", "S_Feedback"),
        c("SE_Place", "SE_Time"),
        c("On_Satisfy", "On_Joy", "On_Easy", "On_Engage"),
        c(5:6, 21)
)|knitr::kable(align ="cccc", digits = 3, booktabs = TRUE)


# ?subset
jjstat::bind_alpha_table( stat_onl, 
        c("S_Review", "S_Add_learn", "S_Feedback"),
        c("SE_Place", "SE_Time"),
        c("On_Satisfy", "On_Joy", "On_Easy", "On_Engage"),
        c(5:6, 21)
)|knitr::kable(align ="cccc", digits = 3) %>%
  kableExtra::pack_rows(
    index = c("MetaCog"= 2, "MetaEnv" = 6, "Online satisfy" = 8, "Use satisfy" = 11)
  )



final <-  c(19, 22, 24, 24, 25, 25, 26, 26, 28, 32)
t.test(final, mu=24, alternative="greater", conf.level = .95) |>
report::report() |jjstat::e()

jjstat::markdown_table()

?kableExtra::pack_rows

#이분형 변수인 경우 신뢰도 #################################################
# install.packages("mokken")
library(mokken)
mtcars[,c("am","vs")]|class()

mokken::coefH( mtcars[,c("am","vs")])

mokken::coefH( mtcars[,c("am","vs")], results = FALSE)$H

psych::alpha(mtcars[,c("am","vs")])|summary()

mtcars[,c("am","vs")] |tibble()
# data(acl)
# Communality <- acl[, 1:10]
# Communality |head()
# # Compute scalability coefficients and standard errors
# coefH(Communality[,1:3],results=T)
# coefH(Communality[,1:3],results=F)$H

"Mokken scale analysis (Mokken, 1971; Sijtsma & Molenaar, 2002; Sijtsma & Van der Ark, 2017) is a scaling procedure for both dichotomous and polytomous items. It consists of an item selection algorithm to partition a set of items into Mokken scales and several methods to check the assumptions of two nonparametric item response theory models: the monotone homogeneity model and the double monotonicity model. The output of this R-package resembles the output of the stand-alone program MSP (Molenaar & Sijtsma, 2000)."|>jjstat::e()



sats<- jjstat::sats
sjPlot::view_df(jjstat::sats)

##########################################

final <-  c(19, 22, 24, 24, 25, 25, 26, 26, 28, 32)
shapiro.test(final) |broom::tidy()
t.test(final, mu=24, alternative="greater", conf.level = .95) |broom::tidy()

##########################################
report_stat <- function(data, type="normality",
                        digits = 3,
                        trans=FALSE, show="data") {
  # Extract relevant information from the data
  data = broom::tidy(data)
  #   #
  if(type=="leven.test"){
    data<- data |dplyr::mutate(method="Leven test")
    # data <- data |transform(method="Leven test")
  }else{
    data
  }

  method <- data$method
  statistic <- data$statistic
  p_value <- data$p.value
  if(p_value < 0.001){p_value_res <- "< .001"
  }else{
    p_value_res <- paste0("= ", round(data$p.value, digits))}
  # Determine the significance based on p-value

  if(type=="t.test"){
    significance <- ifelse(p_value < 0.05, "It was statistically significant t = ", "It was not statistically significant. t = ")

  }else if(type =="normality"){
    significance <- ifelse(p_value < 0.05, "Statistically significant, the null hypothesis was rejected and normality was not secured.  statistic = ",
                           "the normality was secured by rejecting the alternative hypothesis because it was not statistically significant. statistic = ")

  }else if(type =="var.test"| type=="leven.test"){
    significance <- ifelse(p_value < 0.05, "Statistically significant, the null hypothesis is rejected, so equidistribution can not be assumed.  F = ",
                           "It was not statistically significant, and the null hypothesis can be adopted to assume equal variance. F = ")
  }
  # Create the sentence based on the method and p-value
  result_sentence <- paste0( method," showed that ",
                             significance,
                             #  "(stat = ",
                             #  statistic,
                             round(statistic ,digits),
                             ", p ",
                             p_value_res, ")")
  #  p_value , ")")

  # Print the result
  if(trans){
    jjstat::kakaoi(result_sentence,"en","ko", show=show)
  }else{
    result_sentence
  }

}





shapiro.test(final) |report_stat() |>kakaoi()
shapiro.test(final) |report_stat(trans=TRUE) 
shapiro.test(final) |report_stat(trans=FALSE) 
shapiro.test(final) |report_stat()|jjstat::k("en","ko")

t.test(final, mu=24, alternative="greater", conf.level = .95) |>
report_stat("t.test") 

t.test(mpg ~ vs, data= mtcars, var.equal = FALSE)|>broom::tidy()
t.test(mpg ~ vs, data= mtcars, var.equal = FALSE)|>report_stat("t.test") 

t.test(final, mu=24, alternative="greater", conf.level = .95) |>
report_stat("t.test") |jjstat::e()


var.test(mpg ~ vs, data = mtcars)|report_stat("var.test")
var.test(mpg ~ vs, data = mtcars)|report::report()

car::leveneTest(mpg ~ factor(vs), mtcars) |>broom::tidy() |>is.null(method)

car::leveneTest(mpg ~ factor(vs), mtcars) |>broom::tidy()|>
mutate(method="Leven test")
car::leveneTest(mpg~ factor(vs), mtcars)|report_stat("leven.test")
car::leveneTest(mpg~ factor(vs), mtcars)|report_stat("leven.test", trans=TRUE)


fligner.test(mpg ~ vs,data= mtcars) |>broom::tidy()
fligner.test(mpg ~ vs,data= mtcars) |report_stat("t.test")
fligner.test(mpg ~ vs,data= mtcars) ||report::report()
t.test(len ~ supp, data = ToothGrowth, var.equal=TRUE) |report_stat("t.test")
t.test(len ~ supp, data = ToothGrowth, var.equal=TRUE) |report::report()


fuel = data.frame(
                trt = c(1L,1L,1L,1L,1L,1L,2L,
                        2L,2L,2L,2L,2L,3L,3L,3L,3L,3L,3L,4L,4L,4L,
                        4L,4L,4L),
                  y = c(64L,72L,68L,77L,56L,95L,
                        78L,91L,97L,82L,85L,77L,75L,93L,78L,71L,63L,
                        76L,55L,66L,49L,64L,70L,68L)
                )

aov(y~ trt, data=fuel)|>broom::tidy()
aov(y~ trt, data=fuel)|>broom::tidy()|report_stat("aov")

  # if(is.null(grepl(":", term[[i]]))){
  #    significance <- ifelse(p_value[i] < 0.05, 
  #   "It was statistically significant", 
  #   "It was not statistically significant")
  # }else{
  #      significance <- ifelse(p_value[i] < 0.05, 
  #   "The interaction effect was statistically significant.",
  #   "The interaction effect was not statistically significant.")
  # }


report_aov <- function(data, digits = 2, trans=FALSE) {
 data0 = data
 dv = as.character(data$call[[2]][2])
 iv = as.character(data$call[[2]][3])
 Data = broom::tidy(data0)
 summary_data = summary(data0)

  # Extract relevant information from the data
  term <- Data$term
  df <- Data$df
  statistic <- Data$statistic

  p_value <- Data$p.value

  # Create the sentence based on the method and p-value
  result_sentence = list()
  for( i in 1: nrow(Data)-1){
      # Determine the significance based on p-value
 significance <- ifelse(p_value[i] < 0.05, 
    "It was statistically significant", 
    "It was not statistically significant")

  result_sentence[i] <- paste0("one-way analysis of variance result for dv[", 
  dv,"]~[",term[i],"], ", 
      significance, "(F (", 
      df[i], 
      ", ", 
      df[nrow(Data)], 
      ") = ", 
      round(statistic[i], digits),
      ", p = ", 
      round(p_value[i], digits), 
      "). \n")
  #  significance = rbind(significance,)
  }
  # Print the result
res = c(do.call(rbind, result_sentence))

if(trans){
 cat( jjstat::k(res,"en","ko")[2] ,  "\n")
}else{
cat(res ,"\n")
  }
}

# Example usage
# aov(y~ trt, data=fuel)|broom::tidy()
aov(y~ trt, data=fuel)|>report_aov() |>class()
aov(y~ trt, data=fuel)|>report_aov()
aov(y~ trt, data=fuel)|>report_aov(trans=T)

aov(len ~ supp*factor(dose), data= ToothGrowth)|>report_aov(trans=F)
aov(len ~ supp*factor(dose), data= ToothGrowth)|>report_aov(trans=T)

aov(len ~ supp*factor(dose), data= ToothGrowth)|>report_aov()|kakaoi("en","ko")

aov(len ~ supp*factor(dose), data= ToothGrowth)|>report_aov()|>gsub("queryLanguage=en","")


aov(len ~ supp*factor(dose), data= ToothGrowth)|>report::report()

ToothGrowth |str()

lens = aov(len ~ supp, data= ToothGrowth)
lens|str()
as.character(lens$call[[2]][2])
as.character(lens$call[[2]][3])

aov(len ~ supp, data= ToothGrowth)|>broom::tidy()

aov(len ~ supp, data= ToothGrowth)|>report_aov()

aov(len ~ factor(dose), data= ToothGrowth)|>broom::tidy()

aov(len ~ factor(dose), data= ToothGrowth)|>report_aov()

aov(len ~ supp*factor(dose), data= ToothGrowth)|>broom::tidy() #
aov(len ~ supp*factor(dose), data= ToothGrowth)|>summary() #

aov(len ~ supp*factor(dose), data= ToothGrowth)|>report_aov()|>kakaoi()|>cat()

# grep(":","dfadas:Dfadfa", value=T)
grep(":","dfadas:Dfadfa")
grepl(":","dfadas:Dfadfa")

grep(":", aov(len ~ supp*factor(dose), data= ToothGrowth)|broom::tidy()|>
select(term)|>slice(1) 
)


sats<-jjstat::sats
sats|>str()
aov(sux1 ~ grade*onoff*gender, sats) |report_aov(trans=TRUE)



kakaoi = function (text="",
              slang="en",
              elang="ko",
              show ="data") {
  library("httr")
  library("httpuv")
  response <- POST("https://translate.kakao.com/translator/translate.json",
                   body=sprintf("queryLanguage=%s&resultLanguage=%s&q=%s",
                                slang,
                                elang,
                                encodeURIComponent(text)),
                   add_headers(.headers=c("Referer"="https://translate.kakao.com/",
                                          "content-type"="application/x-www-form-urlencoded; charset=UTF-8"))
  )
  data = content(response, "parsed")
  output = data$result$output

  #데이터를 list에서 character로 만들기
  data_result_input = unlist(data$result$input)
  data_result_output = unlist(data$result$output)
  #결과 정리
  resout = paste(data_result_output)
  resin = paste(data_result_input)

  if(show ==" normal"){
    cat("Source language: \n\n",resin,"\n\n")
    cat("Translate language: \n\n", resout)
    #  gsub("queryLanguage=en,","",resout)

  }else if(show == "data"){
    # res = list(source = resin,
    #           translate = resout)
    res = resout
    cat( "\n\n", res, "\n\n")
  }

}

gsub("queryLanguage=en,","", "queryLanguage=en, dv[len]~[factor(dose)]에 대한 일원변량분석 결과, 통계적으로 유의하였다(F(2, 54)=92, p=0).")



 var.test(mpg ~ vs, data = mtcars)|>broom::tidy()
 var.test(mpg ~ vs, data = mtcars)|>
  jjstat::report_stat("var.test", trans= TRUE, show="data")




set.seed(123)
data <- rnorm(100)
data 
data.frame(data)

histogram = function(data, color="gray60"){
  library(tidyverse)
data = data.frame(data)
data = data|tibble::rowid_to_column()

ggplot2::ggplot(data, ggplot2::aes(x = data)) +
  geom_histogram(aes(y = ..density..), alpha = 0.4,
   binwidth = 0.2, color = "black", fill = color) +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Histogram with Overlay Density",
       x = "Values", 
       y = "Density") +
  theme_minimal()
}

histogram(data )

# 데이터 생성 (표준 정규 분포를 따르는 데이터)
set.seed(123)
data <- rnorm(100)

hist2 = function(data,
                 color = "lightblue",
                 linecolor = "red"){
# 히스토그램 그리기
hist(data, probability = TRUE, 
    col = color, 
    main = "Histogram with Overlay Density")
# 밀도 그래프 그리기
lines(density(data), col = linecolor, lwd = 2)
# 범례 추가
legend("topright", legend = c("Histogram", "Density"),
         fill = c(color, linecolor))
}


hist2(data)




#' principal component factor analysis function
#'
#' @param R correlation matrix
#' @param nfactor number of factor
#' @param rowname matrix rowname
#' @param name 'auto', 'manual'
#' @param digits digits = 3
#' @param xlim  c(-1,1)
#' @param ylim c(-1,1)
#' @param cex  size =1
#' @param pos  positon  1
#'
#' @return result table, biplot, dimension
#' @export
#'
#' @examples
#' \dontrun{
#' R= matrix(
#' c(1.00, 0.83, 0.78, 0.70, 0.66, 0.63,
#'   0.83, 1.00, 0.67, 0.67, 0.65, 0.57,
#'   0.78, 0.67, 1.00, 0.64, 0.54, 0.51,
#'   0.70, 0.67, 0.64, 1.00, 0.45, 0.51,
#'   0.66, 0.65, 0.54, 0.45, 1.00, 0.40,
#'   0.63, 0.57, 0.51, 0.51, 0.40,1.00),
#'  byrow=T, nrow=6)
#' subject=c("classic","france","english","math","pitch","music")
#'
#' colnames(R)=subject
#' rownames(R)=subject
#' R
#' pcfa(R, nfactor = 2)
#'
#' }
#'
pcfa <- function(R,
                 nfactor=NULL,
                 rowname=NULL,
                 name="auto",
                 digits=2,
                 xlim = c(-1,1),
                 ylim = c(-1,1),
                 cex=1,
                 pos=1){

  # library(tidyverse)

  if(nfactor==1){
    return(cat("다시 입력하세요 2이상으로 하세요"))

  }else if(nfactor>=2){


    eigen.R <- eigen(R)
    V = eigen.R$vectors
    gof = eigen.R$values/sum(eigen.R$values)*100

    # visualization
    Gof.c <- gof %>% data.frame()
    colnames(Gof.c)="Value"

    Gof.c <- Gof.c%>%
      mutate(eig.prop= paste0("Dim",1: ncol(R))) %>%
      select(eig.prop, Value)



    E = diag(sqrt(eigen.R$values[1:nfactor]))

    # main component
    V_main = V[, 1:nfactor]
    colnames(V_main)= paste0("Dim",1: nfactor)

    if(name=="auto"){
      rownames(V_main)= rownames(R)}
    if(name=="manual"){
      rownames(V_main)= rowname}

    # calculation main component
    LoadingMatrix = V_main %*% E
    colnames(LoadingMatrix)=paste0("PC",1:nfactor)
    if(name=="auto"){
      rownames(LoadingMatrix)= rownames(R)}
    if(name=="manual"){
      rownames(LoadingMatrix)= rowname}

    # LoadingMatrix %>% round(digits)


    # communality
    communality= LoadingMatrix%*%t(LoadingMatrix)

    # specific variance :PSi
    psi = diag(R-LoadingMatrix%*%t(LoadingMatrix))
    # psi

    #residual matrix
    RM = R-(LoadingMatrix%*%t(LoadingMatrix) + diag(psi))
    # RM

  }

  #eigen values propotion visualazation
  g <- Gof.c %>%
    ggplot(aes(x=eig.prop, y=Value))+
    geom_bar(stat="identity",
             aes(colour=eig.prop, fill=eig.prop))+
    geom_text(aes(label= paste0(round(Value,1),"(%)"),
                  vjust= -0.5 ))

  #2 dimension graph
  plot(-LoadingMatrix[,1], -LoadingMatrix[,2],
       cex=cex, pch=21, bg="red",
       xlim=c(-1,1), ylim=c(-1,1))
  abline(v=0, h=0)
  text(-LoadingMatrix[,1], -LoadingMatrix[,2],
       labels = ifelse(name=="auto",rownames(R),
                       ifelse(name=="manual",rowname,"")),
       pos = pos)




  # result
  res = list(data=R,
             propDim=Gof.c,
             communality=communality,
             specific_variance = psi,
             residual_matrix=RM,
             factorloadings = -LoadingMatrix,
             prop_graph=g)

  res

}





#eigen value proption
pcfa_porp_plot <- function(Data,
                     digits=2,
                     prop=FALSE,
                     ymax=NA
                     ){

  data = Data$uniquenesses


  library(tidyverse)
  library(forcats)

  gof = data/sum(data)*100

  if(prop==FALSE){
  Gof.c <- gof %>% data.frame()
  colnames(Gof.c)="Value"

  Gof.c <- Gof.c%>%
    mutate(eig.prop= paste0("Dim",1: length(data))) %>%
    select(eig.prop, Value)

  g<-Gof.c %>%
    mutate(eig.prop = fct_reorder(eig.prop,
                                  desc(Value))) %>%
    ggplot( aes(x= eig.prop, y=Value))+
    geom_bar(stat="identity",
             aes(colour=eig.prop, fill=eig.prop))+
    geom_text(aes(label= paste0(round(Value,digits),"(%)"),
                  vjust= -0.5 ))+
    ylim(0, ymax)+
    labs(title="eigen value proportion")+
      theme_bw()

    }else if(prop==TRUE){
  #pre calculation porp 
    Gof.c <-   data %>% as_tibble() %>%
    mutate("eig.prop" = rownames(R)) %>% select(2,1) %>%
    rename(Value=value)
    # Gof.c <- Gof.c %>%  fct_reorder(eig.prop, Value)

    g<-Gof.c %>%
      mutate(eig.prop = fct_reorder(eig.prop,
                                    desc(Value))) %>%
      ggplot( aes(x= eig.prop, y=Value))+
      geom_bar(stat="identity",
               aes(colour=eig.prop, fill=eig.prop))+
      geom_text(aes(label= paste0(round(Value,digits),"")),
                    vjust= -0.5 )+
      ylim(0, ymax)+
      labs(title="factor Uniquenesses")+
      theme_bw()
  }
  res=list(data, Gof.c, g)
  res
}

# library(psych)
library(MVT)
library(GPArotation)

data(examScor)
 X = examScor
 psych::fa(X, nfactor=2)

X = examScor #data
Z = scale(X, scale=T)  # scale

R = cor(X)  # correlation matrix

#factor analysis tools : factanal
mlfa <- factanal(Z, factors = 2, rotation = "varimax")

mlfa|pcfa_porp_plot()


mlfa$loadings
mlfa$loadings[,]
mlfa|>str()
#Not calculation prorportion
mlfa$uniquenesses%>% propgraph(prop=F, ymax=30)

# prop 
mlfa$uniquenesses %>% propgraph(prop=T, ymax=0.5)

exam.fa = psych::fa(X, nfactor=2)
exam.fa$e.values %>% propgraph()








##################
library(jjstat)
stat_onl <- jjstat::stat_onl

stat_onl[, 5:7] |apply(1, mean)

sats


x<- 1:20
dim(x) <-c(4,5)
x

# Examples:

 x <- 1:12 ; dim(x) <- c(3,4)
 x

x <- array(1:24, dim = c(4,3,2))
x


#  Lunn, A. D. and McNeil, D.R. (1991) _Computer-Interactive Data  Analysis_, Wiley, New York
# install.packages("ade4")
library(ade4)
data(olympic)
str(olympic)

install.packages("nFactors")
?plotnScree
psych::plotnScree(nScree(olympic$tab))

library(sjPlot)
factor.plot(fa, labels=colnames(olympic$tab), pch=20, pos=4, title="Factor Plot")

?sjPlot::sjp.fa
library(sjPlot)
sjPlot::sjp.fa(olympic$tab)

library(GPArotation)
data(efc)
# recveive first item of COPE-index scale
start <- which(colnames(efc) == "c82cop1")
# recveive last item of COPE-index scale
end <- which(colnames(efc) == "c90cop9")
end
# use data frame as argument, let sjp.fa() compute FA
sjp.fa(efc[, start:end])
#Parallel analysis suggests that the number of factors =  3  and the number of components =  NA 
#Following items have no clear factor loading:
#none.



jjstat::exam_long

t.test(score ~ test, data= jjstat::exam_long, paired=TRUE)|str()
t.test(score ~ test, data= jjstat::exam_long, paired=TRUE)|>broom::tidy()

library(tidyverse)
 ?dplyr::cume_dist
x <- c(5, 1, 3, 2, 2)
cume_dist(c(5, 1, 3, 2, 2))
percent_rank(c(5, 1, 3, 2, 2))

"These two ranking functions implement two slightly different ways to compute a percentile. For each x_i in x:
cume_dist(x) counts the total number of values less than or equal to x_i, and divides it by the number of observations. percent_rank(x) counts the total number of values less than x_i, and divides it by the number of observations minus 1."|jjstat::kakaoi()


?min_rank
# x <- c(5, 1, 3, 2, 2, NA)
# row_number(c(5, 1, 3, 2, 2, NA))

# min_rank(c(5, 1, 3, 2, 2, NA))
# dense_rank(c(5, 1, 3, 2, 2, NA))
# # Ranking functions can be used in `filter()` to select top/bottom rows
# df0 <- data.frame(
#   grp = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#   x = c(3, 2, 1, 1, 2, 2, 1, 1, 1),
#   y = c(1, 3, 2, 3, 2, 2, 4, 1, 2),
#   id = 1:9
# )
# df0
# df0 %>% group_by(grp) 
# # Always gives exactly 1 row per group
# df0 %>% group_by(grp) %>% filter(row_number(x) == 1)
# # May give more than 1 row if ties
# df0 %>% group_by(grp) %>% filter(min_rank(x) == 1)
# # Rank by multiple columns (to break ties) by selecting them with `pick()`
# df0 %>% group_by(grp) %>% filter(min_rank(pick(x, y)) == 1)
# # See slice_min() and slice_max() for another way to tackle the same problem
# # You can use row_number() without an argument to refer to the "current"
# # row number.
# df0 %>% group_by(grp) %>% filter(row_number() == 1)
# # It's easiest to see what this does with mutate():
# df0 %>% group_by(grp) %>% mutate(grp_id = row_number())

?case_when()
# `case_when()` evaluates all RHS expressions, and then constructs its
# result by extracting the selected (via the LHS expressions) parts.
# In particular `NaN`s are produced in this case:
y <- seq(1, 5, by = 1)
y

# case_when(
#   y >= 0 ~ sqrt(y),
#   .default = y
# )

# generate data
y <- seq(1, 5, by = 1)
## recoding variable 
data.frame(y) %>% mutate(rev= case_when(
  y== 5 ~ 1,
  y== 4 ~ 2,
  y== 3 ~ 3,
  y== 2 ~ 4,
  y== 1 ~ 5
))
# `case_when()` is particularly useful inside `mutate()` when you want to
# create a new variable that relies on a complex combination of existing
# variables

starwars %>%
  select(name:mass, gender, species)

starwars %>%
  select(name:mass, gender, species) %>%
  mutate(
    type = case_when(
      height 200 | mass 200 ~ "large",
      species == "Droid" ~ "robot",
      .default = "other"
    )
  )



  ################
?gather

# From https://stackoverflow.com/questions/1181060
stocks <- tibble(
  time = as.Date("2009-01-01") + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)
stocks
gather(stocks, "stock", "price", -time)
stocks %>% gather("stock", "price", -time)



?spread
?jjstat::Freq_table

# 필요한 패키지 불러오기
library(ggplot2)
library(moments)  # skewness, kurtosis 계산을 위한 moments 패키지

# 예시 데이터 생성
data <- c(88, 95, 92, 97, 96, 97, 94, 86, 91, 95, 97, 88, 85, 76, 68)

# Density plot 그리기
x11()
ggplot(data = data.frame(data), aes(x = data)) +
  geom_density(color = "blue") +
  labs(title = "Density Plot of Data")
?skewness
# Skewness 계산
 moments::skewness(data)
print(paste("Skewness:", round(skew_value, 4)))
jjstat::SKEW(data)

# set.seed(1234)
# skewness(rnorm(1000), na.rm = TRUE)
# skewness(rnorm(1000), na.rm = FALSE)
# jjstat::SKEW(rnorm(1000))
# Kurtosis 계산
kurtosis_value <- kurtosis(data)
print(paste("Kurtosis:", round(kurtosis_value, 4)))

library(e1071)
#유형1 
e1071::skewness(mtcars$mpg, 1)
sprintf("교과서에서 사용하는 왜도 : %.6f", e1071::skewness(mtcars$mpg, 1))
#유형2 
e1071::skewness(mtcars$mpg, 2)
sprintf("SAS와 SPSS에서 사용되는 공식:%.6f", e1071::skewness(mtcars$mpg, 2))
#유형3 
e1071::skewness(mtcars$mpg, 3)
sprintf("MINITAB과 BMDP에서 사용되는 공식:%.6f", e1071::skewness(mtcars$mpg, 3))
moments::skewness(mtcars$mpg)


e1071::skewness(data, 1)
sprintf("교과서에서 사용하는 왜도 : %.6f", e1071::skewness(data, 1))
#유형2 
e1071::skewness(data, 2)
sprintf("SAS와 SPSS에서 사용되는 공식:%.6f", e1071::skewness(data, 2))
#유형3 
e1071::skewness(data, 3)
sprintf("MINITAB과 BMDP에서 사용되는 공식:%.6f", e1071::skewness(data, 3))
jjstat::SKEW(data)
moments::skewness(data)
?e1071::skewness


jjstat::comma



library(MVT)
data(examScor)
examScor%>% str()
X = examScor #data
Z = scale(X, scale=T)  #표준화

R = cor(X)  # correlation matrix
R
psych::principal(R, nfactor=2)

?factanal
# 예시 데이터 생성
my_data <- data.frame(x = c(1, 2, 3), y = c(10, 15, 8), label = c("A", "B", "C"))
my_data
# factanal 함수를 사용하여 요인분석 수행
# result <- factanal(x = my_data, factors = 2)

# 결과 확인
print(result)
sleep |str()

library(VIM)
VIM::aggr(sleep, prop=FALSE, numbers = TRUE)


?jjstat::missfillinFor

x11()

?rowMeans
library(tidyverse)
interplot_data = function(...,
                          col = 2,
                          row = 2,
                          type = "long",
                          title = NULL,
                          xlab = "x",
                          ylab = "y",
                          size_element = 12,
                          byrow= TRUE,
                          xy=FALSE){
library(tidyverse)
  a = c(...)

  # ddf = data.frame(
  #       B1 = c(a[1], a[3]),
  #       B2 = c(a[2], a[4])
  #       )%>%`rownames<-`(c("A1","A2"))

  ddf = matrix(a, ncol = col, nrow = row, byrow = byrow)
  colnames(ddf) = c(paste0("B",1:ncol(ddf) ))
  rownames(ddf) = c(paste0("A",1:nrow(ddf) ))

  longcol = ncol(ddf)+1

  ddf_long = ddf%>% as.data.frame() %>%
    tibble::rownames_to_column("v1") %>%
    tidyr::pivot_longer(cols = 2:all_of(longcol),
                        names_to = "v2", values_to="value")
  # list(ddf, a[1],a[2],a[3],a[4])

  #plot transpose default FALSE
  if(xy){

    g = ddf_long%>%
      ggplot(aes(x = v1, y = value))+
      geom_point(size = 4, aes(color=v2))+
      geom_line(aes(group = v2, linetype = v2 ), linewidth = 1)+
      geom_text(aes(label = value), vjust= -0.4, hjust = 2, size= 5)+
      labs(title = title, x = xlab, y = ylab)+
      theme_bw()+
      theme(axis.text = element_text(size = size_element),
            axis.title = element_text(size = size_element+2) )+
      ylim(min(ddf_long$value)-0.5, max(ddf_long$value)+0.5)

  }else{

    g = ddf_long%>%
      ggplot(aes(x = v2, y = value))+
      geom_point(size = 4, aes(color=v1))+
      geom_line(aes(group = v1, linetype = v1 ), linewidth = 1)+
      geom_text(aes(label = value), vjust= -0.4, hjust = 2, size= 5)+
      labs(title = title, x = xlab, y = ylab)+
      theme_bw()+
      theme(axis.text = element_text(size = size_element),
            axis.title = element_text(size = size_element+2) )+
      ylim(min(ddf_long$value)-0.5, max(ddf_long$value)+0.5)

  }

  res = list(ddf, ddf_long, g)
  switch(type,
         all = res,
         mat = ddf,
         long = ddf_long,
         plot = g )
}


# x11()
# interplot_data( 6, 1, 4, 5,6,7, type = "mat",  col=3, row=2) %>% as.data.frame() %>%
# tibble::rownames_to_column("v1") %>%pivot_longer(cols=2:4)

interplot_data( 6, 1, 4, 5, type = "mat") 
x
interplot_data(1,7,4,6, type="all", xy=F)
interplot_data(1,7,4,6, type="all",xy=T)
interplot_data(1,4,7,6, type="all", byrow=F)
interplot_data(1,4,7,6, type="all", byrow=T)

interplot_data( c(6, 1, 4, 5,6,7), type = "mat",  col = 3,row = 2)
interplot_data( c(6, 1, 4, 5,6,7), type = "long",  col = 3,row = 2)
interplot_data( 6, 1, 4, 5,6,7, type = "all",  col = 3,row = 2)
x11()
interplot_data( 6, 1, 4, 5,6,7, type = "plot",  col = 3,row = 2)

interplot_data( 6, 1, 4, 5,6,7, type = "all",  col = 2,row = 3)
interplot_data( 6, 1, 4, 5,6,7, type = "long",  col = 2, row = 3)
interplot_data( c(6, 1, 4, 5,6,7), type = "plot",  col = 2,row = 3)

interplot_data( 6, 1, 4, 5, type = "all")
interplot_data( c(6, 1, 4, 5), type = "all")
interplot_data( 6, 1, 4, 5, type = "long")
interplot_data( 6, 8, 4, 5, type = "plot", size_element= 13)


mtcarsdata = read.csv("https://raw.githubusercontent.com/shoutjoy/publicData/main/mtcars.csv")
mtcarsdata

# c(c(6, 1, 4, 5))
# c(6, 1, 4, 5)

interplot_data( 6, 1, 4, 5) %>%
ggplot(aes(x = v1, y = value))+
geom_point(size=4, aes(color=v2))+
geom_line(aes(group = v2, linetype = v2 ), linewidth = 1)+
geom_text(aes(label = value), vjust= -0.4, hjust= 2, size= 5)+
theme_bw()


interplot = function(data_long,
                     type="plot",
                     size_point = 4,
                     size_text = 5,
                     linewidth = 1,
                     vjust = -0.4,
                     hjust = 2,
                     title = NULL,
                     xlab = "x_variable",
                     ylab = "y_variable",
                     size_element = 12,
                     t = FALSE,
                     xy = FALSE

){


  library(tidyverse)

  if(t){
    data_wide <-  data_long
    longcol = ncol(data_wide)+1

    data_long = data_wide%>% as.data.frame() %>%
      tibble::rownames_to_column("v1") %>%
      tidyr::pivot_longer(cols = 2:all_of(longcol),
                          names_to = "v2", values_to="value")
  }else{
    data_long <- as.data.frame(data_long)
    colnames(data_long) = c("v1", "v2", "value")
  }

  #generate matrix
  mat = data_long  %>%
    tidyr::pivot_wider(names_from = "v2",
                       values_from ="value")%>%
    tibble::column_to_rownames("v1")
  colnames(mat) = c( paste0("B",1:ncol(mat)) )
  rownames(mat) = c( paste0("A",1:nrow(mat)) )

  view_mat = t(mat)

if(xy){
  g =  data_long %>%
    ggplot(aes(x = v1, y = value))+
    geom_point(size = size_point , aes(color = v2))+
    geom_line(aes(group = v2, linetype =v2),
              linewidth = linewidth)+
    geom_text(aes(label = value),
              vjust= vjust,
              hjust = hjust,
              size = size_text)+
    labs(title= title, x = xlab, y = ylab)+
    theme_bw()+
    theme(axis.text = element_text(size = size_element),
          axis.title = element_text(size = size_element + 3))+
      ylim(min(data_long$value)-0.5, max(data_long$value)+0.5)
}else{
  g =  data_long %>%
    ggplot(aes(x = v2, y = value))+
    geom_point(size = size_point , aes(color = v1))+
    geom_line(aes(group = v1, linetype = v1),
              linewidth = linewidth)+
    geom_text(aes(label = value),
              vjust= vjust,
              hjust = hjust,
              size = size_text)+
    labs(title= title, x = xlab, y = ylab)+
    theme_bw()+
    theme(axis.text = element_text(size = size_element),
          axis.title = element_text(size = size_element + 3))+
      ylim(min(data_long$value)-0.5, max(data_long$value)+0.5)
  
}
 

  res = list(martrix = mat, plot = view_mat, plot = g)

  switch(type,
         all = res,
         plot = g,
         mat = mat,
         plot_mat = view_mat,
         data = data_long

  )

}

interplot_data( 6, 1, 4, 5, type = "long")%>%
interplot(size_element = 10)
###only plot 
interplot_data( 2, 1, 4, 5, type="long")%>%interplot()

interplot_data( 4, 1, 2, 5, 6, 3, type="long", row=3, col=2)%>%interplot()
interplot_data( 4, 1, 2, 5, 6, 3, type="long", row=2, col=3)%>%interplot()

interplot_data( 4, 1, 2, 5, 6, 3, type="long", row=2, col=3)%>%interplot("all")

#generated direct data matrix and data frame 
matrix(c(4, 1, 2, 5, 6, 3), nrow=2,ncol=3)

matrix(c(4, 1, 2, 5, 6, 3), nrow=2,ncol=3) %>% data.frame() %>% rownames_to_column("v1") %>% pivot_longer(cols=2:4)

matrix(c(4, 1, 2, 5, 6, 3), nrow=2,ncol=3) %>% data.frame() %>% rownames_to_column("v1") %>% pivot_longer(cols=2:4)%>% interplot()

matrix(c(4, 1, 2, 5, 6, 3), nrow=2, ncol=3) %>% interplot()


#matrix 
matrix(c(4, 1, 2, 5, 6, 3), nrow=3, ncol=2)

matrix(c(4, 1, 2, 5, 6, 3), nrow=3, ncol=2) %>% data.frame() %>% rownames_to_column("v1") %>% pivot_longer(cols=2:3)

matrix(c(4, 1, 2, 5, 6, 3), nrow=3, ncol=2) %>% data.frame() %>% rownames_to_column("v1") %>% pivot_longer(cols=2:3)%>% interplot()

matrix(c(4, 1, 2, 5, 6, 3), nrow=3, ncol=2) %>% interplot(t=TRUE)
matrix(c(4, 1, 2, 5, 6, 3), nrow=3, ncol=2) %>% interplot(t=TRUE, xy=T)


matrix(c(4, 1, 2, 5, 6, 3), nrow=3, ncol=2) %>% interplot(t=TRUE, type="all")



matrix(c(4, 1, 2, 5, 6, 3), nrow = 3, ncol = 2) %>%
data.frame() %>% rownames_to_column("v1") %>%
 pivot_longer(cols = 2:3) %>%
  interplot(type="all")

# matrix(c(4, 1, 2, 5, 6, 3), nrow=2,ncol=3) %>%
# data.frame() %>% rownames_to_column("v1")%>%
# interplot(type="all")

interplot_data( c(6, 1, 4, 5,6,7), type = "mat",  row = 3, col = 2)

interplot_data( c(6, 1, 4, 5,6,7), type = "long",  row = 3, col = 2)%>%interplot("all")

interplot_data( c(6, 1, 4, 5,6,7), type = "mat",  row = 2, col = 3)
interplot_data( c(6, 1, 4, 5,6,7), type = "long",  row = 2, col = 3)%>%
interplot("all")


interplot_data( c(6, 1, 4, 5,6,7), type = "long", row = 2, col = 3)%>%

interplot()

interplot_data( c(6, 1, 4, 5,6,7), type = "long",  col = 3,row = 2) %>%
tidyr::pivot_wider(names_from = "v2", values_from ="value")%>%
tibble::column_to_rownames("v1") %>% t()

interplot_data( c(6, 1, 4, 5,6,7), type = "long",  col = 2,row = 3)%>%
interplot()

interplot_data( c(6, 1, 4, 5,6,7), type = "mat",  row = 3, col = 2)
interplot_data( c(6, 1, 4, 5,6,7), type = "long", row = 3, col = 2) %>%
tidyr::pivot_wider(names_from = "v2", values_from ="value")%>%
tibble::column_to_rownames("v1") %>%t()


interplot( c(6,7,4,5))

interplot( 1,4,7,6)%>%
 rownames_to_column("v1") %>%
   pivot_longer(cols = 2:3, names_to = "v2", values_to="value")





report_aov <- function(data, digits = 2, trans=FALSE) {
 data0 = data
 dv = as.character(data$call[[2]][2])
 iv = as.character(data$call[[2]][3])
 Data = broom::tidy(data0)
 summary_data = summary(data0)

 # Extract relevant information from the data
 term <- Data$term
 df <- Data$df
 statistic <- format(Data$statistic, 3, trim=TRUE)
 p_value <- Data$p.value

 # Create the sentence based on the method and p-value
 result_sentence = list()
 for( i in 1: nrow(Data)-1){
  # Determine the significance based on p-value
  significance <- ifelse(p_value[i] < 0.05,
              "It was statistically significant",
              "It was not statistically significant")

  # Determine if the term is an interaction effect or a main effect
  effect <- ifelse(grepl(":", term[i]),
              "Interaction effect",
              "Main effect")

  result_sentence[i] <- paste0(effect, ": one-way analysis of variance result for dv[",
                 dv,"]~[",term[i],"], ",
                 significance, "(F(",
                 df[i],
                 ", ",
                 df[nrow(Data)],
                 ") = ",
                 statistic[i],
                 ", p = ",
                 format(p_value[i], 3, trim=TRUE),
                 "). \n")
 }
 # Print the result
 res = c(do.call(rbind, result_sentence))

 if(trans){
  cat( jjstat::k(res,"en","ko")[2] , "\n")
 }else{
  cat(res ,"\n")
 }
}

aov(len ~ supp*factor(dose), data= ToothGrowth) %>%
report_aov(data)
?format

# grepl(":",  "[supp:dose]")



aov(len ~ supp*factor(dose), data= ToothGrowth)$term[2]



############################
p_mark_sig <-function(data, 
                      col = "p.value", 
                      unite = FALSE,
                      digits= 3
                      ){
  #
  library(tidyverse)

  p.value <- data %>% as.data.frame() %>%
    dplyr::select(all_of(col))
  # any_of() does not check for missing variables. This is especially useful for negative selection when you want to check if a variable has been removed.
  # all_of() is for strict selection. An error occurs if any of the variables in the character vector are missing.
  # all_of : correct, any_of: some
  ndata <- data %>%
    as.data.frame() %>%
    dplyr::mutate(sig = ifelse(p.value < 0.001, "***",
                        ifelse(p.value < 0.01, "**",
                               ifelse(p.value < 0.05, "*",
                                      "ns"))))
  #Change variable to CHARACTER
  ndata$sig <- as.character(ndata$sig)
  res = ndata %>% tibble::tibble()
      


if(unite){
 res = res %>% dplyr::rename(p.value = all_of(col)) %>%
        mutate_if(is.numeric, round, digits)
 res = res %>% tidyr::unite(p.value, p.value, sig, sep="")
 res
}else{
  res }
}

# ?unite
ToothGrowth %>%
   group_by(dose) %>%
  rstatix::t_test(data =., len ~ supp) %>%
  p_mark_sig("p")

ToothGrowth %>%   group_by(dose) %>%  rstatix::t_test(data =., len ~ supp) %>%  p_mark_sig("p", unite=TRUE, digits=2)




?expand_grid
expand_grid(x = 1:4, y = 1:2)
expand_grid(l1 = letters, l2 = LETTERS)
# Can also expand data frames
expand_grid(df = tibble(x = 1:2, y = c(2, 1)), z = 1:3)
# And matrices
expand_grid(x1 = matrix(1:4, nrow = 2), x2 = matrix(5:8, nrow = 2))

# unite(data, col, ..., sep = "_", remove = TRUE, na.rm = FALSE)
df <- expand_grid(x = c("a", NA), y = c("b", NA))
df
df %>% unite("z", x:y, remove = FALSE)
# To remove missing values:
df %>% unite("z", x:y, na.rm = TRUE, remove = FALSE)
# Separate is almost the complement of unite
df %>%
  unite("xy", x:y) %>%
  separate(xy, c("x", "y"))
# (but note `x` and `y` contain now "NA" not NA)


#########################
confint_mark_sig <- function(data, unite=FALSE,digits=3){

  data <- data %>% data.frame()
  ndata <- data%>%
    mutate(sig = ifelse( data[, ncol(data)-1]*data[, ncol(data)] 0, "*","ns"))

  #변수를 CHARACTER로 변경
  ndata$sig <- as.character(ndata$sig)
 res = ndata %>% tibble::tibble() %>%
       dplyr::mutate_if(is.numeric, round, digits)
      


if(unite){
#  ci = res %>% select(ncol(res)-1, ncol(res))
#  colnames(ci) = c("LCI","UCI")
  res = res %>% tidyr::unite(CI, ncol(res)-2, ncol(res)-1, sep=", ")
  # res = res %>% tidyr::unite(CI, ncol(res)-1, ncol(res), sig, sep=", ")
 res$CI = format(paste0("[",res$CI,"]"), digits)
 res = res %>% tidyr::unite(CI, CI, sig, sep="")
 res
}else{
  res }
}


 lm(mpg ~ hp + wt, data=mtcars)%>%
  confint()%>%as.data.frame()%>%
   rownames_to_column()%>%
   confint_mark_sig()

 lm(mpg ~ hp + wt, data=mtcars)%>%  confint()%>%as.data.frame()%>%   rownames_to_column()%>%   confint_mark_sig(unite=TRUE, digits=2)


#######################################
Round <- function(data, digits=3){
if(is.data.frame(data)){  
  # data = data%>%rownames_to_column( "rownames")

  data = data %>% dplyr::mutate_if(is.numeric, round, digits)

#  data = data%>% column_to_rownames("rownames")

  }else{
    data <- sapply(data, 
    function(x) {if(is.numeric(x)){round(x, digits)}})
}
data
}

Round(12.345654)

mtcars%>% Round(1) 
?column_to_rownames
jjstat::mysummaryBy(mpg ~ vs*cyl, data = mtcars)
 ##statistic data output
jjstat::mysummaryBy(mpg ~ 1, data = mtcars, "mpg", stat="t.test")
jjstat::mysummaryBy(mpg ~ vs, data = mtcars)
jjstat::mysummaryBy(mpg ~ vs, data = mtcars, stat="t.test")
jjstat::mysummaryBy(mpg ~ vs+am, data = mtcars, stat="aov")
jjstat::mysummaryBy(mpg ~ vs*cyl, data = mtcars, stat="aov")


?tbl_vars
#########################################

mysummaryBy <- function(data,formula,
                        add_var = NULL,
                        stat = FALSE,
                        agg = FALSE,
                        gm = FALSE,
                        digits = 2) {
  # Make sure the data object is provided
  if (missing(data)) stop("Please provide the data object as an argument.")
  # Import dplyr if needed
  if (requireNamespace("dplyr")) library(dplyr)
  # Aggregate with summary statistics
  func = formula(formula) #formula extraction

  # analysis SKEW, KURT
  result <- aggregate(formula(formula), data,
                      FUN = function(x) {
                        c(
                          Mean = mean(x, na.rm = TRUE),
                          SD = sd(x, na.rm = TRUE),
                          N = length(x),
                          Min = min(x, na.rm = TRUE),
                          Max = max(x, na.rm = TRUE),
                          Skew = jjstat::SKEW(x),
                          Kurt = jjstat::KURT(x)
                        )
                      })


  if(stat=="t.test"){
    stat_res = t.test(formula, data = data) %>% broom::tidy()%>%select(1:5)
  }else if(stat=="aov"){
    stat_res = aov(formula(formula), data = data) %>% broom::tidy()
  }else{
    stat_res=NULL
  }


  # Use when there are too many independent and dependent variables
  if(agg){
    res = result %>%
      # t() %>%
      data.frame()

    res <- res %>%
      mutate(across(where(is.numeric), round, digits))%>%
      t()%>%
      data.frame() %>%
      tibble::rownames_to_column("stat_var") %>%
      tibble::tibble()
     colnames(res) = c("stat_var",  paste0("grp", 1:ncol(res[,-1])))
    
 #stat view 
   if(is.null(stat_res)){
        res
      }else{
        res = list(descriptive = res, 
                   ANOVA = stat_res)
        res
      }


  }else{
    if(func[3]!='1()'){
      res = dplyr::bind_cols(var = result[,1: (ncol(result)-1) ],
                             result[[ncol(result)]] ) %>%
                              tibble::tibble()

      if(is.null(stat_res)){
        res
      }else{
        res = list(descriptive = res, statistic= stat_res)
        res
      }

    }else{ #Used to obtain statistics for one variable
      res = dplyr::bind_cols(stat_var = add_var,
                             result[[ncol(result)]] ) %>% 
                             tibble::tibble()
      if(is.null(stat_res)){
        res
      }else{
        res = list(descriptive = res, 
                   statistic = stat_res)
        res
            }
          }
        }


  # Measure the average of each group - Measure the average of group level with the average of each group
if(is.list(res)){
res

}else{

  Res = res

  if(gm){
    Res = Res %>% mysummary("Mean")
    Res = bind_cols(
      grp = as.character(func[3]),
      dv = as.character(func[2]),
      Res[,-1])
    Res%>%  mutate_if(is.numeric, round, digits)
  }else{
    Res%>%  mutate_if(is.numeric, round, digits)
        }
}

}


mysummaryBy(mpg ~ 1, data = mtcars)
mysummaryBy(mpg ~ 1, data = mtcars, "mpg")
mysummaryBy(mpg ~ 1, data = mtcars, "mpg", stat="t.test")


mysummaryBy(mpg ~ vs, data = mtcars, stat="t.test")



mysummaryBy(mpg ~ vs, data = mtcars, stat="t.test")
mysummaryBy(mpg ~ vs+am, data = mtcars, agg=TRUE)
mysummaryBy(mpg ~ vs*am, data = mtcars, agg=TRUE, stat="aov")
mysummaryBy(mpg ~ vs+am, data = mtcars, stat="aov")
mysummaryBy(mpg ~ vs*cyl, data = mtcars, stat="aov")






?aggregate
aggregate(cbind(Ozone, Temp) ~ Month, data = airquality, mean)

library(heplots)
aggregate(cbind(mb,bh,bl,nh) ~ epoch , data=heplots::Skulls, FUN= mean)%>%Round(2)
# aggregate(cbind("mb","bh","bl","nh") ~ epoch , data=heplots::Skulls, FUN= mean)

aggregate(cbind(mb,bh,bl,nh) ~ epoch , data=heplots::Skulls,
          FUN= function(x) {
                        c(
                          Mean = mean(x, na.rm = TRUE),
                          SD = sd(x, na.rm = TRUE),
                          N = length(x))})
mysummaryBy(cbind(mb,bh,bl,nh) ~ epoch, data=heplots::Skulls ) %>%data.frame() %>%Round(2)

aggregate(x = epoch, by = list(fby1, fby2), FUN = "mean")

ToothGrowth |aggregate(len ~ ., FUN = mean)
?Skulls



library(heplots)
data(Skulls)
library(car)    # for Anova
# make shorter labels for epochs
Skulls$epoch <- factor(Skulls$epoch, labels=sub("c","",levels(Skulls$epoch)))

Skulls
# longer variable labels
vlab <- c("maxBreadth", "basibHeight", "basialLength", "nasalHeight")
# fit manova model
sk.mod <- lm(cbind(mb, bh, bl, nh) ~ epoch, data=Skulls)
sk.mod
car::Anova(sk.mod)
summary(car::Anova(sk.mod, type=3))

# test trends over epochs
print(car::linearHypothesis(sk.mod, "epoch.L"), SSP=FALSE) # linear component
print(car::linearHypothesis(sk.mod, "epoch.Q"), SSP=FALSE) # quadratic component

?scatterplot
# typical scatterplots are not very informative
x11()
vlab <- c("maxBreadth", "basibHeight", "basialLength", "nasalHeight")
car::scatterplot(mb ~ bh|epoch, data=Skulls, 
            ellipse = list(levels=0.68), 
            smooth=FALSE, 
            legend = list(coords="topright"),
            xlab=vlab[2], ylab=vlab[1])

car::scatterplot(mb ~ bl|epoch, data=Skulls, 
            ellipse = list(levels=0.68), 
            smooth=FALSE, 
            legend = list(coords="topright"),
            xlab=vlab[3], ylab=vlab[1])
# HE plots
heplot(manova(cbind(mb, bh, bl, nh) ~ epoch, data=Skulls), 
       hypotheses=list(Lin="epoch.L", Quad="epoch.Q"), 
       xlab=vlab[1], ylab=vlab[2])

pairs(manova(cbind(mb, bh, bl, nh) ~ epoch, data=Skulls), 
      hypotheses=list(Lin="epoch.L", Quad="epoch.Q"), 
      var.labels=vlab)
# 3D plot shows that nearly all of hypothesis variation is linear!
## Not run: 
heplot3d(sk.mod, hypotheses=list(Lin="epoch.L", Quad="epoch.Q"), col=c("pink", "blue"))
# view in canonical space
if (require(candisc)) {
	sk.can <- candisc(sk.mod)
	sk.can
	heplot(sk.can)
	heplot3d(sk.can)
}
## End(Not run)




#  cat("분석 결과, 시대별(epoch) craniometric measurements에 대한 다변량 분석 결과는 다음과 같습니다:\n\n")
  
#   # Outputting mean effects for each variable
#   cat(paste("mb (Maximum breadth of the skull)에서의 epoch의 평균 효과는", epoch_means$coefficients[1], "로 나타났습니다.\n"))
#   cat(paste("bh (Basibregmatic height)에서의 epoch의 평균 효과는", epoch_means$coefficients[2], "로 나타났습니다.\n"))
#   cat(paste("bl (Basi-alveolar length)에서의 epoch의 평균 효과는", epoch_means$coefficients[3], "로 나타났습니다.\n"))
#   cat(paste("nh (Nasal height)에서의 epoch의 평균 효과는", epoch_means$coefficients[4], "로 나타났습니다.\n\n"))
  
#   # Outputting residual information for each variable
#   cat("각 측정 항목에 대한 잔차(residuals)는 각각의 측정 항목에 대한 오차 변동을 나타냅니다. 이러한 결과들은", df[2], "개의 자유도를 갖는 것으로 나타났습니다.\n\n")
  
#   cat("또한, 잔차 표준 오차는 각각의 측정 항목에 대해 다음과 같이 나타났습니다:\n")
#   cat(paste("mb:", residual_std_errors[1], "\n"))
#   cat(paste("bh:", residual_std_errors[2], "\n"))
#   cat(paste("bl:", residual_std_errors[3], "\n"))
#   cat(paste("nh:", residual_std_errors[4], "\n"))

###########################
manova(cbind(mb,bh,bl,nh)~ epoch, data=Skulls)

manovares = manova(cbind(mb,bh,bl,nh)~ epoch, data=Skulls)
sumamryres = manova(cbind(mb,bh,bl,nh)~ epoch, data=Skulls)%>%
summary.aov()


# sqtest =model_summary%>% 
#               data.table::rbindlist()
# sqtest = bind_cols(
#             var = c(manovares$residuals %>%colnames(),"residuals"),
#             model_summary%>% 
#               data.table::rbindlist() %>% 
#                 # slice(seq(1, n(), by = 2)) %>% #odd extraction 
#                 tibble()   )%>% p_mark_sig("Pr(>F)")

# dof = c( "DoF", 
#           nrow(manovares$residuals) - manovares$df.residual - 1,
#             manovares$df.residual,
#             "",
#             ""            
#             )

###########################################
report_manova <- function(model_manova, 
                          type="res", 
                          digits = 2) {
 manovares = model_manova
  model_summary = summary.aov(model_manova)
  # Extracting relevant information from the model summary
  type2 = manovares %>%summary()
#####

var = c(manovares$residuals %>%colnames())
# combinde data.frame 
sqtest = do.call(rbind, 
                lapply(model_summary, function(x) {
                    data.frame(
                     name = trimws(rownames(x)),
                    #  levels = c(manovares$residuals %>%colnames()),
                     Df = x$Df,
                     Sum_Sq = x$"Sum Sq",
                     Mean_Sq = x$"Mean Sq",
                     F_value = x$"F value",
                      Pr_F = x$"Pr(>F)" )
                       }))%>% p_mark_sig("Pr_F")%>%
              dplyr::filter(name != "Residuals")

residuals = do.call(rbind, 
                lapply(model_summary, function(x) {
                    data.frame(
                     name = trimws(rownames(x)),
                    # levels = c(manovares$residuals %>%colnames()),
                     Df = x$Df,
                     Sum_Sq = x$"Sum Sq",
                     Mean_Sq = x$"Mean Sq",
                     F_value = x$"F value",
                      Pr_F = x$"Pr(>F)" )
                       }))%>% p_mark_sig("Pr_F")%>%
              dplyr::filter(name == "Residuals")

#Sum of squares and products for the hypothesis
iv = as.character(manovares$call[[2]][3])  
## data combine 
sosh = cbind(sqtest[,c(1,2, 3)], 
             residuals[,c(3)],
             sqtest[,c(5, 7)])%>%
             `colnames<-`(c("dv","Df", iv ,"Residuals","F","sig" ))%>% 
    Round(digits) %>%
   tidyr::unite(F, F, sig, sep="") 

sosh$F = format(sosh$F, justify="left")

Nrow =  nrow(sosh)
Levels = manovares$residuals %>%colnames()
Nlevels =  length(manovares$residuals %>%colnames())
levels = rep(c(manovares$residuals %>%colnames()), each= Nrow/Nlevels)

## data arrange 
### combine variables 
sosh1  =  cbind(levels, sosh ) %>%
           tidyr::unite(dv, dv, levels, sep="_")
### combine variables
sqtest =  cbind(levels, sqtest ) %>%
           tidyr::unite(dv, name, levels, sep="_")%>%
           Round(digits)

##Mutltivariate test  : Pillai, Wilks,  Hotelling-Lawley, Roy   
Pillai0 = summary(manovares, test = "Pillai")
Wilks0 = summary(manovares, test = "Wilks")
Hotelling_Lawley0 = summary(manovares, test = "Hotelling-Lawley")
Roy0 = summary(manovares, test = "Roy")

## Select data for combine 
Pillai = summary(manovares, test = "Pillai")$stats%>%
                as.data.frame() %>%
                tibble::rownames_to_column("term") %>%
                dplyr::filter(term != "Residuals")%>%
                rename(test = Pillai)
Wilks = summary(manovares, test = "Wilks")$stats%>%
                as.data.frame() %>%
                tibble::rownames_to_column("term") %>%
                dplyr::filter(term != "Residuals") %>%
                rename(test = Wilks)
Hotelling_Lawley = summary(manovares, test = "Hotelling-Lawley")$stats%>%
                as.data.frame() %>%
                tibble::rownames_to_column("term") %>%
                dplyr::filter(term != "Residuals")%>%
                rename(test = `Hotelling-Lawley`)
Roy = summary(manovares, test = "Roy")$stats %>%
                as.data.frame() %>%
                tibble::rownames_to_column("term") %>%
                dplyr::filter(term != "Residuals")%>%
                rename(test = Roy)

#####Multivariate_Tests : combine each test 
Multivariate_Tests = rbind(
  Pillai%>% dplyr::slice( nrow(Pillai)), 
  Wilks%>% dplyr::slice( nrow(Wilks)), 
  Hotelling_Lawley%>% dplyr::slice( nrow(Hotelling_Lawley)), 
  Roy %>% dplyr::slice( nrow(Roy))
      )%>% `rownames<-`(c("Pillai", "Wilks", "Hotelling-Lawley", "Roy"))%>%
          dplyr::select(-term) %>%
          tibble::rownames_to_column("Test")%>%
          p_mark_sig("Pr(>F)")%>%
          Round(digits)

##result arrange 
res0 = list(
  MANOVA_test = type2, 
          SS_hypothesis = sosh1, 
          # variable_tests = sqtest, 
          Multivariate_Tests = Multivariate_Tests
           )
#full result data 
res = list(
  MANOVA_test = type2, 
          SS_hypothesis = sosh1, 
          variable_tests = sqtest, 
          Multivariate_Tests = Multivariate_Tests
           )

## message 
cat("\nType II MANOVA Tests(by jjstat package): \n\n")

## result putpput
switch(type, 
      res = res0,
      all = res,
      summmary_aov = model_summary, 
      sumsq = sosh1,
      aov = sqtest,
      Pillai = Pillai0,
      Wilks = Wilks0,
      Hotelling_Lawley = Hotelling_Lawley0,
      Roy = Roy0,
      Multivariate_Tests= Multivariate_Tests,
      levels = Levels
      )

}

library(jjstat)

# library(heplots)
Skulls = heplots::data(Skulls)
# execute 
manova(cbind(mb,bh,bl,nh)~ epoch, data=Skulls)%>%class()
# lm(cbind(mb,bh,bl,nh)~ epoch, data=Skulls) %>%report_manova()
manova(cbind(mb,bh,bl,nh)~ epoch, data=Skulls) %>%report_manova()
manova(cbind(mb,bh,bl,nh)~ epoch, data=Skulls) %>%report_manova(type="all")
manova(cbind(mb,bh,bl,nh)~ epoch, data=Skulls) %>%report_manova(type="summmary_aov")
manova(cbind(mb,bh,bl,nh)~ epoch, data=Skulls) %>%report_manova(type="Pillai")
manova(cbind(mb,bh,bl,nh)~ epoch, data=Skulls) %>%report_manova(type="Wilks")
manova(cbind(mb,bh,bl,nh)~ epoch, data=Skulls) %>%report_manova(type="Hotelling_Lawley")
manova(cbind(mb,bh,bl,nh)~ epoch, data=Skulls) %>%report_manova(type="Roy")
manova(cbind(mb,bh,bl,nh)~ epoch, data=Skulls) %>%report_manova(type="Multivariate_Tests")


## Example on producing plastic film from Krzanowski (1998, p. 381)
Y <- cbind.data.frame(
  tear=c(6.5, 6.2, 5.8, 6.5, 6.5, 6.9, 7.2, 6.9, 6.1, 6.3,
          6.7, 6.6, 7.2, 7.1, 6.8, 7.1, 7.0, 7.2, 7.5, 7.6), 
   gloss=c(9.5, 9.9, 9.6, 9.6, 9.2, 9.1, 10.0, 9.9, 9.5, 9.4,
           9.1, 9.3, 8.3, 8.4, 8.5, 9.2, 8.8, 9.7, 10.1, 9.2),
    opacity=c(4.4, 6.4, 3.0, 4.1, 0.8, 5.7, 2.0, 3.9, 1.9, 5.7,
             2.8, 4.1, 3.8, 1.6, 3.4, 8.4, 5.2, 6.9, 2.7, 1.9),
   rate = gl(2,10, labels = c("Low", "High")),
   additive = gl(2, 5, length = 20, labels = c("Low", "High")) )
manova(cbind(tear, gloss, opacity) ~ rate * additive, data=Y)%>%report_manova()
manova(cbind(tear, gloss, opacity) ~ rate * additive, data=Y)%>%report_manova(type="all")
manova(cbind(tear, gloss, opacity) ~ rate * additive, data=Y)%>%report_manova(type="summmary_aov")
manova(cbind(tear, gloss, opacity) ~ rate * additive, data=Y)%>%report_manova("Pillai")
manova(cbind(tear, gloss, opacity) ~ rate * additive, data=Y)%>%report_manova("Wilks")
manova(cbind(tear, gloss, opacity) ~ rate * additive, data=Y)%>%report_manova("Hotelling_Lawley")
manova(cbind(tear, gloss, opacity) ~ rate * additive, data=Y)%>%report_manova("Roy")
manova(cbind(tear, gloss, opacity) ~ rate * additive, data=Y)%>%report_manova("Multivariate_Tests")

# manova(cbind(mb,bh,bl,nh)~ epoch, data=Skulls) %>%summary(test="Pillai")

# manova(cbind(tear, gloss, opacity) ~ rate * additive, data=Y)%>%summary(test="Pillai")


# manova(cbind(mb,bh,bl,nh)~ epoch, data=Skulls)$residuals %>%colnames()
# manova(cbind(tear, gloss, opacity) ~ rate * additive, data=Y)$residuals %>%colnames()

# ?mutate_at()
# ?mutate_if()
# 가정: 'df'는 우리가 사용하는 데이터프레임입니다.
# df$name <- trimws(df$name)
# 
summary(car::Anova(lm(cbind(tear, gloss, opacity) ~ rate * additive, data=Y), type=2))

resum = manova(cbind(tear, gloss, opacity) ~ rate * additive, data=Y)%>%
summary.aov()
resum[1]%>%class() #%>% rownames()

manova(cbind(tear, gloss, opacity) ~ rate * additive, data=Y)
manova(cbind(tear, gloss, opacity) ~ rate * additive, data=Y)$residuals %>%colnames()



manova(cbind(tear, gloss, opacity) ~ rate, data=Y)
manova(cbind(tear, gloss, opacity) ~ rate, data=Y)%>%
report_manova(3)




# 가정: 'aov_result'는 우리가 사용하는 ANOVA 결과입니다.
lapply(resum, rownames)
 do.call(cbind, lapply(resum, rownames))

# 각 리스트 요소를 데이터 프레임으로 변환합니다.
df_list <- lapply(resum, function(x) {
  data.frame(
    name= rownames(x),
    Df = x$Df,
    Sum_Sq = x$"Sum Sq",
    Mean_Sq = x$"Mean Sq",
    F_value = x$"F value",
    Pr_F = x$"Pr(>F)"
  )
})

# 데이터 프레임을 결합합니다.
do.call(rbind, df_list)
















#-------


# manova(cbind(mb,bh,bl,nh)~ epoch, data=Skulls) 
# Skulls %>%nrow()
# # iv = manovares$call[[2]][3]%>%as.character()
# nrow(manovares$residuals)
# dof= cbind(df_res = manovares$df.residual,
#             df = nrow(manovares$residuals) - df_res - 1)

# manovares$xlevels
# cat("\nType II MANOVA Tests: \n\n")
# manovares %>%summary()


# # resid(manovares)

# #####
# bind_cols(
# var = manovares$residuals %>%colnames(),
# sumamryres%>% data.table::rbindlist() %>% 
#               slice(seq(1, n(), by = 2)) %>% 
#               tibble()   )%>% p_mark_sig("Pr(>F)")

# #####
# rbind(
#  summary(manovares, test = "Pillai")$stats[1,],
#  summary(manovares, test = "Wilks")$stats[1,],
# summary(manovares, test = "Hotelling-Lawley")$stats[1,],
# summary(manovares, test = "Roy")$stats[1,]
# )%>%`rownames<-`(c("Pillai", "Wilks", "Hotelling-Lawley", "Roy"))%>%
# as.data.frame() %>%rename(test= Pillai) %>%
# rownames_to_column("Multivariate_Tests") %>%
# p_mark_sig("Pr(>F)")













# manovares = manova(cbind(mb,bh,bl,nh)~ epoch, data=Skulls)
# manovares 
# manovares$residuals %>%colnames() 

# manovares %>%str()
# manovares$coefficients
# manovares$residuals
# manovares$effects
# manovares$rank
# manovares$fitted.values  #predict 
# manovares$assign
# manovares$qr
# manovares$df.residual
# manovares$contrasts
# manovares$xlevels$epoch
# manovares$call
# manovares$terms
# manovares$model
# coef(manovares) #*
# resid(manovares)
# effects(manovares) %>%head()
# terms(manovares)
# predict(manovares)

# summary(manovares, test = "Pillai")$stats[1,]
# summary(manovares, test = "Wilks")$stats[1,]
# summary(manovares, test = "Hotelling-Lawley")$stats[1,]
# summary(manovares, test = "Roy")$stats[1,]
# #	The name of the test statistic to be used. Partial matching is used so the name can be abbreviated.
# test = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy")

# sumamryres = manova(cbind(mb,bh,bl,nh)~ epoch, data=Skulls)%>%summary.aov()
# sumamryres[[1]] %>%rownames()
# sumamryres%>% str()
# sumamryres

# ## 데이터 묶음
# options(knitr.kable =NA,"")

# bind_cols(
# var = rep( manovares$residuals %>%colnames(), each=2),
# type = rep(sumamryres[[1]] %>%rownames(), 4), 
# sumamryres%>% data.table::rbindlist() %>% tibble()) %>%
# unite(dv, var, type) %>% knitr::kable("pandoc", 2)



################

cat("Type II MANOVA Tests:")
car::Anova(manovares, type=2)
car::Anova(manovares, type=3)



bind_cols(
var = rep( manovares$residuals %>%colnames(), each=2),
type = rep(sumamryres[[1]] %>%rownames(), 4), 
sumamryres%>% data.table::rbindlist() %>% tibble()) %>%
unite(dv, var, type)


# sumamryres%>% data.table::rbindlist() %>% slice(seq(1, n(), by = 2))
manovares

# cat("\nType II MANOVA Tests: \n\n")
# manovares %>%summary()


# # resid(manovares)

# #####
# bind_cols(
# var = manovares$residuals %>%colnames(),
# sumamryres%>% data.table::rbindlist() %>% 
#               slice(seq(1, n(), by = 2)) %>% 
#               tibble()   )%>% p_mark_sig("Pr(>F)")

# #####
# rbind(
#  summary(manovares, test = "Pillai")$stats[1,],
#  summary(manovares, test = "Wilks")$stats[1,],
# summary(manovares, test = "Hotelling-Lawley")$stats[1,],
# summary(manovares, test = "Roy")$stats[1,]
# )%>%`rownames<-`(c("Pillai", "Wilks", "Hotelling-Lawley", "Roy"))%>%
# as.data.frame() %>%rename(test= Pillai) %>%
# rownames_to_column("Multivariate_Tests") %>%
# p_mark_sig("Pr(>F)")



# odd_rows_df <- df[seq(1, nrow(df), 2), ]
# # 가정: 'df'는 우리가 사용하는 데이터프레임입니다.
# library(dplyr)

# odd_rows_df <- df %>% slice(seq(1, n(), by = 2))


# sumamryres[[1]]%>% tidy()
# rep(c("epoch", "residuals"),4)

#________________________________
# make shorter labels for epochs
Skulls$epoch <- factor(Skulls$epoch, labels=sub("c","",levels(Skulls$epoch)))

Skulls
# longer variable labels
vlab <- c("maxBreadth", "basibHeight", "basialLength", "nasalHeight")
# fit manova model
sk.mod <- lm(cbind(mb, bh, bl, nh) ~ epoch, data=Skulls)
sk.mod
car::Anova(sk.mod)
car::Anova(sk.mod, type=3)
summary(car::Anova(sk.mod, type=2))
summary(car::Anova(sk.mod, type=3))




#------------------
manova(cbind(mb,bh,bl,nh)~ epoch, data=Skulls)
 epoch_means <- model_summary$terms$epoch
  residuals <- model_summary$univariate$Residuals
  df <- c(epoch_means$df[1], residuals$Df[2])
  residual_std_errors <- model_summary$univariate$`Residual standard errors`

manova_result= manova(cbind(mb,bh,bl,nh)~ epoch, data=Skulls)
manova(cbind(mb,bh,bl,nh)~ epoch, data=Skulls)%>%summary()

 summary_result <- summary(manova(cbind(mb,bh,bl,nh)~ epoch, data=Skulls))
  summary_result
  # Extract the names of the dependent variables
  dv_vars <- rownames(summary_result$stats)
  dv_vars
  # Remove 'Residuals' from the dependent variables
  dv_vars <- dv_vars[dv_vars != "Residuals"]
  
  # Loop over each dependent variable
  for (var in dv_vars) {
    # Extract the results for this variable
    var_result <- summary_result$stats[var, ]

  }
var_result
manova_result
manova_result %>% coef()
manova_result%>%str()
manova_result$terms
manova_result$effects
manova_result$df.residual
manova_result$residuals 

?manova
## S3 method for class 'manova'
summary(object,
        test = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy"),
        intercept = FALSE, tol = 1e-7, ...)


        
# tear <- c(6.5, 6.2, 5.8, 6.5, 6.5, 6.9, 7.2, 6.9, 6.1, 6.3,
#           6.7, 6.6, 7.2, 7.1, 6.8, 7.1, 7.0, 7.2, 7.5, 7.6)
# gloss <- c(9.5, 9.9, 9.6, 9.6, 9.2, 9.1, 10.0, 9.9, 9.5, 9.4,
#            9.1, 9.3, 8.3, 8.4, 8.5, 9.2, 8.8, 9.7, 10.1, 9.2)
# opacity <- c(4.4, 6.4, 3.0, 4.1, 0.8, 5.7, 2.0, 3.9, 1.9, 5.7,
#              2.8, 4.1, 3.8, 1.6, 3.4, 8.4, 5.2, 6.9, 2.7, 1.9)
## Example on producing plastic film from Krzanowski (1998, p. 381)
Y <- cbind.data.frame(
  tear=c(6.5, 6.2, 5.8, 6.5, 6.5, 6.9, 7.2, 6.9, 6.1, 6.3,
          6.7, 6.6, 7.2, 7.1, 6.8, 7.1, 7.0, 7.2, 7.5, 7.6), 
   gloss=c(9.5, 9.9, 9.6, 9.6, 9.2, 9.1, 10.0, 9.9, 9.5, 9.4,
           9.1, 9.3, 8.3, 8.4, 8.5, 9.2, 8.8, 9.7, 10.1, 9.2),
    opacity=c(4.4, 6.4, 3.0, 4.1, 0.8, 5.7, 2.0, 3.9, 1.9, 5.7,
             2.8, 4.1, 3.8, 1.6, 3.4, 8.4, 5.2, 6.9, 2.7, 1.9),
   rate = gl(2,10, labels = c("Low", "High")),
   additive = gl(2, 5, length = 20, labels = c("Low", "High")) )
Y
?gl
# Generate factors by specifying the pattern of their levels.

# Usage
# gl(n, k, length = n*k, labels = seq_len(n), ordered = FALSE)
## First control, then treatment:
# gl(2, 8, labels = c("Control", "Treat"))
# ## 20 alternating 1s and 2s
# gl(2, 1, 20)
# ## alternating pairs of 1s and 2s
# gl(2, 2, 20)

# rate     <- gl(2,10, labels = c("Low", "High"))
# additive <- gl(2, 5, length = 20, labels = c("Low", "High"))
?manova
ddd <- lm(cbind(tear, gloss, opacity) ~ rate * additive, data=Y)

coef(ddd)
manova(cbind(tear, gloss, opacity) ~ rate * additive, data=Y)%>%
report_manova(3)


manova(cbind(tear, gloss, opacity) ~ rate, data=Y)%>%
report_manova(3)

manova(cbind(tear, gloss, opacity) ~ rate * additive, data=Y)%>%summary.aov()
# same F statistics as single-df terms
manova(cbind(tear, gloss, opacity) ~ rate * additive, data=Y)%>%summary()
# ANOVA table of Wilks' lambda
manova(cbind(tear, gloss, opacity) ~ rate * additive, data=Y)%>%
summary(test = "Wilks")

#	The name of the test statistic to be used. Partial matching is used so the name can be abbreviated.
test = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy")
manova(cbind(tear, gloss, opacity) ~ rate * additive, data=Y)%>%
summary(test = "Hotelling-Lawley")

summary(car::Anova(lm(cbind(tear, gloss, opacity) ~ rate * additive, data=Y), type=3))
# The summary.manova method uses a multivariate test statistic for the summary table. Wilks' statistic is most popular in the literature, but the default Pillai–Bartlett statistic is recommended by Hand and Taylor (1987).

# The table gives a transformation of the test statistic which has approximately an F distribution. The approximations used follow S-PLUS and SAS (the latter apart from some cases of the Hotelling–Lawley statistic), but many other distributional approximations exist: see Anderson (1984) and Krzanowski and Marriott (1994) for further references. All four approximate F statistics are the same when the term being tested has one degree of freedom, but in other cases that for the Roy statistic is an upper bound.

# The tolerance tol is applied to the QR decomposition of the residual correlation matrix (unless some response has essentially zero residuals, when it is unscaled). Thus the default value guards against very highly correlated responses: it can be reduced but doing so will allow rather inaccurate results and it will normally be better to transform the responses to remove the high correlation.




# References
# Anderson, T. W. (1994) An Introduction to Multivariate Statistical Analysis. Wiley.

# Hand, D. J. and Taylor, C. C. (1987) Multivariate Analysis of Variance and Repeated Measures. Chapman and Hall.

# Krzanowski, W. J. (1988) Principles of Multivariate Analysis. A User's Perspective. Oxford.

# Krzanowski, W. J. and Marriott, F. H. C. (1994) Multivariate Analysis. Part I: Distributions, Ordination and Inference. Edward Arnold.



iris1 <- iris
iris1

set.seed(123)
random1 = sample(1:150, 10)  #150개중 10개 
random2 = sample(1:5, 10, replace = TRUE )  #누락값 열의 값 
random1
random2

#누락시킨 데이터 
for( i in 10) iris1[random1[i], random2[i]]<-NA
iris1[random1,]
iris[random1,]

###
imp_iris = mice::mice(iris1)

imp_iris

imp_iris$imp
imp_iris

irna = iris[1:5,1:4]
irna
rand1 = sample(1:5, 4)  
rand2 = sample(1:5, 4, replace = TRUE )  #누락값 열의 값 
for( i in 4) irna[rand1[i], rand2[i]]<-NA


########
# Missing data 생성 
irna = iris[1:5,1:4]
irna1 <- irna
irna1[5, 1] <- NA  # 5
irna1[2, 2] <- NA  # 3
irna1[4, 3] <- NA  # 1.5
irna1


irna2 = mice::mice(irna1, m=20)
irna2

# install.packages("miceadds")
myirna  = miceadds::mids2datlist(irna2)
myirna[[1]]
myirna[[2]]
myirna[[3]]

# 반복계산 함수 
mi_des_rep = function(myimp){
result  = myimp %>%
          summarise_all(
            .funs = c("mean","sd")
          ) %>%
          tidyr::pivot_longer(cols =  names(.))%>%
          tidyr::separate(name, c("vars", "type"), sep = "_") %>%
          tidyr::pivot_wider(names_from = type, 
                        values_from = value, ) %>%
  data.frame()

  rownames(result) = result$vars
  result = result %>% dplyr::select(-vars)
  return(result)
} 
?heplots::heplot

# mi_des_stat(myirna)
myirna %>% str()
# myirna %>% data.table::rbindlist()

with(myirna, fun = function(data){mi_des_rep(data)})


with(myirna, fun = function(data){mi_des_rep(data)}) %>%
miceadds::withPool_MI() %>% 
tibble::rownames_to_column("vars") %>%
Round(3)

irna%>%summarise_all(.funs = c("mean"))%>%
          tidyr::pivot_longer(cols =  names(.))

bind_cols(
var =  colnames(irna),
mean = irna %>%apply(2, mean),
sd = irna %>%apply(2, sd)) %>% Round(3)









myirna %>% data.table::rbindlist()


#비교 
bind_rows(
raw = irna %>% summarise_all(.funs = c("mean", "sd")),
imp = myirna[[1]] %>% summarise_all(.funs = c("mean", "sd")) )

irna2$imp 
irna2$imp$Sepal.Length %>% as.numeric() %>% mean()
irna2$imp$Sepal.Width %>% as.numeric() %>% mean()
irna2$imp$Petal.Length %>% as.numeric() %>% mean()

irna[5, 1] 
irna[2, 2] 
irna[4, 3] 

irna2$imp%>%summarise_all(list(mean))
irna2$imp %>% lapply(mean)



aov_table <- function(data, 
                        dv_var, 
                        iv_vars,
                        mean = FALSE,
                        unite=FALSE) {
  # data: 데이터 프레임
  # dv_var: 종속변수 컬럼명 (문자열)
  # iv_vars: 독립변수 컬럼명 리스트 (문자열 벡터)
  if(!is.data.frame(data)){
    stop("you need input data.frame")
  }
  # 결과를 저장할 데이터 프레임 초기화
  result_df <- data.frame(Indv_Variable = character(0), 
                          F_Value = numeric(0), 
                          P_Value = numeric(0))
  
  # 각 독립변수별로 ANOVA 분석 수행
  for (iv in iv_vars) {
    anova_result <- aov(formula(paste(dv_var, "~", iv)), 
                          data = data)
    meandata =  jjstat::mysummaryBy(formula(paste(dv_var, "~", iv)), 
                          data = data)[2]      

    tidy_result <- broom::tidy(anova_result)
    levels_paste <- paste0(unique(data[[iv]]), collapse =", " )
    levels <- unique(data[[iv]])

    result_df <- rbind(result_df, 
                   data.frame(
                   iv=iv,
                   dv=dv_var,
                   levels = levels_paste,   # unite
                   level = levels,                   
                   Mean = meandata,                  
                   df1= tidy_result$df[1],
                   df2= tidy_result$df[2],
                   F_Value = tidy_result$statistic[1],
                   P_Value = tidy_result$p.value[1]))
        } #for 
   
    # 결과를 데이터 프레임에 추가
   if(mean){ 
     result_df  = result_df %>%dplyr::select(-levels)

            }else{
  result_df  = result_df %>%dplyr::select(-Mean, -level)
  result_df = dplyr::distinct(result_df, 
              iv, dv, levels, df1, df2, F_Value, P_Value)
  }

result_df = result_df %>% jjstat::p_mark_sig("P_Value", unite=unite)
return(result_df)
 }

# 예시: mtcars 데이터에서 gear, carb를 독립변수로 wt를 종속변수로 분석
aov_table(data = mtcars, dv_var = "mpg", iv_vars = c("cyl", "gear", "carb"))

aov_table(data = mtcars, dv_var = "mpg", iv_vars = c("cyl", "gear", "carb"), mean=T)


jjstat::aov_table(data = mtcars, dv_var = "mpg", iv_vars = c("cyl", "gear", "carb"))

jjstat::aov_table(data = mtcars, dv_var = "mpg", 
                iv_vars = c("cyl", "gear", "carb"),
                mean=TRUE) #%>% jjstat::markdown_table()

distinct(mtcars, cyl)



# formula(paste("wt", "~", "cyl"))
unique(mtcars$cyl)
paste(unique(mtcars$cyl), collapse = ", ")
mtcars %>% str()

jjstat::mysummaryBy(data = mtcars, mpg ~ cyl)

jjstat::mysummaryBy(data = mtcars, mpg ~ cyl, gm = T)

(list(mpg ~ cyl, mpg ~ am))[[1]][3]

jjstat::bind_mysummaryBy(mtcars, 
                        mpg ~ am, 
                        mpg ~ vs, 
                        mpg ~ cyl)

jjstat::bind_mysummaryBy(mtcars, 
                        mpg ~ am, 
                        mpg ~ vs, 
                        mpg ~ cyl, unite = TRUE)

jjstat::mysummaryBy(mpg ~ vs + am, data = mtcars, agg = T)
jjstat::mysummaryBy(mpg ~ vs, data = mtcars, gm = T)
jjstat::mysummaryBy(mpg ~ vs, data = mtcars, gm = F)

jjstat::mysummary(mtcars, "mpg","wt","hp","disp") %>% jjstat::Round(2)
jjstat::mysummary(mtcars, colnames(mtcars)) %>% jjstat::Round(2)

##대비함수 개발 ########################################################

jjstat::mysummaryBy(mpg ~ cyl, data= mtcars)

#실제로 얻어진 자료 

M = c(26.66, 19.74, 15.10)
sd = c(4.51, 1.45, 2.56)
n = c(11, 7, 14)

#contrast 3가지  
cont1 = c(1, -1, 0)
cont2 = c(1, 0, -1)
cont3 = c(0, 1, -1)
#MSW=s_pooled
var.pooled <- sum((n-1)*sd^2)/sum(n-1) 
var.pooled
 #em.mean계산  
cont.mean <- sum(cont1*M)
cont.mean
 #03 표준오차
SE<- sqrt(sum(cont1^2*var.pooled/n))
# SE<- sqrt(sum(cont3^2*var.pooled/n))
SE
#04 t-value
t.stat1 <- cont.mean/SE #계산용 소수점모두 적용
t.stat1
#05 critical t
# df:  (11-1)+ (7-1) + (14-1)
df = 29
critical_t <- abs(qt(0.05/2, df))
critical_t
#t-value에 따른 p-value : lower.tail이라서 양측검정을 위해서 2배를 함.
p.va <- pt(q=t.stat1,df=df,lower.tail = F)*2
p.va
 
 CI.95per <-cbind(round(cont.mean-critical_t*SE,2),
                  round(cont.mean+critical_t*SE,2))
colnames(CI.95per)=c("(low.limit","high.limit)")
rownames(CI.95per)= "Contrast"
CI.95per
 
bonferroni.p <- .05/length(M)
critical.t.bon <- abs(qt(bonferroni.p/2,df)) 
CI.95per.bon <-cbind(round(cont.mean-critical.t.bon*SE,2),
                    round(cont.mean+critical.t.bon*SE,2))
colnames(CI.95per.bon)=c("(low.limit","high.limit)")
rownames(CI.95per.bon)= "Bonferroni adj."
CI.95per.bon

grp = c(paste0("g", 1:3))
M = c(26.66, 19.74, 15.10)
sd = c(4.51, 1.45, 2.56)
n = c(11, 7, 14)
#contrast 3가지  
cont1 = c(1, -1, 0)
cont2 = c(1, 0, -1)
cont3 = c(0, 1, -1)

sum(cont1*M)

str_c(grp, cont1,sep=":" ,collapse= " / ")
paste(grp, cont1,sep=":" ,collapse= " / ")

#Contrast test ###################################
contrast_msdn = function(M, sd, n, 
                      contr=...,
                      digits=3,
                  type="res"){
grp = c(paste0("g", 1:length(M),":"))
# contratst_var = paste0(grp, M,"(", fracture::fracture(contr), sep=")" ,collapse= "/")
contratst_var = paste0(grp, M,"(", contr, sep=")" ,collapse= "/")

#MSW=s_pooled
var.pooled <- sum((n-1)*sd^2)/sum(n-1) 
 #em.mean계산  
c_mean = contr*M 
cont.mean <- sum(contr*M)

 #03 standard error 
SE <- sqrt(sum(contr^2*var.pooled/n))
#04 t-value
t.stat1 <- round(cont.mean/SE , digits)

df = sum(n)-length(n)
critical_t <- round(abs(qt(0.05/2, df)), digits)

#t-value에 따른 p-value : lower.tail이라서 양측검정을 위해서 2배를 함.
# p.value <- round(pt(q = t.stat1, df=df, lower.tail = F)*2, digits+add)
p.value <- pt(q = t.stat1, df=df, lower.tail = F)*2


  #bonferroni.p interval 
bonferroni.p <- .05/length(M)
critical.t.bon <- abs(qt(bonferroni.p/2,df)) 
CI.95per.bon <-cbind(round(cont.mean-critical.t.bon*SE,2),
                    round(cont.mean+critical.t.bon*SE,2))
colnames(CI.95per.bon)=c("adj.lowCI","adj.highCI")
# rownames(CI.95per.bon)= "Bonferroni adj."
# CI.95per = CI.95per.bon

#confidence interval 
CI.95per <-cbind(round(cont.mean-critical_t*SE,2),
                  round(cont.mean+critical_t*SE,2))
colnames(CI.95per)=c("low95CI","high95CI")
# rownames(CI.95per)= "Contrast"


#result 
res = cbind(cont.mean,critical_t, p.value)
rownames(res) = contratst_var
res=res%>% data.frame()%>%
            tibble::rownames_to_column("contr")%>%
             jjstat::p_mark_sig("p.value")
res_ci = cbind(cont.mean,critical_t, p.value, CI.95per)
rownames(res_ci) = contratst_var
res_ci = res_ci%>% data.frame()%>%
            tibble::rownames_to_column("contr")%>%
             jjstat::p_mark_sig("p.value")
res_bon = cbind(cont.mean,critical_t, p.value, CI.95per.bon)
rownames(res_bon) = contratst_var
res_bon = res_bon%>% data.frame()%>%
            tibble::rownames_to_column("contr")%>%
             jjstat::p_mark_sig("p.value")

switch(type, res=res, res_ci=res_ci, res_bon= res_bon)
}


contrast_msdn(M = c(26.66, 19.74, 15.10),
              sd = c(4.51, 1.45, 2.56),
              n = c(11, 7, 14),
              contr = c(1/2, -1/2, 0) )



contrast_msdn(M = c(26.66, 19.74, 15.10),
              sd = c(4.51, 1.45, 2.56),
              n = c(11, 7, 14),
              contr = c(1, -1, 0) , type= "res_ci")
contrast_msdn(M = c(26.66, 19.74, 15.10),
              sd = c(4.51, 1.45, 2.56),
              n = c(11, 7, 14),
              contr = c(1, -1, 0), type="res_bon" )

https://github.com/rossellhayes/fracture/



# contrast_msdn0 <- function(M, sd, n, 
#                           contr = NULL,
#                           digits = 3,
#                           type = "res") {
  
#   # 누적 결과를 담을 빈 데이터프레임 생성
#   result <- data.frame(contr = character(),
#                        cont.mean = numeric(),
#                        critical_t = numeric(),
#                        p.value = numeric(),
#                        sig = character(),
#                        stringsAsFactors = FALSE)
  
#   for (i in 1:length(contr)) {
#     grp <- c(paste0("g", 1:length(M), ":"))
#     contratst_var <- paste0(grp, M, "(", contr[[i]], sep = ")", collapse = "/")
    
#     # MSW=s_pooled
#     var.pooled <- sum((n - 1) * sd^2) / sum(n - 1) 
#     # em.mean 계산  
#     c_mean <- contr[[i]] * M 
#     cont.mean <- sum(contr[[i]] * M)
    
#     # standard error 
#     SE <- sqrt(sum(contr[[i]]^2 * var.pooled / n))
    
#     # t-value
#     t.stat1 <- round(cont.mean / SE , digits)
    
#     df <- sum(n) - length(n)
#     critical_t <- round(abs(qt(0.05 / 2, df)), digits)
    
#     # p-value: lower.tail이라서 양측검정을 위해서 2배를 함.
#     p.value <- pt(q = t.stat1, df = df, lower.tail = FALSE) * 2
    
#     # 결과를 데이터프레임에 추가
#     res <- data.frame(contr = contratst_var,
#                       cont.mean = cont.mean,
#                       critical_t = critical_t,
#                       p.value = p.value,
#                       sig = ifelse(p.value < 0.05, "***", ""),
#                       stringsAsFactors = FALSE)
    
#     # 결과를 누적
#     result <- rbind(result, res)
#   }
  
#   return(result)
# }
################################################################
contrast_msdn <- function(M, sd, n, 
                          ...,
                          digits = 3,
                          type = "res",
                          meanadd=TRUE) {
  


  # Create an empty dataframe to hold the cumulative results
  result <- data.frame(contributes = character(),

                       cont.mean = numeric(),
                       critical_t = numeric(),
                       p.value = numeric(),
                       sig = character(),
                       low_CI = numeric(),
                       high_CI = numeric(),
                       stringsAsFactors = FALSE)
contr = list(...)

  for (i in 1:length(contr)) {
    grp <- c(paste0("g", 1:length(M), ":"))
    
    if(meanadd){
    contratst_var <- paste0(grp, M, "(", contr[[i]], sep = ")", collapse = "/")
    }else{
      contratst_var <- paste0(grp, "(", contr[[i]], sep = ")", collapse = "/")
    }


    # MSW=s_pooled
    var.pooled <- sum((n - 1) * sd^2) / sum(n - 1) 
    # em.mean Calculations  
    c_mean <- contr[[i]] * M 
    cont.mean <- sum(contr[[i]] * M)
    
    # standard error 
    SE <- sqrt(sum(contr[[i]]^2 * var.pooled / n))
    
    # t-value
    t.stat1 <- round(cont.mean / SE , digits)
    
    df <- sum(n) - length(n)
    critical_t <- round(abs(qt(0.05 / 2, df)), digits)
    
    # p-value: Since it is lower.tail, we double it for two-tailed test.
    p.value <- pt(q = t.stat1, df = df, lower.tail = FALSE) * 2
    
    # confidence interval 
    CI <- c(cont.mean - critical_t * SE, cont.mean + critical_t * SE)
    


    # Adding results to a dataframe
    res <- data.frame(
                      contributes = contratst_var,
                      cont.mean = cont.mean,
                      critical_t = critical_t,
                      p.value = p.value,
                      sig = ifelse(p.value < 0.001, "***",
                             ifelse(p.value < 0.01,"**",
                               ifelse(p.value<0.05,"*", ""))),
                      low_CI = CI[1],
                      high_CI = CI[2],
                      stringsAsFactors = FALSE)
    
    # Accumulate results
    result <- rbind(result, res)  
  }
      
   res_basic <- result %>% dplyr::select(1:5)
   res_ci = result
   switch(type, 
            res = res_basic, 
            res_ci = result)


}




contrast_msdn(M = c(26.66, 19.74, 15.10),
              sd = c(4.51, 1.45, 2.56),
              n = c(11, 7, 14),
              c(1, -1, 0) )

contrast_msdn(M = c(26.66, 19.74, 15.10),
              sd = c(4.51, 1.45, 2.56),
              n = c(11, 7, 14),
              c(1, -1, 0) ,
              meanadd = FALSE)

contrast_msdn(M = c(26.66, 19.74, 15.10),
              sd = c(4.51, 1.45, 2.56),
              n = c(11, 7, 14),
              c(1, -1, 0) ,
              meanadd = FALSE,
              type="res_ci")

 
contrast_msdn(M = c(26.66, 19.74, 15.10),
               sd = c(4.51, 1.45, 2.56),
               n = c(11, 7, 14),
                c(1, -1,  0), 
                 c(1,  0, -1), 
               c(0,  1, -1)
               )
contrast_msdn(M = c(26.66, 19.74, 15.10),
               sd = c(4.51, 1.45, 2.56),
               n = c(11, 7, 14),
                contr1 = c(1, -1,  0), 
                contr2= c(1,  0, -1), 
                contr3= c(0,  1, -1),
                meanadd=F
               )
contrast_msdn(M = c(26.66, 19.74, 15.10),
               sd = c(4.51, 1.45, 2.56),
               n = c(11, 7, 14),
                contr1 = c(1, -1,  0), 
                contr2= c(1,  0, -1), 
                contr3= c(0,  1, -1),
                meanadd=FALSE, type="res_ci"
               )

list(  contr1=c(1, -1,  0), 
                contr2= c(1,  0, -1), 
                contr3= c(0,  1, -1))%>%names()


#error 
contrast_msdn(M = c(26.66, 19.74, 15.10),
              sd = c(4.51, 1.45, 2.56),
              n = c(11, 7, 14),
              contr = list(c(1, -1,  0),
                           c(1,  0, -1),
                           c(0,  1, -1)
               ))


# list(  c(1, -1,  0), 
#         c(1,  0, -1), 
#        c(0,  1, -1))%>%names()




# list(c(1, -1,  0),  c(1,  0, -1), c(0,  1, -1)) %>%length()
# c(1, -1,  0) %>%length()

# list(list(c(1, -1,  0),  c(1,  0, -1), c(0,  1, -1))) %length()


list(list(c(1, -1,  0), c(1,  0, -1),  c(0,  1, -1)))

do.call(rbind, list(c(1, -1,  0), c(1,  0, -1),  c(0,  1, -1)) )
# list(c(1, -1,  0), c(1,  0, -1),  c(0,  1, -1)) %>% do.call(what= rbind)








aov(mpg ~ cyl, mtcars)%>% summary()              

emmeans::emmeans(aov(mpg ~ factor(cyl), mtcars), "cyl")
emmeans::emmeans(aov(mpg ~ factor(cyl), mtcars), "cyl")%>%
emmeans::contrast(c(1,-1,0))


#F검증 
# # Define a function to perform ANOVA using sample size, mean, and standard deviation
# contrast_msdn1 <- function(M, sd, n) {
#   # Calculate the variance for each group
#   group_var <- sd^2
  
#   # Calculate the total sample size
#   total_n <- sum(n)
  
#   # Calculate the overall mean
#   total_mean <- sum(M * n) / total_n
  
#   # Calculate the total sum of squares (SS)
#   total_ss <- sum((n - 1) * group_var)
  
#   # Calculate the total degrees of freedom (df)
#   total_df <- total_n - 1
  
#   # Calculate the between-group sum of squares (SS)
#   between_group_ss <- sum(n * (M - total_mean)^2)
  
#   # Calculate the between-group degrees of freedom (df)
#   between_group_df <- length(M) - 1
  
#   # Calculate the within-group sum of squares (SS)
#   within_group_ss <- total_ss - between_group_ss
  
#   # Calculate the within-group degrees of freedom (df)
#   within_group_df <- total_df - between_group_df
  
#   # Calculate the mean square between (MSB)
#   msb <- between_group_ss / between_group_df
  
#   # Calculate the mean square within (MSW)
#   msw <- within_group_ss / within_group_df
  
#   # Calculate the F-statistic
#   f_statistic <- abs(msb / msw)
  
#   # Calculate the p-value
#   p_value <- 1 - pf(f_statistic, between_group_df, within_group_df)
  
#   # Return the F-statistic and p-value
#   return(list(F_statistic = f_statistic,
#            p_value = p_value))
# }

# # Perform ANOVA
# contrast_msdn1(M=c(26.66, 19.74, 15.10), 
#               sd= c(4.51, 1.45, 2.56), 
#              n =c(11, 7, 14))

# # Print ANOVA results
# cat("F-statistic:", result$F_statistic, "\n")
# cat("p-value:", result$p_value, "\n")





#MSW=s_pooled
var.pooled <- sum((n-1)*sd^2)/sum(n-1) 
var.pooled
 #em.mean계산  
cont.mean <- sum(cont1*M)
cont.mean
 #03 표준오차
SE<- sqrt(sum(cont1^2*var.pooled/n))
# SE<- sqrt(sum(cont3^2*var.pooled/n))
SE
#04 t-value
t.stat1 <- cont.mean/SE #계산용 소수점모두 적용
t.stat1
#05 critical t
# df:  (11-1)+ (7-1) + (14-1)
df = 29
critical_t <- abs(qt(0.05/2, df))
critical_t
#t-value에 따른 p-value : lower.tail이라서 양측검정을 위해서 2배를 함.
p.va <- pt(q=t.stat1,df=df,lower.tail = F)*2
p.va
 
 CI.95per <-cbind(round(cont.mean-critical_t*SE,2),
                  round(cont.mean+critical_t*SE,2))
colnames(CI.95per)=c("(low.limit","high.limit)")
rownames(CI.95per)= "Contrast"
CI.95per
 
bonferroni.p <- .05/length(M)
critical.t.bon <- abs(qt(bonferroni.p/2,df)) 
CI.95per.bon <-cbind(round(cont.mean-critical.t.bon*SE,2),
                    round(cont.mean+critical.t.bon*SE,2))
colnames(CI.95per.bon)=c("(low.limit","high.limit)")
rownames(CI.95per.bon)= "Bonferroni adj."
CI.95per.bon
































#######################################
rdata = data.frame(
  # id= 1:5,
  Constant = c(12,13,11,12,12),
  Frequent = c(9,10,9,13,14),
  Infrequent = c(15,16,17,16,16),
  Never =c(17,18,12,18,20)
)
rdata
# rdata%>% rowid_to_column("id")
# rdata %>% mysummary(c("Constant", "Frequent","Infrequent","Never"))
rdata %>% mysummary(colnames(.))%>% select(1:4) %>%jjstat::Round(2)

library(tidyverse)
rdata_long<- rdata%>%pivot_longer(names_to="grp", values_to="dv", cols=Constant:Never)
rdata_long %>%arrange(grp)

summary(aov(dv ~ grp, rdata_long))
# Pooles variance 3.88

#1 contrast coefficient 설정 
cont1=c(1,-1/3,-1/3, -1/3)
cont2=c(0, 1,  -1/2, -1/2)
cont3=c(0, 0,     1, -1)
cont1;cont2;cont3


#2 orthogonal check
sum(cont1*cont2)
sum(cont1*cont3)
sum(cont2*cont3)
#condot <-sum(sum(cont1*cont2)+sum(cont1*cont3)+sum(cont2*cont3))


#3 contrast value인 mean 구하기 cont.mean
cont.mean1 <- sum(cont1*M);cont.mean1
cont.mean2 <- sum(cont2*M);cont.mean2
cont.mean3 <- sum(cont3*M);cont.mean3

cont.mean1;cont.mean2;cont.mean3



#3pooled var. MSW
summary(aov(dv~grp, ct))
# MSW <- 3.88

#실제 계산하기 
var.pooled <- sum((n-1)*sd^2)/sum(n-1)
round(var.pooled ,2)


#SE (표준오차) 구하기 
SE1<- sqrt(var.pooled*sum(cont1^2/n));SE1
SE2<- sqrt(var.pooled*sum(cont2^2/n));SE2
SE3<- sqrt(var.pooled*sum(cont3^2/n));SE3

SE1;SE2;SE3


#t-value구하기 
t1 =cont.mean1 /SE1 ;t1
t2 =cont.mean2 /SE2 ;t2
t3 =cont.mean3 /SE3 ;t3

t1;t2;t3

#자유도 구하기 
df<-sum((n-1))
df
#05 유의성 판정 데이터
critical_t <- abs(qt(0.05/2,df))#critical t value
critical_t
#임계치와 비교하여 t-value가 임계치보다 크면 유의함
#sig <-ifelse(abs(qt(.05/2,df))< abs(t.stat1),"'sig'","Not sig(include '0'")
ifelse(abs(qt(.05/2,df))< abs(t1),"'sig'","Not sig(include '0')")
ifelse(abs(qt(.05/2,df))< abs(t2),"'sig'","Not sig(include '0')")
ifelse(abs(qt(.05/2,df))< abs(t3),"'sig'","Not sig(include '0')")

#t-value에 따른 p-value : lower.tail이라서 양측검정을 위해서 2배를 함.
p.va1 <- pt(q=abs(t1),df=df,lower.tail = F)*2
star1 <- ifelse(p.va1 <.001,"***",ifelse(p.va1<.01,"**",ifelse(p.va1<.05,"*","not sig.")))

p.va2 <- pt(q=abs(t2),df=df,lower.tail = F)*2
star2 <- ifelse(p.va2 <.001,"***",ifelse(p.va2<.01,"**",ifelse(p.va2<.05,"*","not sig.")))

p.va3 <- pt(q=abs(t3),df=df,lower.tail = F)*2
star3 <- ifelse(p.va3 <.001,"***",ifelse(p.va3<.01,"**",ifelse(p.va3<.05,"*","not sig.")))

p.va1;star1; p.va2;star2; p.va3;star3
#신뢰구간(confidence Interval) :cont.mean ± critical_t*SE
CI.95per1 <-cbind(round(cont.mean1-critical_t*SE1,2),round(cont.mean1+critical_t*SE1,2))
colnames(CI.95per1)=c("(low.limit","high.limit)")
rownames(CI.95per1)= "Contrast.1"

CI.95per2 <-cbind(round(cont.mean2-critical_t*SE2,2),round(cont.mean2+critical_t*SE2,2))
colnames(CI.95per2)=c("(low.limit","high.limit)")
rownames(CI.95per2)= "Contrast.2"

CI.95per3 <-cbind(round(cont.mean3-critical_t*SE3,2),round(cont.mean3+critical_t*SE3,2))
colnames(CI.95per3)=c("(low.limit","high.limit)")
rownames(CI.95per3)= "Contrast.3"


CI.95per1;CI.95per2;  CI.95per

#sum of square
SSB1 <- cont.mean1^2/sum(cont1^2/n)
SSB2 <- cont.mean2^2/sum(cont2^2/n)
SSB3 <- cont.mean3^2/sum(cont3^2/n)
SSB1;SSB2;SSB3


SSB<-SSB1+SSB2+SSB3
SSB
#F-value
F1=SSB1/MSW
F2=SSB2/MSW
F3=SSB3/MSW
F1;F2;F3

Fb=(F1+F2+F3)/3
Fb

dfB=length(M)-1
dfB
df


F.pval<- pf(Fb,dfB,df, lower.tail = F)
starF <- ifelse(F.pval <.001,"***",ifelse(F.pval<.01,"**",ifelse(F.pval<.05,"*","not sig.")))

F1.pval1<- pf(F1,1,df, lower.tail = F)
starF1 <- ifelse(F1.pval1 <.001,"***",ifelse(F1.pval1<.01,"**",ifelse(F1.pval1<.05,"*","")))
F1.pval1
F2.pval2<- pf(F2,1,df, lower.tail = F)
starF2 <- ifelse(F2.pval2 <.001,"***",ifelse(F2.pval2<.01,"**",ifelse(F2.pval2<.05,"*","")))

F3.pval3<- pf(F3,1,df, lower.tail = F)
starF3 <- ifelse(F3.pval3 <.001,"***",ifelse(F3.pval3<.01,"**",ifelse(F3.pval3<.05,"*","")))



#ANOVA table 
Between.grp =cbind(SSB,dfB, round(SSB/dfB,2),round(Fb,2),round(F.pval,4),starF)

Between.grp
#colnames(Between.grp)=c("SS","df","Mean Sq","F","p","")
rownames(Between.grp)="Between.grp"
Between.grp

aov.x1 <-cbind(round(SSB1,2),1,round(SSB1/1,2),round(F1,2),round(F1.pval1 ,4),starF1)
rownames(aov.x1)="aov.x1"
aov.x1


aov.x2<-cbind(round(SSB2,2),1,round(SSB2/1,2),round(F2,2),round(F2.pval2,4),starF2)
rownames(aov.x2)="aov.x2"
aov.x2


aov.x3 <-cbind(round(SSB3,2),1,round(SSB3/1,2),round(F3,2),round(F3.pval3,4),starF3)
rownames(aov.x3)="aov.x3"
aov.x3

#within data는 pooled variance로 구한다. 
SSW= var.pooled *df
SSW


Residuals =cbind(SSW,df,var.pooled,"","","")
rownames(Residuals)="Residuals"
Residuals


#최종결과 
ANOVA =rbind(Between.grp,aov.x1,aov.x2,aov.x3,Residuals )
colnames(ANOVA)=c("SS","df","Mean Sq","F","p","")
ANOVA %>% kable("pandoc")

 1.311/1.618
-0.366/-2.732
# 사교육 비용 
jjstat::ztest(1.311, 1.311/1.618, 1.311-0.366,  (1.311-0.366)/-2.732)

#사교육 참여 
jjstat::ztest(5.714, 5.714/8.245, 5.714+0.295,  (5.714+0.295)/2.822)
# jjstat::ztest(5.714, 5.714/8.245, 0.295,  0.295/2.822)

#사교육 시간
jjstat::ztest(-0.795,-0.795/-1.138, -0.795+0.149,  (-0.795+0.149)/1.344)

wald_test <- function(b1, se1, b2, se2, est1=NULL, est2=NULL){
  library(broom)
  t1 = b1/se1
  t2 = b2/se2

  z = (b1 - b2)/ sqrt(se1^2 + se2^2)
  p = 2*(1-pnorm(abs(z)))
  
  var1 = paste0(est1,"_est1 = ", b1,", se = ", round(se1, 3), ", t = ", t1,"." )
  var2 = paste0(est2,"_est2 = ", b2,", se = ", round(se2, 3), ", t = ", t2,"." )
  
   res = broom::tidy(c( 
     z.value = z, 
     pvalue = p))
  colnames(res)=c("statistics","value")

  res = res%>%mutate(coef=c(var1, var2))
  res
  # list(res, var1, var2)
}
wald_test(-0.795,-0.795/-1.138, -0.795+0.149,  (-0.795+0.149)/1.344)
wald_test(-0.795,-0.795/-1.138,-0.795+0.149,  (-0.795+0.149)/1.344,est1="남성",est2="여성")
wald_test(1.311, 1.311/1.628, 1.311-0.366,  (1.311-0.366)/-2.732)


wald_test2 <- function(b1, t1, b2, t2, 
                    est1 = NULL, est2=NULL, 
                    gender = FALSE){
  library(broom)
  if(gender){

    se1 = b1/t1
    se2 = (b1+b2)/t2    
    B1 = b1
    B2 = b1+b2
    
    z = (B1 - B2)/ sqrt(se1^2 + se2^2)
    p = 2*(1-pnorm(abs(z)))

  var1 = paste0(est1,"_est1(intercept) = ", b1,", se = ", 
                round(se1, 3), ", t = ", t1,"." )
  var2 = paste0(est2,"_est2","(",b2,") = ", B2,", se = ", 
                round(se2, 3), ", t = ", t2,"." )
  

    res = broom::tidy(c(
            z.value = z, 
      pvalue = p))
    colnames(res)=c("statistics","value")
  res = res%>%mutate(coef=c(var1, var2))


  }else{
  se1 = b1/t1
  se2 = b2/t2

  z = (b1 - b2)/ sqrt(se1^2 + se2^2)
  p = 2*(1-pnorm(abs(z)))
  
  var1 = paste0(est1,"_est1 = ", b1,", se = ", round(se1, 3), ", t = ", 
      round(t1, 3),"." )
  var2 = paste0(est2,"_est2 = ", b2,", se = ", round(se2, 3), ", t = ", 
      round(t2, 3),"." )
  
   res = broom::tidy(c( 
     z.value = z, 
     pvalue = p))
  colnames(res)=c("statistics","value")

  res = res%>%mutate(coef=c(var1, var2))
  }
  
  res

}
wald_test2(-0.795,8.245, -0.795+0.149,  (-0.795+0.149)/1.344)
wald_test2(-0.795,8.245, -0.795+0.149,  (-0.795+0.149)/1.344, gender=TRUE)
wald_test2(-0.795,-0.795/-1.138,-0.795+0.149,  (-0.795+0.149)/1.344,est1="남성",est2="여성")


wald_test(1.311, 1.311/1.628, 1.311-0.366,  (1.311-0.366)/-2.732)
##gender variable 
##first method
wald_test2(1.311, 1.628, 1.311-0.366,  -2.732)
##second method
wald_test2(1.311, 1.628, -0.366,  -2.732, gender=T)
wald_test2(1.311, 1.628, -0.366,  -2.732,"male","female", gender=T)


?chisq.test()
