rop <- function(left,op,right){
    ## Relational operators between poker hands
    capture.output(if (class(left) == 'hand' & class(left) == 'hand'){
                       bl <- left
                       br <- right
                   } else {bl <- poker.strongest(left); br <- poker.strongest(right)}, file='NUL')
    return(eval(parse(text = paste('(bl$level*100 + bl$high)', op,
                                   '(br$level*100 + br$high)'))))
}

'%>%'<- function(left,right){rop(left,'>',right)}
'%<%'<- function(left,right){rop(left,'<',right)}
'%<=%'<- function(left,right){rop(left,'<=',right)}
'%>=%'<- function(left,right){rop(left,'>=',right)}
'%==%'<- function(left,right){rop(left,'==',right)}
'%!=%'<- function(left,right){rop(left,'!=',right)}
