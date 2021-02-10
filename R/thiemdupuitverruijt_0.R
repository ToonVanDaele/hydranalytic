## ThiemDupuitVerruijt_0

ThiemDupuitVerruijt_0 <- function(r, r0, Q, D, Kh, Rech, s){
  s_test <- ThiemDupuitVerruijt(r = r, r0 = r0, Q = Q, D = D, Kh = Kh, Rech = Rech)
  s0 <- s - s_test
  return(s0)
}
