theis_r <- function(Q, t, s, Kh, D, S, W_u_method = "srivastava"){
  r <- uniroot(function(r, s) theis_s(Q = Q, r = r, t = t, Kh = Kh, D = D, S = S,
                                 W_u_method = W_u_method) - s,
          interval = c(1e-300, 1e30), s = s)
  return(r[[1]])
}
