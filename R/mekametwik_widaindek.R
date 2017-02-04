jMat <- outer(as.character(1:8), as.character(1:13), function(x, y) {
  paste0("x", x,",", y)
})
jMat