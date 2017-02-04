# from https://cran.r-project.org/web/packages/sodium/vignettes/crypto101.html

# 8bit by 8bit matrix :D
# aka 1byte by 1byte matrix :D
matrix(as.raw(0:255), byrow = FALSE, ncol = 16)

# XOR two (8bit) bytes 'x' and 'y'
x <- as.raw(0x7a)
y <- as.raw(0xe4)
z <- base::xor(x, y)
dput(z)

# Show the bits in each byte
cbind(x = rawToBits(x), 
      y = rawToBits(y), 
      z = rawToBits(z))

# In cryptography we xor a message x with secret 
# random data y. Because each bit in y is randomly 
# true with probability 0.5, the xor output is 
# completely random and uncorrelated to x. 
# This is called perfect secrecy. Only if we 
# know y we can decipher the message x.

# Encrypt message using random one-time-pad
require(gam)

# Encrypt message using random one-time-pad
msg <- charToRaw("TTIP is evil")
one_time_pad <- random(length(msg))
ciphertext <- base::xor(msg, one_time_pad)

# It's really encrypted
rawToChar(ciphertext)
# should return
# [1] "(8\xd7??%\u035f\x81\xbb\023\xa2"

# Decrypt with same pad
rawToChar(base::xor(ciphertext, one_time_pad))
# should return
# [1] "TTIP is evil"