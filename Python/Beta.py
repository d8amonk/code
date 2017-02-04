import pandas as pd
import numpy as np
# from np import *
# from pd import *

s=(10,6)
M = pd.DataFrame(np.zeros(s))

cnames = ["x","y","(Xi-Xbar)","(Yi-Ybar)","(Xi-Xbar)^2","(Yi-Ybar)^2"]
M.columns = cnames

M["x"] = np.random.randint(1,100,size=10)
M["y"] = np.random.randint(1,100,size=10)

x = M["x"]
y = M["y"]

mean_x = np.mean(x)
mean_y = np.mean(y)

M["(Xi-Xbar)"] = x-mean_x #dev xi from mean x
M["(Yi-Ybar)"] = y-mean_y #dev yi from mean y

d_x = sum(M['(Xi-Xbar)'])
d_y = sum(M['(Yi-Ybar)'])

M["(Xi-Xbar)^2"] = (M["x"]-mean_x)**2
M["(Yi-Ybar)^2"] = (M["y"]-mean_y)**2

v_x = np.var(x)
st_x = np.std(x)
v_y = np.var(y)
st_y = np.std(y)

sum_xy = sum((M["(Xi-Xbar)"]*M["(Yi-Ybar)"]))
sum_x = sum(M["(Xi-Xbar)^2"])

#notice that you can't do a_hat without having done b_hat FIRST!
b_hat = sum_xy/sum_x
a_hat = mean_y - b_hat * mean_x

M.to_csv("C:\\Users\\Jeffrey\\Google Drive\\Code\\data\\beta.csv", index="FALSE")

b_hat
a_hat

