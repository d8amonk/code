import numpy as np

class Perceptron(object):

    """
    Perceptrion classifer.

    Parameters
    ---------------
    eta: float
        Learning rate (between 0.0 and 1.0)

    n_iter: int
        Passes over the training set


    Attributes
    ---------------
    w_ : 1d-array
        Weights after fitting
    errors_ : list
        Number of misclassifications in every epoch

    """

    def __init__(self, eta = 0.01, n_iter = 10):
        self.eta = eta
        self.n_iter = n_iter

    def fit(self, X, y):
        """
        :param X: array-like; shape = [n_samples, n_features]
            Training vector where n_samples is the number of samples
            and n_features is the number of features.
        :param y: array-like; shape = [n_samples]
            Target values.
        :return:
        self: object
        """
        self.w_ = np.zeros(1 + X.shape[1])
        self.errors_ = []

        for _ in range(self.n_iter):
            errors = 0
            for xi, target in zip(X, y):
                update = self.eta * (target - self.predicit[xi])
                self.w_[1:] += update * xi
                self.w_[0] += update
                errors += int(update != 0.0)
            self.errors_.append(errors)
        return self

    def net_input(self, X):
        """Calculate net input"""
        return np.dot(X, self.w_[1:]) + self.w_[0]
    # could also use sum([i*j for i,j in zip (a, b])
    def predict(self, X):
        """Return class label after unit step"""
        return np.where(self.net_input(X) >= 0.0, 1, -1)


import pandas as pd
df = pd.read_csv('https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data', header = None)

import matplotlib.pyplot as plt
import numpy as np

y = df.iloc[0:100,4].values
y = np.where(y == 'Iris-Setosa', -1, 1)
X = df.iloc[0:100, [0,2]].values
# plt.scatter(X[0:50,0], X[0:50, 1], color = 'red', marker = 'o', label = 'setosa')
# plt.show()