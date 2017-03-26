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
                update = self.eta * (target - self.predict(xi))
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
# plt.scatter(X[50:100,0], X[50:100, 1], color = 'blue', marker = 'x', label = 'versicolor')
# plt.xlabel('sepal length')
# plt.ylabel('pental length')
# plt.legend(loc = 'upper left')
# plt.show()

ppn = Perceptron(eta = 0.1, n_iter = 10)
ppn.fit(X, y)
# plt.plot(range(1, len(ppn.errors_) + 1), ppn.errors_, marker = 'o')
# plt.xlabel('Epocs')
# plt.ylabel('Number of Misclassifications')
# plt.show()

# convenience function to visualizae a decision boundary for 2D data
from matplotlib.colors  import ListedColormap
def plot_decision_regions(X, y, classifier, resolution = 0.02):
    # setup marker generator and color map
    markers = ('s','x','p','^','v')
    colors = ('red', 'blue', 'lightgreen', 'gray', 'cyan')
    cmap = ListedColormap(colors[:len(np.unique(y))])

    # plot the decision surface
    x1_min, x1_max = X[:,0].min() - 1, X[:,0].max() + 1
    x2_min, x2_max = X[:,1].min() - 1, X[:.1].max() + 1

    xx1, xx2 = np.meshgrid(np.arange(x1_min, x1_max, resolution), np.arange(x2_min, x2_max, resolution))
    Z = classifier.predict(np.array([xx1.ravel(), xx2.ravel()]).T)
    Z = Z.reshape(xx1.shape)
    plt.contourf(xx1, xx2, Z, alpha = 0.4, cmap = cmap)
    plt.xlim(xx1.min(), xx1.max())
    plt.ylim(xx2.min(), xx2.max())

    # plot the class samples
    for idx, cl in enumerate(np.unique(y)):
        plt.scatter(x = X[y == cl, 0], y = X[y == cl, 1],
                    alpha = 0.8, c = cmap(idx),
                    marker = markets[idx], label = cl)

# plot_decision_regions(X, y, classifier = ppn)
# plt.xlabel('sepal length (cm)')
# plt.ylabel('petal length (cm)')
# plt.legend(loc = 'upper left')
# plt.shot()


