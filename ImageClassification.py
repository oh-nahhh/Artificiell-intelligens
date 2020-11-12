#Assignment 4
#Niklas Bergqvist
#Group 29

#Supress warnings if you will
import warnings
#warnings.simplefilter(action='ignore', category=FutureWarning)
#warnings.filterwarnings("ignore")

#Load libraries
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split

import tensorflow as tf
from tensorflow.keras import datasets, layers, models, losses, metrics, optimizers

#Load image data from the home folder
test = pd.read_csv("test.csv")
data = pd.read_csv("train.csv")

#print(data.head())

#Split dataset into training and validation set
train, valid = train_test_split(data, test_size=0.20,random_state=0,)

#Loading the labels
X_test = np.array(valid.drop(['label'], axis=1))
X_train = np.array(train.drop(['label'], axis=1))
y_train = np.array(train['label'])
y_test = np.array(valid['label'])

#Reshaping into the right dimensions
X_train = np.reshape(X_train, (X_train.shape[0], 28, 28))
X_test = np.reshape(X_test, (X_test.shape[0], 28, 28))
X_train = np.reshape(X_train, (X_train.shape[0], 28, 28, 1))
X_test = np.reshape(X_test, (X_test.shape[0], 28, 28, 1))

#Normalize the image data between 0 and 1
X_train = X_train / 255
X_test = X_test / 255

#Define the keras model
#Convolutional neural networks are very good at picking up on patterns in images
model = models.Sequential()
#Apply The rectified linear activation (relu) which is a function that is a piecewise
#linear function that will output the input directly if it is positive, otherwise, it will
#it will output zero
model.add(layers.Conv2D(64, (3, 3), activation='relu', input_shape=(28, 28, 1)))
model.add(layers.Conv2D(64, (3, 3), activation='relu'))
#Apply MaxPooling2D layer that downsamples the input representation by taking the maximum value
#over the window defined by pool_size for each dimension along the features axis.
model.add(layers.MaxPool2D(pool_size=(2, 2)))
model.add(layers.Conv2D(64, (3, 3), activation='relu'))
model.add(layers.Conv2D(64, (3, 3), activation='relu'))
model.add(layers.MaxPool2D(pool_size=(2, 2)))
#Flatten the model by remove all of the dimensions except for one.
model.add(layers.Flatten())
model.add(layers.Dense(512, activation='relu'))
#Dropout layer to combat overfitting
model.add(layers.Dropout(0.5))
#Add a softmax layer to translate the numbers into a probability distribution.
model.add(layers.Dense(10, activation='softmax'))
#Adam is an adaptive learning rate method, which means, it computes individual
#learning rates for different parameters. Its name is derived from adaptive moment estimation
#Adam can be looked at as a combination of RMSprop and Stochastic Gradient Descent with momentum
model.compile(loss='sparse_categorical_crossentropy', optimizer='adam', metrics=['accuracy'])
#Fit the keras model on the dataset for 5 epochs
model.fit(X_train, y_train, batch_size=32, epochs=5, validation_data=(X_test, y_test))

# print the accuracy score
score = model.evaluate(X_test, y_test, verbose=0)
print('Accuracy score:', score[1])

#print(test.head())

# make predictions
x_test = np.array(test)
x_test = np.reshape(x_test, (x_test.shape[0], 28, 28))
x_test = x_test.astype('float32')
x_test = np.reshape(x_test, (x_test.shape[0], 28, 28, 1))

pred = model.predict(x_test)

df = pd.DataFrame({"ImageID": np.arange(1, 14001), "Label": np.argmax(pred, axis=1)})
df.to_csv("Submission.csv", index=False)
