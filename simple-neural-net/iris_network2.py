#It goes without saying that much of this code is reused directly from the
#tutorial code, only the necessary sections were modified
from keras.models import Sequential
from keras.layers import Dense
import numpy
#load iris training data
dataset = numpy.loadtxt("iris_train.csv", delimiter=",")
# split into input (X) and output (Y) variables
X = dataset[:,0:4]
Y = dataset[:,4:]
#load the iris test data
dataset_test = numpy.loadtxt("iris_test.csv", delimiter=",")
# split into input (A) and output (B) variables
A = dataset_test[:,0:4]
B = dataset_test[:,4:]
# create model
model = Sequential()
model.add(Dense(4, input_dim=4, activation='relu'))
model.add(Dense(24, activation='relu'))
model.add(Dense(3, activation='softmax'))
# Compile model
model.compile(loss='binary_crossentropy', optimizer='adam', metrics=['accuracy'])
# Fit the model
model.fit(X, Y, epochs=1500, batch_size=10)
# evaluate the model
scores = model.evaluate(X, Y)
print("\n%s: %.2f%%" % (model.metrics_names[1], scores[1]*100))
#Run the model the model on our test data
predictions = model.predict(A)
print("Neural network predictions (for test data):")
i = 0
correct = []
confidence_avg = 0
for row in predictions:
    format_row = ["%.8f" % member for member in row]
    confidence_avg += numpy.max(row)
    if(numpy.argmax(row) == numpy.argmax(B[i])):
        correct.append(True)
    else:
        correct.append(False)
    print("Prediction: %s ||| Actual: %s ||| Correct?: %s\n" % (format_row, B[i], correct[i]))
    i += 1
accuracy = correct.count(True)/len(correct)
print("Accuracy: %.2f%%\nAverage Confidence: %.2f%%" % (accuracy*100, (confidence_avg/len(predictions))*100))
