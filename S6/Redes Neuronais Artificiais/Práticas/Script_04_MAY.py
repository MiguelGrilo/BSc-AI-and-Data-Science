import numpy as np
import tensorflow as tf
from tensorflow.keras import layers, models
import matplotlib.pyplot as plt

# Load the MNIST dataset
mnist = tf.keras.datasets.mnist
(x_train, y_train), (x_test, y_test) = mnist.load_data()

x_train, x_test = x_train / 255.0, x_test / 255.0

# Gray pixels are integers from 0 to 255.
# Normalize values to be floats between 0 and 1.
# (This helps the model converge faster)
plt.figure()
for i in range(30):
    plt.subplot(6, 5, i+1)
    plt.imshow(x_test[i], cmap='gray')
    plt.axis(False)
plt.show(block=False)

# ...
# elu (exponential linear unit)
model = models.Sequential([
    layers.Input(shape=(28, 28)),

    # Flaten the 28x28 images into a 1D vector of 78 ...
    layers.Flatten(),

    # Hidden layer with 128 neurons and ReLU activation
    layers.Dense(128, activation='relu'),

    # Dropout layer to prevent overfitting (at each i ...
    # off 20% of neurons from previous layer)
    layers.Dropout(0.2),

    # Output layer with 10 neurons (one for each digit ...)
    # Softmax turns the output into probabilities
    layers.Dense(10, activation='softmax')
])
model.summary()

# Compile the model
# 'adam' is the most common method. 'sgd' is the clas ...
# 'sparse_categorical_crossentropy' and 'sparse_categorical_...'
# the difference being how the outputs are formatted: ...
# 'accuracy' is #correct / #total
model.compile(optimizer='adam',
              loss='sparse_categorical_crossentropy',
              metrics=['accuracy'])

# Train the model for 5 epochs
# 1 epoch corresponds to a complete pass over the tra...
# this case.
model.fit(x_train, y_train, epochs=5)

# Evaluate performance
test_loss, test_acc = model.evaluate(x_test, y_test, verbose=2)
print(f'\nTest Accuracy: {test_acc*100}')

# ---------------------
# Select 30 images from the test set
images = x_test[:30]
actual_labels = y_test[:30]

# Make the predictions for the selected images
predictions = model.predict(images)

# Prediction are arrays of 10 probabilities.
# argmax picks the max along axis 1 (returns max for each image)
predicted_labels = np.argmax(predictions, axis=1)

print(f"Model Prediction: {predicted_labels}")
print(f"Actual Label: {actual_labels}")
print(f"Correct: {sum}")