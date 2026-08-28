import numpy as np
import matplotlib.pyplot as plt
import tensorflow as tf
from tensorflow.keras import layers, models

# 1. Load and Normalize Data
(x_train, y_train), (x_test, y_test) = tf.keras.datasets.cifar10.load_data()
x_train, x_test = x_train / 255.0, x_test / 255.0

# 2. Build model
model = models.Sequential(
    [
        layers.Conv2D(32, (3, 3), activation="relu", input_shape=(32, 32, 3)),
        layers.MaxPooling2D((2, 2)),
        layers.Conv2D(64, (3, 3), activation="relu"),
        layers.MaxPooling2D((2, 2)),
        layers.Flatten(),
        layers.Dense(64, activation="relu"),
        layers.Dense(10, activation="softmax"),
    ]
)

model.compile(
    optimizer="adam", loss="sparse_categorical_crossentropy", metrics=["accuracy"]
)

# 3. Train
model.fit(x_train, y_train, epochs=10, validation_data=(x_test, y_test))

# Let's test with the first N images
N = 4
class_names = [
    "airplane",
    "automobile",
    "bird",
    "cat",
    "deer",
    "dog",
    "frog",
    "horse",
    "ship",
    "truck",
]
images = x_train[:N]
predictions = model.predict(images)

predicted_labels = [class_names[i] for i in np.argmax(predictions, axis=1)]
print(f"Model Prediction: {predicted_labels}")

actual_labels = [class_names[i] for i in y_train[:4, 0]]
print(f"Actual Label:     {actual_labels}")

plt.figure()
for i in range(4):
    plt.subplot(2, 2, i + 1)
    plt.imshow(images[i])
    plt.axis(False)
plt.show(block=False)
input("Press Key")