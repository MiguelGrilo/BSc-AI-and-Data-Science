import numpy as np
import tensorflow as tf
from tensorflow.keras import layers, models
from tensorflow.keras.callbacks import EarlyStopping
import matplotlib.pyplot as plt

# -------
# ANNs
# -------
fashion_mnist = tf.keras.datasets.fashion_mnist
(x_train, y_train), (x_test, y_test) = fashion_mnist.load_data()
class_names = ["T-shirt/top", "Trouser", "Pullover", "Dress", 
               "Coat", "Sandal", "Shirt", "Sneaker", 
               "Bag", "Ankle boot"]

x_train, x_test = x_train / 255.0, x_test / 255.0

plt.figure()
for i in range(30):
    plt.subplot(6, 5, i+1)
    plt.imshow(x_test[i], cmap='gray')
    plt.axis(False)
plt.show(block=False)

# Mais neurónios e 3 camadas ocultas
model = models.Sequential([
    layers.Input(shape=(28, 28)),
    layers.Flatten(),
    layers.Dense(300, activation='relu'),
    layers.BatchNormalization(),
    layers.Dropout(0.3),
    layers.Dense(100, activation='relu'),
    layers.BatchNormalization(),
    layers.Dropout(0.3),
    layers.Dense(10, activation='softmax')
])
model.summary()

model.compile(optimizer='adam',
              loss='sparse_categorical_crossentropy',
              metrics=['accuracy'])

# Configurar paragem antecipada
# (monitoriza a perda de validação, espera 5 épocas de margem)
early_stop = EarlyStopping(monitor='val_loss', patience=5, restore_best_weights=True)

# Treino com Batch Size ajustado e Early Stopping
print("\nA iniciar o treino otimizado...")
history = model.fit(x_train, y_train,
                    epochs=50,
                    batch_size=128, # Lotes maiores para estabilidade
                    validation_split=0.2,
                    callbacks=[early_stop],
                    verbose=1)

# Avaliação Global Corrigida
test_loss, test_acc = model.evaluate(x_test, y_test, verbose=2)
print(f'\nGlobal Accuracy Test: {test_acc*100:.2f}%\n')

# ---------------------
# Select 30 images from the test set
images = x_test[:30]
actual_labels = y_test[:30]

predictions = model.predict(images)
predicted_labels = np.argmax(predictions, axis=1)

print(f"Model Prediction: {predicted_labels}")
print(f"Actual Label: {actual_labels}")
#print(f"Correct: {sum(predicted_labels[i] == actual_labels[i] for i in range(len(predicted_labels))) / 28}")

total = len(predicted_labels)
corretas = sum(predicted_labels[i] == actual_labels[i] for i in range(total))
prec = corretas / total
print(f"Correct: {corretas} of {total}")
print(f"Precision: {prec*100:.2f}%")



# -------
# CNNs
# -------
fashion_mnist = tf.keras.datasets.fashion_mnist
(x_train, y_train), (x_test, y_test) = fashion_mnist.load_data()
class_names = ["T-shirt/top", "Trouser", "Pullover",
               "Dress", "Coat", "Sandal",
               "Shirt", "Sneaker", "Bag",
               "Ankle boot"]

# 1. Normalização
x_train, x_test = x_train / 255.0, x_test / 255.0

# 2. Ajuste de formato para CNNs (adicionar a dimensão da "cor", que é 1 para escala de cinzentos)
x_train = np.expand_dims(x_train, -1)
x_test = np.expand_dims(x_test, -1)

# 3. Nova Arquitetura: Rede Neuronal Convolucional (CNN)
model = models.Sequential([
    layers.Input(shape=(28, 28, 1)),

    # Primeira camada de convolução para detetar padrões básicos
    layers.Conv2D(32, (3, 3), activation='relu'),
    layers.MaxPooling2D((2, 2)), # Reduz o tamanho da imagem pela metade, mantendo os padrões

    # Segunda camada de convolução para padrões mais complexos
    layers.Conv2D(64, (3, 3), activation='relu'),
    layers.MaxPooling2D((2, 2)),

    # Agora sim, "achatamos" para tomar a decisão final
    layers.Flatten(),
    layers.Dense(128, activation='relu'),
    layers.Dropout(0.3),
    layers.Dense(10, activation='softmax')
])

model.summary()

model.compile(optimizer='adam',
              loss='sparse_categorical_crossentropy',
              metrics=['accuracy'])

# 4. Configurar a Paragem Antecipada
# patience=3: Se não melhorar durante 3 épocas seguidas, pára.
# restore_best_weights=True: Volta aos pesos da melhor época.
early_stop = EarlyStopping(monitor='val_loss', patience=3, restore_best_weights=True)

# Treino (podes meter 30 épocas, o early_stop vai pará-lo mais cedo se necessário)
print("\nA iniciar o treino...")
model.fit(x_train, y_train,
          epochs=30,
          validation_split=0.2,
          callbacks=[early_stop],
          verbose=1)

# Corrigido o erro da variável inexistente
test_loss, test_acc = model.evaluate(x_test, y_test, verbose=2)
print(f'\nAccuracy Test: {test_acc*100:.2f}%\n')

# ---------------------
# Amostra de 30 imagens do teste
images = x_test[:30]
actual_labels = y_test[:30]

predictions = model.predict(images)
predicted_labels = np.argmax(predictions, axis=1)

print(f"Model Prediction: {predicted_labels}")
print(f"Actual Label:     {actual_labels}")

total = len(predicted_labels)
corretas = sum(predicted_labels[i] == actual_labels[i] for i in range(total))
prec = corretas / total
print(f"Correct: {corretas} of {total}")
print(f"Precision: {prec*100:.2f}%")