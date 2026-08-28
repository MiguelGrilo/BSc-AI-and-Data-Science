from math import exp
import matplotlib.pyplot as plt
import numpy as np

def sigma(x): # Função de ativação
    return 1 / (1 + exp(-x))

def dsigma(x):
    return sigma(x) * (1 - sigma(x))

dataAND = [(0, 0, 0), (0, 1, 0), (1, 0, 0), (1, 1, 1)]
dataOR = [(0, 0, 0), (0, 1, 1), (1, 0, 1), (1, 1, 1)]

# Starting Weights
np.random.seed(0)
w = 0.01 * np.random.randn(3)
eta = 0.5
iterations = 1000
E = np.zeros(iterations)

for i in range(iterations):
    for x1, x2, ystar in dataAND:
        a = w[0] + w[1] * x1 + w[2] * x2
        y = sigma(a)
        delta = -(ystar - y) * dsigma(a)
        w += -eta * delta * np.array([1, x1, x2])
        E[i] += (ystar - y) ** 2

print("Table (AND):")
for x1, x2, ystar in dataAND:
    a = w[0] + w[1] * x1 + w[2] * x2
    y = sigma(a)
    print(f"x1: {x1}, x2: {x2} | Expected: {ystar} | Output: {y:.4f}")

print(f"\nFinal Weights: {w}\n\n")

plt.plot(E)
plt.title("Evolução do Erro ao Longo das Iterações")
plt.xlabel("Iterações")
plt.ylabel("Erro Quadrático Total")
plt.grid()
plt.show()

for i in range(iterations):
    for x1, x2, ystar in dataOR:
        a = w[0] + w[1] * x1 + w[2] * x2
        y = sigma(a)
        delta = -(ystar - y) * dsigma(a)
        w += -eta * delta * np.array([1, x1, x2])
        E[i] += (ystar - y) ** 2

print("Table (OR):")
for x1, x2, ystar in dataOR:
    a = w[0] + w[1] * x1 + w[2] * x2
    y = sigma(a)
    print(f"x1: {x1}, x2: {x2} | Esperado: {ystar} | Output: {y:.4f}")

print(f"\nFinal Weights: {w}\n\n")

plt.plot(E)
plt.title("Evolução do Erro ao Longo das Iterações")
plt.xlabel("Iterações")
plt.ylabel("Erro Quadrático Total")
plt.grid()
plt.show()

def testCycle(name, data):
    np.random.seed(0) 
    w = 0.01 * np.random.randn(3)
    eta = 0.5
    iterations = 1000
    E = np.zeros(iterations)

    # Train Cycle
    for i in range(iterations):
        for x1, x2, ystar in data:
            a = w[0] + w[1] * x1 + w[2] * x2
            y = sigma(a)
            delta = -(ystar - y) * dsigma(a)
            w += -eta * delta * np.array([1, x1, x2])
            E[i] += (ystar - y) ** 2

    print(f"Table ({name}):")
    for x1, x2, ystar in data:
        a = w[0] + w[1] * x1 + w[2] * x2
        y = sigma(a)
        print(f"x1: {x1}, x2: {x2} | Expected: {ystar} | Output: {y:.4f}")
    print(f"\nFinal Weights: {w}\n\n")
    
    plt.figure(figsize=(5, 3))
    plt.plot(E)
    plt.title(f"Erro Quadrático - {name}")
    plt.xlabel("Iterações")
    plt.ylabel("Erro")
    plt.grid(True)
    plt.show()

dataAND = [(0, 0, 0), (0, 1, 0), (1, 0, 0), (1, 1, 1)]
testCycle("AND", dataAND)

dataOR = [(0, 0, 0), (0, 1, 1), (1, 0, 1), (1, 1, 1)]
testCycle("OR", dataOR)

dataXOR = [(0, 0, 0), (0, 1, 1), (1, 0, 1), (1, 1, 0)]
testCycle("XOR", dataXOR)