# Reformulate previous class
from math import exp
import matplotlib.pyplot as plt
import numpy as np

def sigma(x): # Função de ativação
    return 1 / (1 + exp(-x))

def dsigma(x):
    return sigma(x) * (1 - sigma(x))

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
    print(f"\nFinal Weights ({name}): {w}\n\n")
    
    plt.figure(figsize=(5, 3))
    plt.plot(E)
    plt.title(f"Erro Quadrático {name}")
    plt.xlabel("Iterações")
    plt.ylabel("Erro")
    plt.grid(True)
    plt.show()

dataAND = [(0, 0, 0), (0, 1, 0), (1, 0, 0), (1, 1, 1)]
testCycle("AND", dataAND)

dataOR = [(0, 0, 0), (0, 1, 1), (1, 0, 1), (1, 1, 1)]
testCycle("OR", dataOR)

# NEW
def testCycleXOR(data):
    np.random.seed(0) 
    
    # Weights for 2 hidden layer neurons (bias, h1, h2) 
    wHiddenLayer = np.random.uniform(-1, 1, (2, 3)) 
    # Weights for 1 output neuron (bias, h1, h2)
    wOutput = np.random.uniform(-1, 1, 3)
    
    eta = 0.5
    iterations = 10000
    E_XOR = np.zeros(iterations)

    for i in range(iterations):
        for x1, x2, ystar in data:
            # Hidden Layers
            aHiddenLayer1 = wHiddenLayer[0,0] + wHiddenLayer[0,1]*x1 + wHiddenLayer[0,2]*x2
            h1 = sigma(aHiddenLayer1)
            aHiddenLayer2 = wHiddenLayer[1,0] + wHiddenLayer[1,1]*x1 + wHiddenLayer[1,2]*x2
            h2 = sigma(aHiddenLayer2)
            
            # Output Layer
            aOutput = wOutput[0] + wOutput[1]*h1 + wOutput[2]*h2
            y = sigma(aOutput)
            
            # Backpropagation Error
            deltaOutput = -(ystar - y) * dsigma(aOutput)
            
            # Backpropagation Hidden Layer Error
            deltaHiddenLayer1 = deltaOutput * wOutput[1] * dsigma(aHiddenLayer1)
            deltaHiddenLayer2 = deltaOutput * wOutput[2] * dsigma(aHiddenLayer2)

            wOutput += -eta * deltaOutput * np.array([1, h1, h2])
            wHiddenLayer[0] += -eta * deltaHiddenLayer1 * np.array([1, x1, x2])
            wHiddenLayer[1] += -eta * deltaHiddenLayer2 * np.array([1, x1, x2])
            
            E_XOR[i] += (ystar - y) ** 2

    print("Table (XOR):")
    for x1, x2, ystar in data:
        h1 = sigma(wHiddenLayer[0,0] + wHiddenLayer[0,1]*x1 + wHiddenLayer[0,2]*x2)
        h2 = sigma(wHiddenLayer[1,0] + wHiddenLayer[1,1]*x1 + wHiddenLayer[1,2]*x2)
        y = sigma(wOutput[0] + wOutput[1]*h1 + wOutput[2]*h2)
        print(f"x1: {x1}, x2: {x2} | Expected: {ystar} | Output: {y:.4f}")
    print("\nFinal Weights (XOR):")
    print("Hidden Layers Weights: [bias, w1, w2]")
    print("   Neuron HiddenLayer1:", wHiddenLayer[0])
    print("   Neuron HiddenLayer2:", wHiddenLayer[1])
    print("Output Layer Weights:")
    print("   Neuron Output:", wOutput)
    print("\n")

    plt.plot(E_XOR)
    plt.title("Erro Quadrático XOR (MLP)")
    plt.show()

dataXOR = [(0, 0, 0), (0, 1, 1), (1, 0, 1), (1, 1, 0)]
testCycleXOR(dataXOR)