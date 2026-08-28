# Problemas:
# Para cada uma das seguintes funções:
#
# f(x) = (x - 1.4)^2 - 2
#
# f(x) = h(x) - h(x + 2)
# h(x) = 1 / (1 + exp^(-x))
#
# f(x_1, x_2) = x_1^2 + 3 x_2^2 + 2 x_1 x_2 + 1
#
# 1. Desenhar o gráfico das funções usando o `matplotlib`.
# 2. Minimizar usando o metodo do gradiente (implementar metodo do gradiente).
# 3. Desenhar gráfico da norma do gradiente em cada iteração.

import matplotlib.pyplot as plt
import numpy as np

def f1(x):
    return (x - 1.4) ** 2 - 2

def h(x):
    return 1 / (1 + np.exp(-x))

def f2(x):
    return h(x) - h(x + 2)

def f3(x1, x2):
    return x1 ** 2 + 3 * x2 ** 2 + 2 * x1 * x2 + 1


# EX 1 - Plotting the functions

# Plot f1
x = np.linspace(-1, 4, 100)
y = f1(x)
plt.figure(figsize=(8, 4))
plt.plot(x, y)
plt.title('f1(x) = (x - 1.4)^2 - 2')
plt.xlabel('x')
plt.ylabel('f1(x)')

# Plot f2
x = np.linspace(-10, 10, 100)
y = f2(x)
plt.figure(figsize=(8, 4))
plt.plot(x, y)
plt.title('f2(x) = h(x) + h(x + 2)')
plt.xlabel('x')
plt.ylabel('f2(x)')

# Plot 3d f3
x1 = np.linspace(-10, 10, 100)
x2 = np.linspace(-10, 10, 100)
X1, X2 = np.meshgrid(x1, x2)
Y = f3(X1, X2)
fig = plt.figure(figsize=(8, 4))
ax = fig.add_subplot(111, projection='3d')
ax.plot_surface(X1, X2, Y, cmap='viridis')
ax.set_title('f3(x1, x2) = x1^2 + 3x2^2 + 2x1x2 + 1')
ax.set_xlabel('x1')
ax.set_ylabel('x2')
ax.set_zlabel('f3(x1, x2)')
plt.show()


# EX 2 - Gradient Descent Implementation

# Define gradient method as a function
def gradient_method(grad, x0, lr=0.01, max_iter=100):
    grad_norms = []
    x = x0
    for i in range(max_iter):
        g = grad(x)
        grad_norms.append(np.linalg.norm(g))
        x = x - lr * g
    return x, grad_norms


# EX 3 - Applying Gradient Descent and Plotting Norms

# Gradient method for f1, plotting the norm of the gradients at each iteraction
def grad_f1(x):
    return 2 * (x - 1.4)

x0 = 5
x_opt, grad_norms = gradient_method(grad_f1, x0)
# Plot the gradient norms as a scatter plot
plt.figure(figsize=(8, 4))
plt.scatter(range(len(grad_norms)), grad_norms)
plt.title('Gradient norms for f1')
plt.xlabel('Iteration')
plt.ylabel('Gradient norm')
plt.show()

# Gradient method for f2, plotting the norm of the gradients at each iteraction
def grad_f2(x):
    return h(x) * (1 - h(x)) - h(x + 2) * (1 - h(x + 2))

x0 = 5
x_opt, grad_norms = gradient_method(grad_f2, x0)
# Plot the gradient norms as a scatter plot
plt.figure(figsize=(8, 4))
plt.scatter(range(len(grad_norms)), grad_norms)
plt.title('Gradient norms for f2')
plt.xlabel('Iteration')
plt.ylabel('Gradient norm')
plt.show()


# Gradient method for f3, plotting the norm of the gradients at each iteraction
def grad_f3(x1, x2):
    df_dx1 = 2 * x1 + 2 * x2
    df_dx2 = 6 * x2 + 2 * x1
    return np.array([df_dx1, df_dx2])

x0 = np.array([5, 5])
x_opt, grad_norms = gradient_method(lambda x: grad_f3(x[0], x[1]), x0)
# Plot the gradient norms as a scatter plot
plt.figure(figsize=(8, 4))
plt.scatter(range(len(grad_norms)), grad_norms)
plt.title('Gradient norms for f3')
plt.xlabel('Iteration')
plt.ylabel('Gradient norm')
plt.show()