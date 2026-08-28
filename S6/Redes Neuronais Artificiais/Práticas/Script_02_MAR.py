# --------------
# Sigma Function
# --------------
def sigma(x):
    if x >= 0:
        return 1
    else:
        return 0
    
# --------------
# Neuron OR
# --------------
def neuron_or(x1, x2):
    return sigma(x1 + x2 - 1.5)

# --------------
# Neuron AND
# --------------
def neuron_and(x1, x2):
    return sigma(x1 + x2 - 2)

# --------------
# Neuron XOR
# --------------
def neuron_xor(x1, x2):
    return neuron_or(
        neuron_and(neuron_not(x1), x2),
        neuron_and(x1, neuron_not(x2))
    )

# --------------
# Neuron NOT
# --------------
def neuron_not(x):
    return sigma(0.5 -x)