import numpy as np

Px = np.array([0.25, 0.75])
Pxy = np.array([[0.1, 0.2], [0.5, 0.2]])

# ----------------------------------------
# Informação de Shannon (Surpresa)
# ----------------------------------------
def shannonInf(px):
    return np.prod(1/px)

print(f'Shannon Information: {shannonInf(Px)}')

# ----------------------------------------
# Entropia (Medida de Incerteza)
# ----------------------------------------
# Metodo 1 (Menos eficaz)
def entropy1(px):
    h = 0
    for ps in px:
        h = h + ps * np.log2(p)
    return -h

# Metodo 2
def entropy2(px):
    return 0 - np.sum(px * np.log2(px))

print(f'Hadamard Product: {np.array([1,2,3]) * np.array([10,20,30])}') # Produto Hadamard
print(f'Dot Product: {np.array([1,2,3]) @ np.array([10,20,30])}') # Produto Interno

# Metodo otimizado
def entropy(px):
    px = px[px > 0] # Filtra os valores do array
    return - px @ np.log2(px)
# Deste modo evita erros caso seja necessário calcular o logaritmo de 0

print(f'Entropy: {entropy(Px)}')

# ----------------------------------------
# Informação Mútua (Medida de Dependência)
# ----------------------------------------
def mutInformation(pxy):
    px = np.sum(pxy, axis=1)
    py = np.sum(pxy, axis=0)
    
    pxpy = np.outer(px, py) # Outer Product cria a seguinte tabela para auxiliar no cálculo final:
    # px[0]*py[0]  |  px[1]*py[0]
    # px[0]*py[1]  |  px[1]*py[1]
    
    return np.sum(pxy * np.log2(pxy / pxpy))

print(f'Mutual Information: {mutInformation(Pxy)}')

# ----------------------------------------
# Divergência de Kullback-Leibler
# ----------------------------------------
def KullbackLeiblerDiv(p, q):
    p = np.array(p)
    q = np.array(q)
    
    p = p[(p > 0) & (q > 0)]
    q = q[(p > 0) & (q > 0)]
    
    return np.sum(- p * np.log2(p / q))

p = np.array([0.1, 0.2])
q = np.array([0.3, 0.4])

print(f'Kullback Leibler Divergence: {KullbackLeiblerDiv(p,q)}')