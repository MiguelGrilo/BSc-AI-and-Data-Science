# def f(x, y, z=[]):
#     r = x + y + sum(z)
#     z += [r]
#     return x + y + z
#
#   Summary of function g:
#   *y is used to collect an arbitrary number of extra positional arguments into a tuple.
#   **z is used to collect an arbitrary number of extra keyword arguments into a dictionary.
# def g(x, *y, **z):
#     # Note: 'x + y + z' raises a TypeError. To mathematically sum the inputs instead:
#     return x + sum(y) + sum(z.values())

class Veiculo:
    def __init__(self, x, y, marca="branca"):
        self.x = x
        self.y = y
        self.marca = marca

    def move(self, dx, dy):
        self.x += dx
        self.y += dy

    def __str__(self):
        return f'Marca "{self.marca}" na posição ({self.x}, {self.y})'

class Aviao(Veiculo):
    def __init__(self, x, y, z, marca):
        super().__init__(x, y, marca)
        self.z = z
    def __str__(self):
        return f'Marca "{self.marca}" na posição ({self.x}, {self.y}, {self.z})'

carro = Veiculo(0, 0, "Fiat")
print(carro)

aviao = Aviao(10, 20, 1000, "Boeing")
print(aviao)
