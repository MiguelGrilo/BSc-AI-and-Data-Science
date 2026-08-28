import time
import tracemalloc
from cryptography.hazmat.primitives.asymmetric import rsa, padding
from cryptography.hazmat.primitives import hashes, serialization


def run_rsa_benchmark(iterations: int, message: bytes, key_size: int) -> tuple:
    keygen_iterations = min(5, iterations)
    keygen_times = []

    for _ in range(keygen_iterations):
        start = time.perf_counter()
        pk = rsa.generate_private_key(public_exponent=65537, key_size=key_size)
        end = time.perf_counter()
        keygen_times.append((end - start) * 1000)

    avg_keygen = sum(keygen_times) / keygen_iterations

    private_key = rsa.generate_private_key(public_exponent=65537, key_size=key_size)
    public_key = private_key.public_key()
    pub_key_size = len(
        public_key.public_bytes(
            encoding=serialization.Encoding.PEM,
            format=serialization.PublicFormat.SubjectPublicKeyInfo,
        )
    )

    encrypt_times = []
    decrypt_times = []

    # INÍCIO DO PROFILING DE MEMÓRIA
    tracemalloc.start()

    for _ in range(iterations):
        start_enc = time.perf_counter()
        ciphertext = public_key.encrypt(
            message,
            padding.OAEP(
                mgf=padding.MGF1(algorithm=hashes.SHA256()),
                algorithm=hashes.SHA256(),
                label=None,
            ),
        )
        end_enc = time.perf_counter()
        encrypt_times.append((end_enc - start_enc) * 1000)

        start_dec = time.perf_counter()
        plaintext = private_key.decrypt(
            ciphertext,
            padding.OAEP(
                mgf=padding.MGF1(algorithm=hashes.SHA256()),
                algorithm=hashes.SHA256(),
                label=None,
            ),
        )
        end_dec = time.perf_counter()
        decrypt_times.append((end_dec - start_dec) * 1000)

    # CAPTURAR E PARAR A MEMÓRIA
    current, peak = tracemalloc.get_traced_memory()
    tracemalloc.stop()
    peak_memory_kb = peak / 1024  # Converter bytes para Kilobytes

    avg_enc = sum(encrypt_times) / iterations
    avg_dec = sum(decrypt_times) / iterations

    return avg_keygen, avg_enc, avg_dec, pub_key_size, len(ciphertext), peak_memory_kb
