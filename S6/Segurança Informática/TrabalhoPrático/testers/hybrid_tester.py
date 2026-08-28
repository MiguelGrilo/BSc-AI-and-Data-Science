import time
import tracemalloc
import oqs
from cryptography.hazmat.primitives.asymmetric import rsa, padding
from cryptography.hazmat.primitives import hashes, serialization


def run_hybrid_benchmark(iterations: int, message: bytes) -> tuple:
    # Keygen Híbrido (Capado a 5 por causa do RSA)
    keygen_iterations = min(5, iterations)
    keygen_times = []
    for _ in range(keygen_iterations):
        start = time.perf_counter()
        # Gera ML-KEM e RSA
        kem_temp = oqs.KeyEncapsulation("ML-KEM-512")
        kem_temp.generate_keypair()
        rsa.generate_private_key(public_exponent=65537, key_size=2048)
        end = time.perf_counter()
        keygen_times.append((end - start) * 1000)
        kem_temp.free()

    avg_keygen = sum(keygen_times) / keygen_iterations

    # Setup das chaves persistentes para o teste
    kem = oqs.KeyEncapsulation("ML-KEM-512")
    pub_kem = kem.generate_keypair()

    priv_rsa = rsa.generate_private_key(public_exponent=65537, key_size=2048)
    pub_rsa = priv_rsa.public_key()

    pub_rsa_bytes = len(
        pub_rsa.public_bytes(
            encoding=serialization.Encoding.PEM,
            format=serialization.PublicFormat.SubjectPublicKeyInfo,
        )
    )

    total_pub_key_size = len(pub_kem) + pub_rsa_bytes

    encap_times = []
    decap_times = []

    tracemalloc.start()

    for _ in range(iterations):
        # TRANCAR (Híbrido)
        start_enc = time.perf_counter()
        cipher_kem, secret_kem = kem.encap_secret(pub_kem)
        cipher_rsa = pub_rsa.encrypt(
            message,  # Na vida real trancava-se o secret_kem, mas para benchmark de CPU medimos ambos os blocos
            padding.OAEP(
                mgf=padding.MGF1(algorithm=hashes.SHA256()),
                algorithm=hashes.SHA256(),
                label=None,
            ),
        )
        end_enc = time.perf_counter()
        encap_times.append((end_enc - start_enc) * 1000)

        tamanho_cifra_total = len(cipher_kem) + len(cipher_rsa)

        # DESTRANCAR (Híbrido)
        start_dec = time.perf_counter()
        secret_rec = kem.decap_secret(cipher_kem)
        msg_rec = priv_rsa.decrypt(
            cipher_rsa,
            padding.OAEP(
                mgf=padding.MGF1(algorithm=hashes.SHA256()),
                algorithm=hashes.SHA256(),
                label=None,
            ),
        )
        end_dec = time.perf_counter()
        decap_times.append((end_dec - start_dec) * 1000)

    current, peak = tracemalloc.get_traced_memory()
    tracemalloc.stop()
    peak_memory_kb = peak / 1024

    avg_enc = sum(encap_times) / iterations
    avg_dec = sum(decap_times) / iterations

    kem.free()

    return (
        avg_keygen,
        avg_enc,
        avg_dec,
        total_pub_key_size,
        tamanho_cifra_total,
        peak_memory_kb,
    )
