import time
import tracemalloc
import oqs


def run_mlkem_benchmark(iterations: int, algorithm_name: str) -> tuple:
    kem = oqs.KeyEncapsulation(algorithm_name)

    keygen_times = []
    for _ in range(iterations):
        start = time.perf_counter()
        public_key = kem.generate_keypair()
        end = time.perf_counter()
        keygen_times.append((end - start) * 1000)

    avg_keygen = sum(keygen_times) / iterations
    pub_key_size = len(public_key)

    encap_times = []
    decap_times = []

    # INÍCIO DO PROFILING DE MEMÓRIA
    tracemalloc.start()

    for _ in range(iterations):
        start_enc = time.perf_counter()
        ciphertext, shared_secret_sender = kem.encap_secret(public_key)
        end_enc = time.perf_counter()
        encap_times.append((end_enc - start_enc) * 1000)

        start_dec = time.perf_counter()
        shared_secret_receiver = kem.decap_secret(ciphertext)
        end_dec = time.perf_counter()
        decap_times.append((end_dec - start_dec) * 1000)

    # CAPTURAR E PARAR A MEMÓRIA
    current, peak = tracemalloc.get_traced_memory()
    tracemalloc.stop()
    peak_memory_kb = peak / 1024

    avg_enc = sum(encap_times) / iterations
    avg_dec = sum(decap_times) / iterations

    kem.free()

    return avg_keygen, avg_enc, avg_dec, pub_key_size, len(ciphertext), peak_memory_kb
