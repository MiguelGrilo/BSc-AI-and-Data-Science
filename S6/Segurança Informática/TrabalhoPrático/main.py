from testers import run_rsa_benchmark, run_mlkem_benchmark, run_hybrid_benchmark
from utils import export_to_csv

def main():
    load_batches = [10, 50, 100]
    SECRET_MESSAGE = b"Super_Secret_AES_Symmetric_Key_256"
    results = []

    print("=== PQC Benchmark: Master Thesis Level ===\n")

    for transactions in load_batches:
        print(f"-> A processar bateria de {transactions} transações...")

        # 1. RSA
        avg_kg_rsa, avg_enc_rsa, avg_dec_rsa, pk_sz_rsa, cip_sz_rsa, mem_rsa = (
            run_rsa_benchmark(transactions, SECRET_MESSAGE, 2048)
        )
        results.append(
            [
                "RSA-2048",
                "Standard",
                transactions,
                avg_kg_rsa,
                avg_enc_rsa,
                avg_dec_rsa,
                pk_sz_rsa,
                cip_sz_rsa,
                mem_rsa,
            ]
        )

        # 2. ML-KEM
        avg_kg_ml, avg_enc_ml, avg_dec_ml, pk_sz_ml, cip_sz_ml, mem_ml = (
            run_mlkem_benchmark(transactions, "ML-KEM-512")
        )
        results.append(
            [
                "ML-KEM-512",
                "Standard",
                transactions,
                avg_kg_ml,
                avg_enc_ml,
                avg_dec_ml,
                pk_sz_ml,
                cip_sz_ml,
                mem_ml,
            ]
        )

        # 3. HÍBRIDO (Transição NIST)
        avg_kg_hy, avg_enc_hy, avg_dec_hy, pk_sz_hy, cip_sz_hy, mem_hy = (
            run_hybrid_benchmark(transactions, SECRET_MESSAGE)
        )
        results.append(
            [
                "Hibrido-RSA+MLKEM",
                "Transicao",
                transactions,
                avg_kg_hy,
                avg_enc_hy,
                avg_dec_hy,
                pk_sz_hy,
                cip_sz_hy,
                mem_hy,
            ]
        )

    output_path = "outputs/benchmark_results.csv"
    export_to_csv(output_path, results)

    print(f"\n[SUCESSO] Simulação concluída! Resultados guardados em: '{output_path}'")


if __name__ == "__main__":
    main()
