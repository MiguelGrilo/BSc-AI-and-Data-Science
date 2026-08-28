import csv
import os


def export_to_csv(file_path: str, data: list):
    os.makedirs(os.path.dirname(file_path), exist_ok=True)

    with open(file_path, mode="w", newline="") as file:
        writer = csv.writer(file)

        # Novo cabeçalho com Pico_Memoria_KB
        writer.writerow(
            [
                "Algoritmo",
                "Nivel_Seguranca",
                "Transacoes",
                "Tempo_Medio_KeyGen_MS",
                "Tempo_Medio_Trancar_MS",
                "Tempo_Medio_Destrancar_MS",
                "Tamanho_Chave_Pub_Bytes",
                "Tamanho_Cifra_Bytes",
                "Pico_Memoria_KB",
            ]
        )

        writer.writerows(data)