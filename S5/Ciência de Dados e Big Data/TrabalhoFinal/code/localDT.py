import time
import sys
import pandas as pd
from sklearn.tree import DecisionTreeClassifier
from pyspark.sql import SparkSession
import pyspark.sql.functions as sql_f

"""
Função para criar modelos locais em cada partição usando Scikit-Learn
"""
def build_model(data_partition):
    # Transformar o iterador da partição em DataFrame Pandas
    # column_names é uma variável global definida no main
    partition = pd.DataFrame(data_partition, columns=column_names)
    
    if partition.empty:
        return []

    # O target no teu dataset limpo chama-se 'Label'
    X_train = partition.drop(['Label', 'Dependentes', 'Emp_Imb'], axis=1)
    y_train = partition["Label"].astype("int")

    start = time.time()
    # Usar o max_depth passado pelo utilizador
    clf = DecisionTreeClassifier(random_state=0, max_depth=global_max_depth,
                                 class_weight="balanced")
    model = clf.fit(X_train.values, y_train.values)
    end = time.time()
    
    # Retorna o modelo e o tempo que demorou a treinar nesta partição específica
    return [(model, end - start)]

def main(argv):
    # Parâmetros: <cores> <percentagem_dados> <max_depth> <csv_output_file>
    cores = int(argv[0])
    percentage = int(argv[1])
    max_depth = int(argv[2])
    results_file = argv[3]

    # Definir variáveis globais para serem acessíveis dentro do mapPartitions
    global global_max_depth
    global_max_depth = max_depth

    # Iniciar Spark
    spark = (
        SparkSession.builder.master(f"local[{cores}]")
        .appName(f"LocalDT_{cores}cores_{percentage}pct")
        .config("spark.driver.memory", "16g")
        .config("spark.sql.logLevel", "ERROR")
        .getOrCreate()
    )

    df = (
        spark.read.format("csv")
        .option("header", "true")
        .option("inferSchema", "true")
        .load(f"data/dataset_limpo-{percentage}.csv")
    )

    # Reparticionar para que cada "core" trate de uma partição localmente
    data_rdd = df.repartition(cores).cache()
    data_rdd.count()
    
    global column_names
    column_names = df.columns

    # Medição da fase de construção
    start_total = time.time()
    
    # mapPartitions envia a função build_model para cada núcleo processar a sua parte
    models_runtimes = data_rdd.rdd.mapPartitions(build_model).collect()    
    end_total = time.time()

    models, runtimes = zip(*models_runtimes)
    total_duration = end_total - start_total
    avg_partition_time = sum(runtimes) / len(runtimes)

    print(f"Cores: {cores} | Dados: {percentage}% | Tempo Total: {total_duration:.4f}s")
    print(f"Tempo médio por partição (sklearn): {avg_partition_time:.4f}s")

    # Guardar resultados
    with open(results_file, "a") as f:
        # Formato: cores, percentagem, tempo_total, tempo_medio_particao
        f.write(f"{cores},{percentage},{total_duration},{avg_partition_time}\n")

    spark.stop()

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("python script.py <cores> <percentagem> [max_depth] [arquivo_saida]")
    else:
        main(sys.argv[1:])