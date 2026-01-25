import time
import sys
from pyspark.sql import SparkSession
import pyspark.sql.functions as F
from pyspark.ml.feature import VectorAssembler
from pyspark.ml.classification import DecisionTreeClassifier
"""
    Este programa permitirá executar novamente a mesma experiência com o dataset spam-base, modificando o número de núcleos (cores) e o tamanho dos dados utilizados como entrada. Como tal, muitas partes do código estão "fixas" para este conjunto de dados específico.

    Iremos executar apenas a fase de construção (building phase). 

    Argv 1: Número de Núcleos (Cores)/Partições
    Argv 2: Percentagem dos dados de treino a utilizar

"""
def main(argv):
    # Parâmetros: <cores> <percentagem_dados> <max_depth> <csv_output_file>
    cores = int(argv[0])
    percentage = int(argv[1])
    max_depth = int(argv[2])
    results_file = argv[3]

    # Iniciar Spark com configuração
    spark = (SparkSession.builder
             .master(f"local[{cores}]")
             .appName(f"TesteEscalabilidade_{cores}cores_{percentage}pct")
             .config("spark.driver.memory", "16g")
             .config("spark.sql.logLevel", "ERROR")
             .getOrCreate())

    df = (
        spark.read.format("csv")
        .option("header", "true")
        .option("inferSchema", "true")
        .load(f"data/dataset_limpo-{percentage}.csv")
    )
    
    import pyspark.sql.functions as F # Biblioteca de funções otimizadas
    # Contagem do número de instâncias por classe
    contagem = df.groupBy("Label").count().orderBy("Label").collect()

    classe_0 = contagem[0]["count"]
    classe_1 = contagem[1]["count"]

    # Calcular o desbalanceamento relativo (peso da classe minoritária)
    total = classe_0 + classe_1
    peso_0 = total / (2 * classe_0)
    peso_1 = total / (2 * classe_1)

    df = df.withColumn(
        "weight",
        F.when(F.col("Label") == 0, peso_0).otherwise(peso_1))

    # Reparticionar para garantir que o Spark usa os cores definidos
    df = df.repartition(cores).cache()
    
    # Forçar o Spark a processar o cache antes de iniciar a medição do tempo
    df.count() 
    
    target_col = "Label" 
    feature_cols = df.columns
    feature_cols.remove('Label')
    feature_cols.remove('weight')
    feature_cols.remove('Dependentes')
    feature_cols.remove('Emp_Imb')

    assembler = VectorAssembler(inputCols=feature_cols, outputCol="features")
    df_ml = assembler.transform(df).select("features", target_col, "weight")

    # Treino e Medição de Tempo
    dt = DecisionTreeClassifier(labelCol=target_col, featuresCol="features",
                                maxDepth=max_depth, weightCol="weight")

    start_time = time.time()
    model = dt.fit(df_ml)
    end_time = time.time()

    duration = end_time - start_time
    print(f"Cores: {cores} | Dados: {percentage}% | Tempo: {duration:.4f}s")

    # Guardar resultados
    with open(results_file, "a") as f:
        f.write(f"{cores},{percentage},{duration}\n")

    spark.stop()

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("python script.py <cores> <percentagem> [max_depth] [arquivo_saida]")
    else:
        main(sys.argv[1:])