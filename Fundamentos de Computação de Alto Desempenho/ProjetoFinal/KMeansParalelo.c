/*criar/alterar a pasta .vscode do projeto e adicionar/alterar 
o ficheiro c_cpp_properties.json :

"
{
    "configurations": [
      {
        "name": "Linux",
        "includePath": [
            "${workspaceFolder}/**",
            "/usr/include",
            "/usr/include/hdf5/serial",
            "/usr/lib/x86_64-linux-gnu",
            "/usr/lib/x86_64-linux-gnu/openmpi/include"
        ],
        "defines": [],
        "compilerPath": "/usr/bin/gcc",
        "cStandard": "c17",
        "cppStandard": "gnu++17",
        "intelliSenseMode": "linux-gcc-x64"
      }
    ],
    "version": 4
}
"*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include <mpi.h>
#include <hdf5.h>
#include <jansson.h>
#include "kmeans_paralelo.h"

void read_line(const char *line, double *point, int *dims) {    // Define the maximum number of dimensions and points
    char *token = NULL;
    int dim = 0;
    char temp_line[16384];
    strcpy(temp_line, line); // Copy the input line to a temporary buffer
    *dims = 0;
    char *rest = temp_line;
    while ((token = strtok_r(rest, ",", &rest))) {
        if (dim < MAX_DIMS) {
            point[dim] = atof(token); // Convert string to double
            dim++;
        } else {
            fprintf(stderr, "Error: Number of dimensions exceeds the maximum limit.\n");
            break;
        }
    }
    *dims = dim;
}

void read_csv(const char *filename, Dataset *dataset) {
    FILE *file = fopen(filename, "r");
    if (!file) {
        perror("Erro na abertura");
        exit(EXIT_FAILURE);
    }

    char line[16384];
    dataset->num_points = 0;
    dataset->dimensions = 0;
    dataset->data_points = NULL;

    if (fgets(line, sizeof(line), file) == NULL) {    // Skip the first line
        fprintf(stderr, "Error reading file or empty file\n");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    dataset->data_points = (Point *)malloc(MAX_POINTS * sizeof(Point));    // Allocate memory for data_points
    if (dataset->data_points == NULL) {
        fprintf(stderr, "Error allocating memory for data points\n");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    while (fgets(line, sizeof(line), file)) {
        if (dataset->num_points < MAX_POINTS) {
            dataset->data_points[dataset->num_points].coords = (double *)malloc(MAX_DIMS * sizeof(double));
            if (dataset->data_points[dataset->num_points].coords == NULL) {
                fprintf(stderr, "Error allocating memory for point coordinates\n");
                fclose(file);
                exit(EXIT_FAILURE);
            }
            int dims;
            read_line(line, dataset->data_points[dataset->num_points].coords, &dims);
            if (dataset->num_points == 0) {
                dataset->dimensions = dims;
            } else if (dataset->dimensions != dims) {
                fprintf(stderr, "Error: inconsistent dimensions in CSV file\n");
                fclose(file);
                exit(EXIT_FAILURE);
            }
            dataset->num_points++;
        } else {
            fprintf(stderr, "Error: Number of points exceeds the maximum limit.\n");
            fclose(file);
            exit(EXIT_FAILURE);
        }
    }
    fclose(file);
}

void read_json(const char *filename, Dataset *dataset) {
    json_error_t error;
    json_t *root = json_load_file(filename, 0, &error);
    if (!root) {
        fprintf(stderr, "Erro ao analisar JSON: %s\n", error.text);
        exit(EXIT_FAILURE);
    }
    dataset->num_points = 0;
    dataset->dimensions = 0;
    dataset->data_points = NULL;
    if (!json_is_array(root)) {
        fprintf(stderr, "Erro: o JSON raiz não é um array\n");
        json_decref(root);
        exit(EXIT_FAILURE);
    }
    size_t num_points = json_array_size(root);
    if (num_points > MAX_POINTS) {
        fprintf(stderr, "Erro: o número de pontos excede o limite máximo\n");
        json_decref(root);
        exit(EXIT_FAILURE);
    }
    dataset->data_points = (Point *)malloc(num_points * sizeof(Point));    // Allocate memory for data_points
    if (dataset->data_points == NULL) {
        fprintf(stderr, "Error allocating memory for data points\n");
        json_decref(root);
        exit(EXIT_FAILURE);
    }
    for (size_t i = 0; i < num_points; i++) {
        json_t *point = json_array_get(root, i);
        if (!json_is_object(point)) {
            fprintf(stderr, "Erro: ponto não é um objeto\n");
            json_decref(root);
            free(dataset->data_points);
            exit(EXIT_FAILURE);
        }
        const char *key;
        json_t *value;
        size_t num_dims = 0;
        dataset->data_points[i].coords = (double *)malloc(MAX_DIMS * sizeof(double));
        if (dataset->data_points[i].coords == NULL) {
            fprintf(stderr, "Error allocating memory for point coordinates\n");
            json_decref(root);
            for (size_t j = 0; j < i; j++) {
                free(dataset->data_points[j].coords);
            }
            free(dataset->data_points);
            exit(EXIT_FAILURE);
        }
        json_object_foreach(point, key, value) {
            if (json_is_number(value)) {
                if (num_dims < MAX_DIMS) {
                    dataset->data_points[i].coords[num_dims] = json_number_value(value);
                    num_dims++;
                } else {
                    fprintf(stderr, "Erro: número de dimensões excede o limite máximo\n");
                    json_decref(root);
                    for (size_t j = 0; j < i; j++) {
                        free(dataset->data_points[j].coords);
                    }
                    free(dataset->data_points[i].coords);
                    free(dataset->data_points);
                    exit(EXIT_FAILURE);
                }
            }
        }
        if (dataset->dimensions == 0) {
            dataset->dimensions = num_dims;
        } else if (dataset->dimensions != num_dims) {
            fprintf(stderr, "Erro: dimensões inconsistentes no arquivo JSON\n");
            json_decref(root);
            for (size_t j = 0; j <= i; j++) {
                free(dataset->data_points[j].coords);
            }
            free(dataset->data_points);
            exit(EXIT_FAILURE);
        }
        dataset->num_points++;
    }
    json_decref(root);
}

void read_hdf5(const char *filename, Dataset *dataset) {
    hid_t file_id, dataset_id, dataspace_id;
    herr_t status;
    file_id = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT);    // Open the HDF5 file
    if (file_id < 0) {
        fprintf(stderr, "Erro na abertura do ficheiro.\n");
        exit(EXIT_FAILURE);
    }
    dataset_id = H5Dopen(file_id, "dataset", H5P_DEFAULT);  // Open the dataset in the file
    if (dataset_id < 0) {
        fprintf(stderr, "Erro ao abrir o dataset HDF5\n");
        H5Fclose(file_id);
        exit(EXIT_FAILURE);
    }
    dataspace_id = H5Dget_space(dataset_id);    // Get the dataspace of the dataset
    if (dataspace_id < 0) {
        fprintf(stderr, "Erro ao obter o dataspace do dataset HDF5\n");
        H5Dclose(dataset_id);
        H5Fclose(file_id);
        exit(EXIT_FAILURE);
    }
    int rank = H5Sget_simple_extent_ndims(dataspace_id);
    if (rank != 2) {
        fprintf(stderr, "Erro: Rank do dataset não é 2\n");
        H5Sclose(dataspace_id);
        H5Dclose(dataset_id);
        H5Fclose(file_id);
        exit(EXIT_FAILURE);
    }
    hsize_t dims[2];
    status = H5Sget_simple_extent_dims(dataspace_id, dims, NULL);
    if (status < 0) {
        fprintf(stderr, "Erro ao obter as dimensões do dataspace do dataset HDF5\n");
        H5Sclose(dataspace_id);
        H5Dclose(dataset_id);
        H5Fclose(file_id);
        exit(EXIT_FAILURE);
    }
    if (dims[0] > MAX_POINTS || dims[1] > MAX_DIMS) {
        fprintf(stderr, "Error: Dataset dimensions exceed maximum limits\n");
        H5Sclose(dataspace_id);
        H5Dclose(dataset_id);
        H5Fclose(file_id);
        exit(EXIT_FAILURE);
    }
    dataset->num_points = dims[0];
    dataset->dimensions = dims[1];
    dataset->data_points = (Point *)malloc(dims[0] * sizeof(Point));    // Allocate memory for data_points
    if (dataset->data_points == NULL) {
        fprintf(stderr, "Error allocating memory for data points\n");
        H5Sclose(dataspace_id);
        H5Dclose(dataset_id);
        H5Fclose(file_id);
        exit(EXIT_FAILURE);
    }
    for (size_t i = 0; i < dims[0]; i++) {
        dataset->data_points[i].coords = (double *)malloc(dims[1] * sizeof(double));
        if (dataset->data_points[i].coords == NULL) {
            fprintf(stderr, "Error allocating memory for point coordinates\n");
            for (size_t j = 0; j < i; j++) {
                free(dataset->data_points[j].coords);
            }
            free(dataset->data_points);
            H5Sclose(dataspace_id);
            H5Dclose(dataset_id);
            H5Fclose(file_id);
            exit(EXIT_FAILURE);
        }
    }
    int *data = (int *)malloc(dims[0] * dims[1] * sizeof(int));
    if (data == NULL) {
        fprintf(stderr, "Error allocating memory for dataset\n");
        for (size_t i = 0; i < dims[0]; i++) {
            free(dataset->data_points[i].coords);
        }
        free(dataset->data_points);
        H5Sclose(dataspace_id);
        H5Dclose(dataset_id);
        H5Fclose(file_id);
        exit(EXIT_FAILURE);
    }
    status = H5Dread(dataset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
    if (status < 0) {
        fprintf(stderr, "Erro ao ler os dados do dataset HDF5\n");
        free(data);
        for (size_t i = 0; i < dims[0]; i++) {
            free(dataset->data_points[i].coords);
        }
        free(dataset->data_points);
        H5Sclose(dataspace_id);
        H5Dclose(dataset_id);
        H5Fclose(file_id);
        exit(EXIT_FAILURE);
    }
    for (size_t i = 0; i < dims[0]; i++) {    // Copy the data to the Dataset structure
        for (size_t j = 0; j < dims[1]; j++) {
            dataset->data_points[i].coords[j] = data[i * dims[1] + j];
        }
    }
    free(data);
    H5Sclose(dataspace_id);
    H5Dclose(dataset_id);
    H5Fclose(file_id);
}

void write_output(const char *clusters_filename, const char *centroids_filename, Dataset *dataset, int *assignments, Point *centroids, int k) {
    FILE *file = fopen(clusters_filename, "w");
    if (!file) {
        perror("Error opening clusters file");
        exit(EXIT_FAILURE);
    }
    for (int i = 0; i < dataset->num_points; i++) {
        fprintf(file, "%d:", assignments[i]);
        for (int j = 0; j < dataset->dimensions; j++) {
            fprintf(file, "%.0f", dataset->data_points[i].coords[j]);
            if (j < dataset->dimensions - 1) {
                fprintf(file, ",");
            }
        }
        fprintf(file, "\n");
    }
    fclose(file);
    file = fopen(centroids_filename, "w");
    if (!file) {
        perror("Error opening centroids file");
        exit(EXIT_FAILURE);
    }
    for (int i = 0; i < k; i++) {
        fprintf(file, "%d:", i);
        for (int j = 0; j < dataset->dimensions; j++) {
            fprintf(file, "%.0f", centroids[i].coords[j]);
            if (j < dataset->dimensions - 1) {
                fprintf(file, ",");
            }
        }
        fprintf(file, "\n");
    }
    fclose(file);
}

void initialize_centroids(Point *centroids, Dataset *dataset, int k) {
    for (int i = 0; i < k; i++) {
        centroids[i].coords = (double *)malloc(dataset->dimensions * sizeof(double));
        if (centroids[i].coords == NULL) {
            fprintf(stderr, "Error allocating memory for centroids\n");
            exit(EXIT_FAILURE);
        }
    }
    for (int i = 0; i < k; i++) {
        int random_index = rand() % dataset->num_points;
        for (int j = 0; j < dataset->dimensions; j++) {
            centroids[i].coords[j] = dataset->data_points[random_index].coords[j];
        }
    }
}

int assign_clusters(Point *data_points, Point *centroids, int *assignments, int num_points, int dimensions, int k) {
    int changes = 0;
    for (int i = 0; i < num_points; i++) {
        int cluster_index = 0;
        double min_dist = euclidean_distance(&data_points[i], &centroids[0], dimensions);
        for (int j = 1; j < k; j++) {
            double dist = euclidean_distance(&data_points[i], &centroids[j], dimensions);
            if (dist < min_dist) {
                min_dist = dist;
                cluster_index = j;
            }
        }
        if (assignments[i] != cluster_index) {
            assignments[i] = cluster_index;
            //changes++;  
            changes = 1;
        }
    }
    return changes;
}

void kmeans(Dataset *dataset, int k, int *assignments, Point *centroids) {
    initialize_centroids(centroids, dataset, k);
    int iterations = 0;
    int changes;
    int MPI_rank, MPI_size;
    MPI_Comm_rank(MPI_COMM_WORLD, &MPI_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &MPI_size);
    int local_num_points = dataset->num_points / MPI_size;
    int remainder = dataset->num_points % MPI_size;
    if (MPI_rank == MPI_size - 1) {
        local_num_points += remainder;
    }
    Point *local_data_points = dataset->data_points + MPI_rank * (dataset->num_points / MPI_size);
    Point *local_centroids = (Point *)malloc(k * sizeof(Point));
    for (int i = 0; i < k; i++) {
        local_centroids[i].coords = (double *)calloc(dataset->dimensions, sizeof(double));
    }
    int global_changes;
    do {
        changes = assign_clusters(local_data_points, centroids, assignments + MPI_rank * (dataset->num_points / MPI_size), local_num_points, dataset->dimensions, k);
        MPI_Allreduce(&changes, &global_changes, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
        for (int i = 0; i < k; i++) {
            for (int j = 0; j < dataset->dimensions; j++) {
                local_centroids[i].coords[j] = 0.0;
            }
        }
        int *local_cluster_sizes = (int *)calloc(k, sizeof(int));
        for (int i = 0; i < local_num_points; i++) {
            int cluster = assignments[MPI_rank * (dataset->num_points / MPI_size) + i];
            local_cluster_sizes[cluster]++;
            for (int j = 0; j < dataset->dimensions; j++) {
                local_centroids[cluster].coords[j] += local_data_points[i].coords[j];
            }
        }
        for (int i = 0; i < k; i++) {
            MPI_Allreduce(MPI_IN_PLACE, local_centroids[i].coords, dataset->dimensions, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);
        }
        int *global_cluster_sizes = (int *)calloc(k, sizeof(int));
        MPI_Allreduce(local_cluster_sizes, global_cluster_sizes, k, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
        for (int i = 0; i < k; i++) {
            if (global_cluster_sizes[i] > 0) {
                for (int j = 0; j < dataset->dimensions; j++) {
                    centroids[i].coords[j] = local_centroids[i].coords[j] / global_cluster_sizes[i];
                }
            }
        }
        free(local_cluster_sizes);
        free(global_cluster_sizes);
        iterations++;
    } while (global_changes > 0 && iterations < 100);
    for (int i = 0; i < k; i++) {
        free(local_centroids[i].coords);
    }
    free(local_centroids);
}

double euclidean_distance(const Point *p1, const Point *p2, int dimensions) {
    double sum = 0.0;
    for (int i = 0; i < dimensions; i++) {
        sum += pow((p1->coords[i] - p2->coords[i]), 2);
    }
    return sqrt(sum);
}

int main(int argc, char *argv[]) {
    if (argc < 6) {
        printf("Usage: %s <datafile> <number of clusters> <output clusters.csv> <output centroids.csv> <N>\n", argv[0]);
        return 1;
    }
    
    srand(time(NULL));
    MPI_Init(&argc, &argv);
    
    int MPI_rank, MPI_size;
    MPI_Comm_rank(MPI_COMM_WORLD, &MPI_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &MPI_size);
    
    int k = atoi(argv[2]);
    int N = atoi(argv[5]);
    
    Dataset dataset;
    const char *filename = argv[1];
    
    if (strstr(filename, ".csv")) {
        read_csv(filename, &dataset);
    } else if (strstr(filename, ".json")) {
        read_json(filename, &dataset);
    } else if (strstr(filename, ".h5")) {
        read_hdf5(filename, &dataset);
    } else {
        fprintf(stderr, "Unsupported file format\n");
        MPI_Finalize();
        return 1;
    }
    
    Point *centroids = (Point *)malloc(k * sizeof(Point));
    if (centroids == NULL) {
        fprintf(stderr, "Error allocating memory for centroids\n");
        MPI_Finalize();
        return 1;
    }
    for (int i = 0; i < k; i++) {
        centroids[i].coords = (double *)malloc(dataset.dimensions * sizeof(double));
        if (centroids[i].coords == NULL) {
            fprintf(stderr, "Error allocating memory for centroid coordinates\n");
            for (int j = 0; j <= i; j++) {
                free(centroids[j].coords);
            }
            free(centroids);
            MPI_Finalize();
            return 1;
        }
    }
    
    int *assignments = (int *)malloc(dataset.num_points * sizeof(int));
    if (assignments == NULL) {
        fprintf(stderr, "Error allocating memory for assignments\n");
        for (int i = 0; i < k; i++) {
            free(centroids[i].coords);
        }
        free(centroids);
        MPI_Finalize();
        return 1;
    }
    
    int local_num_points = dataset.num_points / MPI_size;
    int remainder = dataset.num_points % MPI_size;
    if (MPI_rank == MPI_size - 1) {
        local_num_points += remainder;
    }
    
    kmeans(&dataset, k, assignments, centroids);
    
    // Gather assignments
    int *global_assignments = NULL;
    if (MPI_rank == 0) {
        global_assignments = (int *)malloc(dataset.num_points * sizeof(int));
    }
    MPI_Gather(assignments + MPI_rank * (dataset.num_points / MPI_size), local_num_points, MPI_INT, global_assignments, local_num_points, MPI_INT, 0, MPI_COMM_WORLD);
    
    // Reduce centroids
    for (int i = 0; i < k; i++) {
        MPI_Reduce(MPI_rank == 0 ? MPI_IN_PLACE : centroids[i].coords, centroids[i].coords, dataset.dimensions, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
    }
    
    // Finalize centroids (only on rank 0)
    if (MPI_rank == 0) {
        for (int i = 0; i < k; i++) {
            for (int j = 0; j < dataset.dimensions; j++) {
                centroids[i].coords[j] /= MPI_size;
            }
        }
        // Write output files
        write_output(argv[3], argv[4], &dataset, global_assignments, centroids, k);
        free(global_assignments);
    }
    
    // Clean up
    for (int i = 0; i < dataset.num_points; i++) {
        free(dataset.data_points[i].coords);
    }
    free(dataset.data_points);
    
    for (int i = 0; i < k; i++) {
        free(centroids[i].coords);
    }
    free(centroids);
    free(assignments);
    
    MPI_Finalize();
    return 0;
}