#ifndef KMEANS_PARALELO_H
#define KMEANS_PARALELO_H

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <mpi.h>
#include <time.h>
#include <hdf5.h>
#include <jansson.h>
#include <string.h>

// Define the maximum number of dimensions and points
#define MAX_CLUSTERS 10
#define MAX_DIMS 100
#define MAX_POINTS 500000

// Structure to hold KMeans information
typedef struct {
    double *coords;
} Point;

// Structure to hold dataset information
typedef struct {
    Point *data_points;
    int num_points;
    int dimensions;
} Dataset;

void read_line(const char *line, double *point, int *dims);
void read_csv(const char *filename, Dataset *dataset);
void read_json(const char *filename, Dataset *dataset);
void read_hdf5(const char *filename, Dataset *dataset);
void write_output(const char *clusters_filename, const char *centroids_filename, Dataset *dataset, int *assignments, Point *centroids, int k);
void initialize_centroids(Point *centroids, Dataset *dataset, int k);
int assign_clusters(Point *data_points, Point *centroids, int *assignments, int num_points, int dimensions, int k);
void kmeans(Dataset *dataset, int k, int *assignments, Point *centroids);
double euclidean_distance(const Point *p1, const Point *p2, int dimensions);

#endif // KMEANS_PARALELO_H