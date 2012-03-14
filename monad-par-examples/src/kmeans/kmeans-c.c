#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

#define MAX_ITERATIONS 50

typedef struct {
    double x;
    double y;
} Vector;

typedef struct {
    int id;
    int count;
    Vector sum;
    Vector cent;
} Cluster;

inline double sqDistance(Vector v1, Vector v2) {
    return ((v1.x-v2.x)*(v1.x-v2.x)
        + ((v1.y-v2.y)*(v1.y-v2.y)));
}

int compareClusters(Cluster *a, Cluster *b, int nclusters) {
    int i;
    for(i = 0; i < nclusters; i++) {
        if(a[i].cent.x != b[i].cent.x ||
           a[i].cent.y != b[i].cent.y)
            return 0;
    }
    return 1;
}

Cluster combineClusters(Cluster c1, Cluster c2) {
    Cluster nc;
    nc.id = c1.id;
    nc.count = c1.count + c2.count;
    nc.sum.x = c1.sum.x + c2.sum.x;
    nc.sum.y = c1.sum.y + c2.sum.y;
    nc.cent.x = nc.sum.x / nc.count;
    nc.cent.y = nc.sum.y / nc.count;
    return nc;
}

inline void assign(Vector v, Cluster *newclusters, Cluster *clusters, int nclusters, int npoints) {
    int i;
    double nearestDistance;
    int nearestClusterIndex;
    for(i = 0; i < nclusters; i++) {
        double d = sqDistance(v, clusters[i].cent);
        if(i == 0 || d < nearestDistance) {
            nearestDistance = d;
            nearestClusterIndex = i;
        }
    }
    newclusters[nearestClusterIndex].count++;
    newclusters[nearestClusterIndex].sum.x += v.x;
    newclusters[nearestClusterIndex].sum.y += v.y;
}

void step(Cluster *newclusters, Cluster *clusters, Vector *points, int nclusters, int npoints) {
    int i;
    // initialize new clusters
    for(i = 0; i < nclusters; i++) {
        newclusters[i].id = clusters[i].id;
        newclusters[i].count = 0;
        newclusters[i].sum.x = 0;
        newclusters[i].sum.y = 0;
    }
    // assign each point to a vector
    for(i = 0; i < npoints; i++) {
        assign(points[i], newclusters, clusters, nclusters, npoints);
    }
    // calculate cluster centers
    for(i = 0; i < nclusters; i++) {
        newclusters[i].cent.x = newclusters[i].sum.x / newclusters[i].count;
        newclusters[i].cent.y = newclusters[i].sum.y / newclusters[i].count;
    }
}

main(int argc, char **argv) {

    char *ptfilename;
    if(argc < 2) {
        ptfilename = "kmeans-points.txt";
    }
    else
        ptfilename = argv[1];

    int bufsize = 700;
    int npoints = 0;
    Vector *pts = malloc(bufsize * sizeof(Vector));

    FILE *ptfile = fopen(ptfilename, "r");
    double a, b;
    while(fscanf(ptfile, "%lf,%lf\n", &a, &b) != EOF) {
        pts[npoints].x = a;
        pts[npoints].y = b;
        npoints++;
        if(npoints >= bufsize-2) {
            bufsize = bufsize * 2;
            pts = realloc(pts, bufsize * sizeof(Vector));
        }
    }
    printf("Read %d points\n", npoints);

    /**************** CLUSTERS ****************/
    // TODO: read these from a file
    int nclusters = 4;
    Cluster clusters[4];

    clusters[0].id = 0;
    clusters[0].count = 42942;
    clusters[0].sum.x = 193285.44500203873;
    clusters[0].sum.y = 146176.76645810533;
    clusters[0].cent.x = 4.50108157519535;
    clusters[0].cent.y = 3.4040511959877353;

    clusters[1].id = 1;
    clusters[1].count = 42612;
    clusters[1].sum.x = 190894.81278159164;
    clusters[1].sum.y = 143777.6442326363;
    clusters[1].cent.x = 4.479836965680833;
    clusters[1].cent.y = 3.374111617211966;

    clusters[2].id = 2;
    clusters[2].count = 43042;
    clusters[2].sum.x = 193028.18592524767;
    clusters[2].sum.y = 146669.04997076563;
    clusters[2].cent.x = 4.484647226551918;
    clusters[2].cent.y = 3.4075798050919017;

    clusters[3].id = 3;
    clusters[3].count = 43269;
    clusters[3].sum.x = 194076.864003674;
    clusters[3].sum.y = 145677.4150144259;
    clusters[3].cent.x = 4.485355889982991;
    clusters[3].cent.y = 3.3667848809638747;

    Cluster otherclusters[4];

    Cluster *curr = otherclusters;
    Cluster *prev = clusters;
    /**************** END CLUSTERS ****************/


    // start time
    struct timeval t0;
    gettimeofday(&t0, NULL);


    int iterations = 0;
    // kmeans_---
    // let loop
    while(!compareClusters(curr, prev, nclusters)) {
        
        step(curr, prev, pts, nclusters, npoints);
        Cluster *swap = prev;
        prev = curr;
        curr = swap;
        iterations++;
    }

    int i;
    for(i = 0; i < nclusters; i++) {
        printf("Cluster {clId = %d, clCount = %d, clSum = Vector %lf %lf, clCent = Vector %lf %lf\n",
            prev[i].id, prev[i].count, prev[i].sum.x, prev[i].sum.y, prev[i].cent.x, prev[i].cent.y);
    }

    // end time
    struct timeval t1;
    gettimeofday(&t1, NULL);
    printf("%d iterations\n", iterations);
    printf("SELFTIMED %lf\n", (t1.tv_sec - t0.tv_sec) + (double)(t1.tv_usec - t0.tv_usec) / 1000000);


    free(pts);
}
