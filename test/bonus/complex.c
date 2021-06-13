void fn(int vector[][4], int a, int b, int c) {
    int i, j;
    int max = 4;
    for (i=a; i<max; i=i+1) {
        for (j=b; j<max; j=j+1) {
            vector[i][j] = i+j+c;
        }
    }
    for (i=a; i<max; i=i+1) {
        for (j=b; j<max; j=j+1) {
            write(vector[i][j]);
            write("\n");
        }
    }

}
int MAIN() {
    int vector[4][4];
    fn(vector, 0, 1, 2);
}
