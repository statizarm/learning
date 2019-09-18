void insert_sort(int *arr, int length) {
    for (int j = 1; j < length; j++) {
        int key = arr[j];
        int i = j - 1;
        for (; i >= 0 && arr[i] > key; i--) {
            arr[i + 1] = arr[i];
        }
        arr[i + 1] = key;
    }
}