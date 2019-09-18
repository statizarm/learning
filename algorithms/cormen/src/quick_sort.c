#define SWAP(a, b) {                                        \
    int tmp = a;                                                 \
    a = b;                                                 \
    b = tmp;                                                 \
}


static int partition(int *arr, int l, int r) {
    int i = l;
    int x = arr[r];
    for (int j = l; j < r; j++) {
        if (arr[j] < x) {
            SWAP(arr[j], arr[i]);
            i++;
        } 
    }
    SWAP(arr[i], arr[r]);
    return i;
}

static void _quick_sort(int *arr, int l, int r) {
    if(l < r) {
        int q = partition(arr, l, r);
        _quick_sort(arr, l, q - 1);
        _quick_sort(arr, q + 1, r);
    }
}

void quick_sort(int *arr, int length) {
    _quick_sort(arr, 0, length - 1);
}