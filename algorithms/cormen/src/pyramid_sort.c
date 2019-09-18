#define LEFT(i) (2 * i + 1)
#define RIGHT(i) (2 * (i + 1))
#define PARENT(i) ((i - 1) / 2)
#define SWAP(a, b) {                                        \
    a ^= b;                                                 \
    b ^= a;                                                 \
    a ^= b;                                                 \
}


static void max_heapify(int *arr, int length, int i) {
    int largest = 0;
    int r = RIGHT(i);
    int l = LEFT(i);

    if (l < length && arr[l] > arr[i]) {
        largest = l;
    } else {
        largest = i;
    }
    
    if (r < length && arr[r] > arr[largest]) {
        largest = r;
    }
    
    if (largest != i) {
        SWAP(arr[i], arr[largest]);
        max_heapify(arr, length, largest);
    }
}

static void build_max_heap(int *arr, int length) {
    for (int i = length / 2 - 1; i >= 0; i--) {
        max_heapify(arr, length, i);
    }
}

void pyramid_sort(int *arr, int length) {
    build_max_heap(arr, length);
    while(length-- > 1) {
        SWAP(arr[0], arr[length]);
        max_heapify(arr, length, 0);
    }
}
