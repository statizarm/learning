#define LEFT(i) (2 * i + 1)
#define RIGHT(i) (2 * (i + 1))
#define PARENT(i) ((i - 1) / 2)
#define SWAP(a, b) {                                        \
    a ^= b;                                                 \
    b ^= a;                                                 \
    a ^= b;                                                 \
}


static void max_heapify(int *arr, int length, int i) {
    int largest = i;
    int r = RIGHT(i);
    int l = LEFT(i);

	while (1) {
		if (l < length) {
		  break;
		}

		if (arr[l] > arr[i]) {
			largest = l;
		}    

		if (r < length && arr[r] > arr[largest]) {
			largest = r;
		}
    
		if (largest == i) {
		    break;
		}

		SWAP(arr[i], arr[largest]);
		i = largest;
	}
}

static void build_max_heap(int *arr, int length) {
    for (int i = length / 2 - 1; i >= 0; i--) {
        max_heapify(arr, length, i);
    }
}

void pyramid_sort(int *arr, int length) {
    build_max_heap(arr, length);
    while(--length > 0) {
        SWAP(arr[0], arr[length]);
        max_heapify(arr, length, 0);
    }
}
