void insert_sort(int *arr, int length) {
   for (int i = 1; i < length; ++i) {
	  int key = arr[i];
	  int j = i - 1;
	  for (; j >= 0 && arr[j] > key; --j) {
		  arr[j + 1] = arr [j];
	  }
	  arr [j + 1] = key;
	}
}
