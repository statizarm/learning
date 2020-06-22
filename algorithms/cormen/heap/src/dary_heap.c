#include <stdio.h>
#include <string.h>

int find_max_from_array (int *arr, int start, int end)
{
    int max = start;

    for (int i = max + 1; i < end; ++i)
	    if (arr[i] > arr[max])
			max = i;

	return max;
}

void swap (int *arr, int l, int r)
{
	int tmp = arr[l];
	arr[l] = arr[r];
	arr[r] = tmp;
}

void max_heapify (int *arr, int length, int d, int i)
{
	int largest;
	int range;
	int left = i * d + 1;

	while ((range = length - left) > 0) {
		
		if (range > d) {
			largest = find_max_from_array (arr, left, left + d);
		} else {
			largest = find_max_from_array (arr, left, left + range);
		}

		if (arr[largest] <= arr[i])
			break;

		swap (arr, i, largest);
		i = largest;
		left = i * d + 1;
	}
}

void print_arr (int *arr, int arr_length)
{
	int i;
	printf ("arr = [");
	for (i = 0; i < arr_length - 1; ++i) {
		printf ("%d, ", arr[i]);
	}

	printf ("%d]\n", arr[i]);
}

void build_max_heap (int *arr, int length, int d)
{
	for (int i = (length + 1) / d; i >= 0; --i) {
		max_heapify (arr, length, d, i);
	}
}

int check_max_heap_props (int *arr, int length, int d)
{
	int left;
	int largest;
	int range;

	for (int i = (length + 1) / d; i >= 0; --i) {
		left = i * d + 1;
		range = length - left;

		if (range > d) {
			range = d;
		}

		largest = find_max_from_array (arr, left, range);

		if (arr[largest] > arr[i]) {
		    return -1;
		}
	}

	return 0;
}

void test_with_msg (int *arr, int length, int d)
{
	build_max_heap (arr, length, d);

	if (check_max_heap_props (arr, length, d)) {
		printf ("Failed :(\n");
	} else {
		printf ("Success :)\n");
	}
}

int dialog ()
{
	static char msg[256];

	printf ("command: ");
	scanf ("%s", msg);

	if (!strcmp (msg, "q")) {
		return 0;
	} else if (!strcmp (msg, "p")) {
		return 2;
	} else if (!strcmp (msg, "ins")) {
		return 4;
	} else if (!strcmp (msg, "max")) {
		return 3;
	} else if (!strcmp (msg, "inc")) {
		return 5;
	}

	printf ("unknown command, please try again");
	return dialog ();
}

int extract_max (int *arr, int *length, int d)
{
	int max = arr[0];
	arr[0] = arr[--*length];
	max_heapify (arr, *length, d, 0);
	return max;
}

void insert (int *arr, int *length, int d, int elem)
{
	int prev = *length;
	int i = (*length - 1) / d;
	for (; prev > 0; prev = i, i = (i - 1) / d) {
		if (arr[i] < elem) {
			arr[prev] = arr[i];
		} else {
			break;
		}
	}

	arr[prev] = elem;
	*length += 1;
}

int increase_key (int *arr, int length, int d, int i, int key)
{
	if (arr[i] > key)
		return -1;

	int parent = (i - 1) / d;
	while (i > 0 && arr[parent] < key) {
		arr[i] = arr[parent];
		i = parent;
		parent = (i - 1) / d;
	}

	arr[i] = key;
	return 0;
}
			

int main (void)
{
	int arr[256] = {1, 2, 3, 4, 16, 23, 21, 51, 43, 69};
	int arr_length = 10;
	int state = 1;
	int d, elem, id, key;

	while (state != 0) {
		switch (state) {
			case 1:
				printf ("Enter number of childs in pyramid: ");
				scanf ("%d", &d);
				test_with_msg (arr, arr_length, d);
				break;
			case 2:
				print_arr (arr, arr_length);
				break;
			case 3:
				printf ("max: %d", extract_max (arr, &arr_length, d));
				break;
			case 4:
				scanf ("%d", &elem);
				insert (arr, &arr_length, d, elem);
				break;
			case 5:
				scanf ("%d %d", &id, &key);
				if (!increase_key (arr, arr_length, d, id, key)) {
					printf ("Success! :)\n");
				} else {
					printf ("Failed! :(\n");
				}
				break;
		}

		state = dialog ();
	}

	printf ("Bye!\n");

	return 0;
}
