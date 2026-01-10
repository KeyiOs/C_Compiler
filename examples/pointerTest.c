// Test pointer support

int main() {
    int x = 10;
    int *ptr;
    int **pptr;
    
    ptr = &x;
    
    int y = *ptr;
    
    return 0;
}

int* createPointer() {
    int *p;
    return p;
}

void processPointer(int *ptr) {
    int value = *ptr;
}
