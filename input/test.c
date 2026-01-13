struct Person {
    int age;
    float height;
};

struct Point {
    int x, y;
} p1 = { 25, 5.9 }, p2;

struct Person p, *ptr, arr[10];

struct Person p2 = { 25, 5.9 }, p3 = { .age = 25, .height = 5.9 };

struct Person p = {
    .age = 25,
    .height = 5.9
};

int x[10] = { 1, 2, 3 };