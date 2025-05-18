int main() {
    int x = 3;
    for (int i = 0; i < 10; i++) {
        if (x > 5){
            x = x + 3;
        };
        if (x > 0){
            x = x - 4;
        };
    };
    return 0;
}