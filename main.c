int main() {
    int tab[127];

    for (int i = 0; i < 127; i++) {
        tab[i] = 127 - i;
    };

    for (int pass = 0; pass < 127; pass++) {
        for (int j = 0; j < 126; j++) {
            if (tab[j] > tab[j + 1]) {
                int tmp = tab[j];
                tab[j] = tab[j + 1];
                tab[j + 1] = tmp;
            };
        };
    };
    return 0;
}