#include <stdio.h>
#include <string.h>

int palindromo(char s[]) {
    int i = 0;
    int j = strlen(s) - 1;

    while (i < j) {
        if (s[i] != s[j]) {
            return 0;
        }
        i++;
        j--;
    }

    return 1;
}

int main() {
    char palavra[100];
    printf("Digite a palavra: ");
    scanf("%s", palavra);
    getchar();

    printf("%s é um palíndromo: %s\n", palavra, palindromo(palavra) ? "Sim" : "Não");
    
    return 0;
}