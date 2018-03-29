#include <stdio.h>

int add(int i, int j)
{
	return (i + j);
}

int sub(int i, int j)
{
	return (i - j);
}

void print(int x, int y, int (*func)())
{
	printf("value is: %d\n", (*func)(x, y));
}

int main()
{
    int x=100, y=200;
    print(x,y,add);
    print(x,y,sub);

    return 0;
}
