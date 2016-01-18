#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
    int ChangeValue = atoi(argv[1]); //the value to change the color char by
    char *color = argv[2];
    int i=0; //loop counter.
    int index;
    char *ColorValues="0123456789abcdef";

    for (i=0;color[i] != 0;i++)
    {
        switch(color[i])
        {
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9': index=color[i] - '0'; break;
            case 'a':
            case 'A': index=10; break;
            case 'b':
            case 'B': index=11; break;
            case 'c':
            case 'C': index=12; break;
            case 'd':
            case 'D': index=13; break;
            case 'e':
            case 'E': index=14; break;
            case 'f':
            case 'F': index=15; break;
            default: index=-1; break;
        }

        if ( index != -1 )
        {
            index+=ChangeValue;

            while(index<0)
                index+=16;

            index=index%16;
            color[i]=ColorValues[index];
        }
    }
    printf("%s\n",color);
    return 0;
}
