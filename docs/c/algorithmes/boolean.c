//  First programs in C
//  Julien VALENTIN (2022)  julien.vlnt@gmail.com
//  Basic boolean and logical manipulations in C. The bool datatype is not a
//  built-in data type, but one may use them thanks to <stdbool.h> lib.
#include <stdbool.h>
#include <stdio.h>

int main()
{

    // Declaration of booleans
    bool a = true;     // true, or 1
    int  b = 1;        // true, or 1
    char c = 1;        // true, or 1
    
    bool d = false;    // false, or 0
    int  e = 0;        // false, or 0
    char f = 0;        // false, or 0 

    printf("\n");
    printf("Declaration\n");
    printf("-----------\n");
    printf("bool t = true;\n");
    printf("int  t = 1;\n");
    printf("char t = 1;\n");
    printf("bool f = false;\n");
    printf("int  f = 0;\n");
    printf("char f = 0;\n");

    printf("\n");
    printf("Size of these types ?\n");
    printf("---------------------\n");
    printf("bool : %llu byte\n" , sizeof(a));
    printf("char : %llu byte\n" , sizeof(c));
    printf("int  : %llu bytes\n", sizeof(b));
    
    printf("\n");
    printf("Identities\n");
    printf("----------\n");
    if(a == b) { printf("(bool) true  == (int)  1\n");  }
    if(a == c) { printf("(bool) true  == (char) 1\n");  }
    if(d == e) { printf("(bool) false == (int)  0\n");  }
    if(d == f) { printf("(bool) false == (char) 0\n");  }
    
    printf("\n");
    printf("Logical negation `!`\n");
    printf("--------------------\n");
    if( (!true) == false )  { printf("(!true)   == false\n");  }
    if( (!1) == 0 )         { printf("(!1)      == 0\n");      }
    if( (1-true) == false ) { printf("(1-true)  == false\n");  }
    if( (!false) == true )  { printf("(!false)  == true\n");   }
    if( (!0) == 1 )         { printf("(!0)      == 1\n");      }
    if( (1-false) == true ) { printf("(1-false) == true\n");   }

    printf("\n");
    printf("Bitwise negation `~`\n");
    printf("--------------------\n");
    printf("(int)  (~1) == %i\n", (~b));
    printf("(char) (~1) == %c\n", (~c));
    printf("(int)  (~0) == %i\n", (~e));
    printf("(char) (~0) == %c\n", (~f));

    printf("\n");
    printf("Logical Or `||`\n");
    printf("---------------\n");
    if( true || true )      { printf("(true || true)   == true\n");  }
    if( true || false )     { printf("(true || false)  == true\n");  }
    if( false || true )     { printf("(false || true)  == true\n");  }
    if( !(false || false) ) { printf("(false || false) == false\n"); }

    printf("\n");
    printf("Bitwise Or `|`\n");
    printf("--------------\n");
    if( 1 | 1 )    { printf("(1 | 1) == 1\n"); }
    if( 1 | 0 )    { printf("(1 | 0) == 1\n"); }
    if( 0 | 1 )    { printf("(0 | 1) == 1\n"); }
    if( !(0 | 0) ) { printf("(0 | 0) == 0\n"); }

    printf("\n");
    printf("Logical And `&&`\n");
    printf("----------------\n");
    if( true && true )      { printf("(true && true)   == true\n");  }
    if( !(true && false) )  { printf("(true && false)  == false\n"); }
    if( !(false && true) )  { printf("(false && true)  == false\n"); }
    if( !(false && false) ) { printf("(false && false) == false\n"); }

    printf("\n");
    printf("Bitwise And `&`\n");
    printf("---------------\n");
    if( 1 & 1 )    { printf("(1 & 1) == 1\n"); }
    if( !(1 & 0) ) { printf("(1 & 0) == 0\n"); }
    if( !(0 & 1) ) { printf("(0 & 1) == 0\n"); }
    if( !(0 & 0) ) { printf("(0 & 0) == 0\n"); }
    
    printf("\n");
    printf("Bitwise Exclusive Or `^`\n");
    printf("------------------------\n");
    if( !(1 ^ 1) ) { printf("(1 ^ 1) == 0\n"); }
    if( 1 ^ 0 )    { printf("(1 ^ 0) == 1\n"); }
    if( 0 ^ 1 )    { printf("(0 ^ 1) == 1\n"); }
    if( !(0 ^ 0) ) { printf("(0 ^ 0) == 0\n"); }

    return 0;

}