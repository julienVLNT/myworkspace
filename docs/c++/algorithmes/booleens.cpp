//  First programs in C++
//  Julien VALENTIN, July, 2022;  julien.vlnt@gmail.com
//  Basic binary and logical manipulations in C++.
#include <iostream>

int main()
{

    // Declaration of booleans
    bool a = true;     // true, or 1
    int  b = 1;        // true, or 1
    char c = 1;        // true, or 1
    
    bool d = false;    // false, or 0
    int  e = 0;        // false, or 0
    char f = 0;        // false, or 0

    std::cout                      << std::endl;
    std::cout << "Declaration"     << std::endl;
    std::cout << "-----------"     << std::endl;
    std::cout << "bool t = true;"  << std::endl;
    std::cout << "int  t = 1;"     << std::endl;
    std::cout << "char t = 1;"     << std::endl;
    std::cout << "bool f = false;" << std::endl;
    std::cout << "int  f = 0;"     << std::endl;
    std::cout << "char f = 0;"     << std::endl;

    std::cout                                       << std::endl;
    std::cout << "Size of these types ?"            << std::endl;
    std::cout << "---------------------"            << std::endl;
    std::cout << "bool : " << sizeof(a) << " byte"  << std::endl;
    std::cout << "char : " << sizeof(c) << " byte"  << std::endl;
    std::cout << "int  : " << sizeof(b) << " bytes" << std::endl;

    std::cout <<                 std::endl;
    std::cout << "Identities" << std::endl;
    std::cout << "----------" << std::endl;
    if( a == b ) { std::cout << "(bool) true  == (int)  1" << std::endl; }
    if( a == c ) { std::cout << "(bool) true  == (char) 1" << std::endl; }
    if( d == e ) { std::cout << "(bool) false == (int)  0" << std::endl; }
    if( d == f ) { std::cout << "(bool) false == (char) 0" << std::endl; }
    
    std::cout <<                           std::endl;
    std::cout << "Logical negation `!`" << std::endl;
    std::cout << "--------------------" << std::endl;
    if( (!true)   == false ) { std::cout << "(!true)   == false" << std::endl; }
    if( (!1)      == 0 )     { std::cout << "(!1)      == 0"     << std::endl; }
    if( (1-true)  == false ) { std::cout << "(1-true)  == false" << std::endl; }
    if( (!false)  == true )  { std::cout << "(!false)  == true"  << std::endl; }
    if( (!0)      == 1 )     { std::cout << "(!0)      == 1"     << std::endl; }
    if( (1-false) == true )  { std::cout << "(1-false) == true"  << std::endl; }

    std::cout <<                            std::endl;
    std::cout << "Bitwise negation `~`"  << std::endl;
    std::cout << "--------------------"  << std::endl;
    std::cout << "(int)  (~1) == " << ~b << std::endl;
    std::cout << "(char) (~1) == " << ~c << std::endl;
    std::cout << "(int)  (~0) == " << ~e << std::endl;
    std::cout << "(char) (~0) == " << ~f << std::endl;

    std::cout <<                      std::endl;
    std::cout << "Logical Or `||`" << std::endl;
    std::cout << "---------------" << std::endl;
    
    std::cout <<                     std::endl;
    std::cout << "Bitwise Or `|`" << std::endl;
    std::cout << "--------------" << std::endl;
    
    std::cout <<                       std::endl;
    std::cout << "Logical And `&&`" << std::endl;
    std::cout << "----------------" << std::endl;
    
    std::cout <<                      std::endl;
    std::cout << "Bitwise And `&`" << std::endl;
    std::cout << "---------------" << std::endl;
    
    std::cout <<                               std::endl;
    std::cout << "Bitwise Exclusive Or `^`" << std::endl;
    std::cout << "------------------------" << std::endl;

    return 0;

}
