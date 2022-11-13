//  First programs in C++
//  Julien VALENTIN (June 2022)  julien.vlnt@gmail.com
//  About characters and strings. A great part of this code is presented in
//  the Microsoft Documentation :
//  https://docs.microsoft.com/en-us/cpp/cpp/string-and-character-literals-cpp?view=msvc-170!
#include <algorithm>
#include <cstring>
#include <iostream>
#include <string>

using namespace std;
using namespace std::string_literals;

int main()
{
    /*
        C-style character literals.
    */
    // Three character types
    char c0 = 'A';
    signed char c1 = 'A';
    unsigned char c2 = 'A';

    // A word of four characters
    char word[4] = {'a', 'b', 'c', 'd'};    
    for(int i=0; i < sizeof(word)/sizeof(char); i++){ cout << word[i]; }; cout << endl;

    // Character literals declaration with `auto` keyword
    auto c3 =   'A';    // char
    // auto c4 = u8'A';    // char
    auto c5 =  L'A';    // wchar_t
    auto c6 =  u'A';    // char16_t
    auto c7 =  U'A';    // char32_t
    
    /*
        The list of special escape characters
    */
    auto newline   = '\n';    // newline
    auto backslash = '\\';    // backslash
    auto horiztab  = '\t';    // horizontal tab
    auto questmark = '\?';    // question mark
    auto vertab    = '\v';    // vertical tab
    auto squote    = '\'';    // single quote
    auto backspace = '\b';    // backspace
    auto dquote    = '\"';    // double quote
    auto carriage  = '\r';    // carriage return
    auto nullchar  = '\0';    // null character
    auto ffill     = '\f';    // form fill
    auto alert     = '\a';    // alert ?

    /*
        Some literal strings.
    */
   // String literals
    auto s0 =   "hello"; // const char*
    auto s1 = u8"hello"; // const char* before C++20, encoded as UTF-8,
                         // const char8_t* in C++20
    auto s2 =  L"hello"; // const wchar_t*
    auto s3 =  u"hello"; // const char16_t*, encoded as UTF-16
    auto s4 =  U"hello"; // const char32_t*, encoded as UTF-32

    // Raw string literals containing unescaped \ and "
    auto R0 =   R"("Hello \ world")"; // const char*
    auto R1 = u8R"("Hello \ world")"; // const char* before C++20, encoded as UTF-8,
                                      // const char8_t* in C++20
    auto R2 =  LR"("Hello \ world")"; // const wchar_t*
    auto R3 =  uR"("Hello \ world")"; // const char16_t*, encoded as UTF-16
    auto R4 =  UR"("Hello \ world")"; // const char32_t*, encoded as UTF-32

    // Combining string literals with standard s-suffix
    auto S0 =   "hello"s; // std::string
    auto S1 = u8"hello"s; // std::string before C++20, std::u8string in C++20
    auto S2 =  L"hello"s; // std::wstring
    auto S3 =  u"hello"s; // std::u16string
    auto S4 =  U"hello"s; // std::u32string

    // Combining raw string literals with standard s-suffix
    auto S5 =   R"("Hello \ world")"s; // std::string from a raw const char*
    auto S6 = u8R"("Hello \ world")"s; // std::string from a raw const char* before C++20, encoded as UTF-8,
                                       // std::u8string in C++20
    auto S7 =  LR"("Hello \ world")"s; // std::wstring from a raw const wchar_t*
    auto S8 =  uR"("Hello \ world")"s; // std::u16string from a raw const char16_t*, encoded as UTF-16
    auto S9 =  UR"("Hello \ world")"s; // std::u32string from a raw const char32_t*, encoded as UTF-32

    /*
        The type `string`
    */
    string messageA("hello");
    string messageB = "hello";

    /*
        Sample of tools and algorithms useful when dealing with strings.
    */
    // Length of a string
    string messageC = "ABC";
    cout << messageC.length() << endl;

    // Concatenation
    string message0 = "Hello, ";
    string message1 = "World !";
    string message2 = message0 + message1;
    string message3 = message0.append(message1);
    cout << message0 << endl;

    // And many other methods... For instance, look at 
    // https://www.scaler.com/topics/cpp/strings-in-cpp/

    return 0;
}
